(ns logger4clj.formatters
  (:use [logger4clj.logger])
  (:import  [java.text SimpleDateFormat]
            [java.util.regex Pattern]
            [java.io ByteArrayOutputStream PrintStream]))

(def LINE_SEP (System/getProperty "line.separator"))

(defn create-stacktrace-string
  [ex]
  (let [out (ByteArrayOutputStream.)]
    (.printStackTrace ex (PrintStream. out))
    (.toString out)))

(def LINE_FORMATTER_TAG "\\$\\{(\\w+)(:(.*?))?\\}")
(def LINE_FORMATTER_TAG_PATTERN (Pattern/compile LINE_FORMATTER_TAG))

(defn- make-formatter-func
  [type param]
  (condp = (.toLowerCase type)
    "ts" (let [fmt (SimpleDateFormat. (if param 
                                        param 
                                        "yyyy-MM-dd HH:mm:ss.SSSZ"))]
           (fn [t l c m e fn ln] (.format fmt t)))
    "lvl" (fn [t l c m e fn ln] (first (get valid-log-levels l)))
    "cat" (fn [t l c m e fn ln] c)
    "msg" (fn [t l c m e fn ln] m)
    "ex" (fn [t l c m e fn ln] (if-not (nil? e) 
                           (if (= param "msg")
                             (.getLocalizedMessage e)
                             (str LINE_SEP (create-stacktrace-string e)))
                           ""))
    "fn" (fn [t l c m e fn ln] fn)
    "ln" (fn [t l c m e fn ln] ln)
    "n" (fn [t l c m e fn ln] LINE_SEP)
    (throw (IllegalArgumentException. 
             (format "Unknown formatting field [%s]" type)))))

(defn- parse-line-formatter-string
  "Returns a vector containing:
       a new string using (format) replacement variables
       a vector of functions (ts log-lvl cat msg ex) -> string, one for each
           replacement variable
"
  [input-str]
  (let [matcher (.matcher LINE_FORMATTER_TAG_PATTERN input-str)]
    (loop [new-str input-str fn-vec []]
      (if-not (.find matcher)
        [new-str fn-vec]
        (let [type (.group matcher 1)
              param (.group matcher 3)
              func (make-formatter-func type param)]
          (recur (.replaceFirst new-str LINE_FORMATTER_TAG "%s") (conj fn-vec func)))))))

(defn create-line-formatter
  "Creates a formatter using the given specification.

E.g. (stacktraces are truncated in this example):  
[${ts:yyyy-MM-dd HH:mm:ss.SSSZ}][${lvl}] ${msg} (${fn}:${ln}) ${ex}${n} ==>

[2012-12-09 21:19:20.841-0800][ERROR] An error was encountered! (sample_logger.clj:35) 
java.lang.IllegalStateException: Divide by zero!
        at sample_logger$eval4086.invoke(sample_logger.clj:33)
        at clojure.lang.Compiler.eval(Compiler.java:6511)
Caused by: java.lang.ArithmeticException: Divide by zero
        at clojure.lang.Numbers.divide(Numbers.java:156)
        at clojure.lang.Numbers.divide(Numbers.java:3691)
        ... 25 more

Supported fields:
    ${ts}  - timestamp with optional timestamp format specification using 
             java.text.SimpleDateFormat formatting. The date format is
             placed after a colon with the curly-braces as shown in the example
             above. The default SimpleDateFormat is yyyy-MM-dd HH:mm:ss.SSSZ. 
    ${lvl} - log level, e.g. DEBUG, ERROR, etc
    ${cat} - category (name of the source logger), usually used to filter log
             messages
    ${msg} - the log message
    ${ex}  - the exception stacktrace (if any). This will always be preceeded 
             by a newline. This may alternatively be specified as ${ex:stack},
             or to print the exception message only, ${ex:msg}.
    ${fn}  - file name
    ${ln}  - line number within the file designated in ${fn}
    ${n}   - platform line ending

Remember to put a newline at the end if you want each message on a separate
line!.
"
  [format-str]
  (let [[line-template replacement-funcs] (parse-line-formatter-string format-str)]
    (fn [{:keys [time-ms log-lvl category msg exception file-name line-number]}]
      (String/format line-template 
                     (object-array 
                       (for [func replacement-funcs] 
                         (func time-ms log-lvl category msg exception file-name line-number)))))))

(defn- get-stacktrace-element-info
  [stacktrace-element]
  [(.getClassName stacktrace-element)
   (.getFileName stacktrace-element)
   (.getLineNumber stacktrace-element)
   (.getMethodName stacktrace-element)
   ])

(defn- create-stacktrace-tree
  [ex]
  [(.getName (class ex))
   (.getLocalizedMessage ex) 
   (map get-stacktrace-element-info (seq (.getStackTrace ex)))
   (if (nil? (.getCause ex))
     nil
     (create-stacktrace-tree (.getCause ex)))])

(defn- format-stacktrace-elements
  [elements initial-spacing]
  (apply str
         (for [e elements]
           (str initial-spacing 
                "<element "
                "className=\"" (get e 0) "\" "
                "fileName=\"" (get e 1) "\" "
                "lineNumber=\"" (get e 2) "\" "
                "methodName=\"" (get e 3) "\"/>" LINE_SEP))))

(defn- format-causes
  [cause]
  (str
    "        <cause>" LINE_SEP
    "            <class>" (first cause) "</class>" LINE_SEP
    "            <message>" (second cause) "</message>" LINE_SEP
    "            <stacktrace>" LINE_SEP
    (format-stacktrace-elements (get cause 2) "                ")
    "            </stacktrace>" LINE_SEP
    "        </cause>" LINE_SEP
    (if (nil? (get cause 3)) "" (format-causes (get cause 3)))))

(defn- format-xml-exception
  [st indent]
  (str 
    indent "        <class>" (first st) "</class>" LINE_SEP
    indent "        <message>" (second st) "</message>" LINE_SEP
    indent "        <stacktrace>" LINE_SEP
    (format-stacktrace-elements 
             (get st 2) 
             (str indent "            "))
    indent "        </stacktrace>" LINE_SEP
    
    (if (get st 3) 
      (str
        indent "        <cause>" LINE_SEP
        (format-xml-exception 
          (get st 3) 
          (str indent "    "))
        indent "        </cause>" LINE_SEP) 
      "")))

(defn create-xml-formatter
  "Creates a new formatter with output in XML format. The XML is not a closed document, 
but a series of <log-message> tags with no namespace. An optional parameter 
:timestamp-format accepts a string in java.text.SimpleDateFormat format. The 
default value is yyyy-MM-dd HH:mm:ss.SSSZ. 

E.g. (stacktraces are truncated in this example):  

<log-message>
    <timestamp>2012-12-09 21:25:32.229-0800</timestamp>
    <log-level>ERROR</log-level>
    <category>my-log</category>
    <file-name>sample_logger.clj</file-name>
    <line-number>35</line-number>
    <message>An error was encountered!</message>
    <exception>
            <class>java.lang.IllegalStateException</class>
            <message>Divide by zero!</message>
            <stacktrace>
                <element className=\"sample_logger$eval4168\" fileName=\"sample_logger.clj\" lineNumber=\"33\" methodName=\"invoke\"/>
                <element className=\"clojure.lang.Compiler\" fileName=\"Compiler.java\" lineNumber=\"6511\" methodName=\"eval\"/>
            </stacktrace>
            <cause>
                <class>java.lang.ArithmeticException</class>
                <message>Divide by zero</message>
                <stacktrace>
                    <element className=\"clojure.lang.Numbers\" fileName=\"Numbers.java\" lineNumber=\"156\" methodName=\"divide\"/>
                    <element className=\"clojure.lang.Numbers\" fileName=\"Numbers.java\" lineNumber=\"3691\" methodName=\"divide\"/>
                </stacktrace>
            </cause>
    </exception>
</log-message>

"  
  [& {timestamp-format :timestamp-format 
      :or {timestamp-format "yyyy-MM-dd HH:mm:ss.SSSZ"}}]
  (let [ts-format (SimpleDateFormat. timestamp-format)]
    (fn [{:keys [time-ms log-lvl category msg exception file-name line-number]}]
      (str 
        "<log-message>" LINE_SEP
        "    <timestamp>" (.format ts-format time-ms) "</timestamp>" LINE_SEP
        "    <log-level>" (first (get valid-log-levels log-lvl)) "</log-level>" LINE_SEP
        "    <category>" category "</category>" LINE_SEP
        "    <file-name>" file-name "</file-name>" LINE_SEP
        "    <line-number>" line-number "</line-number>" LINE_SEP
        "    <message>" msg "</message>" LINE_SEP
        (if exception 
          (str 
            "    <exception>" LINE_SEP
            (format-xml-exception (create-stacktrace-tree exception) "    ")
            "    </exception>" LINE_SEP) 
          "")
        "</log-message>" LINE_SEP))))

(defn- format-yaml-stacktrace-elements
  [elements initial-spacing]
  (apply str
         (for [e elements]
           (format 
             (str 
               initial-spacing 
               "- {className: %s, fileName: %s, lineNumber: %s, methodName: %s}"
               LINE_SEP),
             (get e 0) (get e 1) (get e 2) (get e 3)))))

(defn- format-yaml-exception
  [st indent]
  (str
    indent "exception:" LINE_SEP
    indent "    class:      " (first st) LINE_SEP
    indent "    message:    " (second st) LINE_SEP
    indent "    stacktrace: " LINE_SEP
    (format-yaml-stacktrace-elements 
      (get st 2) 
      (str indent "        "))
    (if (get st 3) 
      (str indent "    cause:" LINE_SEP
           (format-yaml-exception (get st 3) (str indent "        "))) 
      "")))

(defn create-yaml-formatter
  "Creates a new formatter with output in YAML format (http://www.yaml.org). An
optional parameter :timestamp-format accepts a string in java.text.SimpleDateFormat
format. The default value is yyyy-MM-dd HH:mm:ss.SSSZ. 

E.g. (stacktraces are truncated in this example):  
---
timestamp:    2012-12-09 21:24:48.958-0800
log-level:    ERROR
category:     my-log
file-name:    sample_logger.clj
line-number:  35
message:      An error was encountered!
    exception:
        class:      java.lang.IllegalStateException
        message:    Divide by zero!
        stacktrace: 
            - {className: sample_logger$eval4128, fileName: sample_logger.clj, lineNumber: 33, methodName: invoke}
            - {className: clojure.lang.Compiler, fileName: Compiler.java, lineNumber: 6511, methodName: eval}
        cause:
            exception:
                class:      java.lang.ArithmeticException
                message:    Divide by zero
                stacktrace: 
                    - {className: clojure.lang.Numbers, fileName: Numbers.java, lineNumber: 156, methodName: divide}
                    - {className: clojure.lang.Numbers, fileName: Numbers.java, lineNumber: 3691, methodName: divide}

"    
  [& {timestamp-format :timestamp-format 
      :or {timestamp-format "yyyy-MM-dd HH:mm:ss.SSSZ"}}]
  (let [ts-format (SimpleDateFormat. timestamp-format)]
    (fn [{:keys [time-ms log-lvl category msg exception file-name line-number]}]
      (str
        "---" LINE_SEP
        "timestamp:    " (.format ts-format time-ms) LINE_SEP
        "log-level:    " (first (get valid-log-levels log-lvl)) LINE_SEP
        "category:     " category LINE_SEP
        "file-name:    " file-name LINE_SEP
        "line-number:  " line-number LINE_SEP
        "message:      " msg LINE_SEP
        (if exception
          (format-yaml-exception 
            (create-stacktrace-tree exception) "    ") 
          "")))))

(defn format-clojure-stacktrace-elements
  [elements initial-spacing]
  (apply str 
         (for [e elements]
           (str initial-spacing 
                (format "{:className %s :fileName %s :lineNumber %s :methodName %s }"
                        (str "\"" (get e 0) "\"") 
                        (str "\"" (get e 1) "\"") 
                        (get e 2) 
                        (str "\"" (get e 3) "\"")
                        ) LINE_SEP))))

(defn- format-clojure-exception
  [st indent]
  (str
    indent "{:class     \"" (first st) "\"" LINE_SEP
    indent " :message   \"" (second st) "\"" LINE_SEP
    indent " :stacktrace (" LINE_SEP
    (format-clojure-stacktrace-elements 
      (get st 2)  (str indent "    "))
    indent "    )" LINE_SEP
    (if (get st 3) 
      (str indent ":cause" LINE_SEP
           (format-clojure-exception (get st 3) (str indent "  ")))
      "")
    indent "}" LINE_SEP
    ))


(defn create-clojure-formatter
  "Creates a new formatter with output as a clojure datastructure. An
optional parameter :timestamp-format accepts a string in java.text.SimpleDateFormat
format. The default value is yyyy-MM-dd HH:mm:ss.SSSZ. 

{:timestamp \"2012-12-09 21:27:39.607-0800\"
 :log-level :error
 :category \"my-log\"
 :file-name \"sample_logger.clj\"
 :line-number 35
 :message  \"An error was encountered!\"
 :exception  {:class     \"java.lang.IllegalStateException\"
   :message   \"Divide by zero!\"
   :stacktrace (
      {:className \"sample_logger$eval4216\" :fileName \"sample_logger.clj\" :lineNumber 33 :methodName \"invoke\" }
      {:className \"clojure.lang.Compiler\" :fileName \"Compiler.java\" :lineNumber 6511 :methodName \"eval\" }
      )
  :cause
    {:class     \"java.lang.ArithmeticException\"
     :message   \"Divide by zero\"
     :stacktrace (
        {:className \"clojure.lang.Numbers\" :fileName \"Numbers.java\" :lineNumber 156 :methodName \"divide\" }
        {:className \"clojure.lang.Numbers\" :fileName \"Numbers.java\" :lineNumber 3691 :methodName \"divide\" }
        )
    }
  }
}

"  
  [& {timestamp-format :timestamp-format 
      :or {timestamp-format "yyyy-MM-dd HH:mm:ss.SSSZ"}}]
  (let [ts-format (SimpleDateFormat. timestamp-format)]
    (fn [{:keys [time-ms log-lvl category msg exception file-name line-number]}]
      (str
        "{:timestamp \"" (.format ts-format time-ms) "\"" LINE_SEP
        " :log-level " log-lvl LINE_SEP
        " :category \"" category  "\"" LINE_SEP
        " :file-name \"" file-name "\"" LINE_SEP
        " :line-number " line-number LINE_SEP
        " :message  \"" msg "\"" LINE_SEP
        (if exception (str ":exception" 
                    (format-clojure-exception (create-stacktrace-tree exception) "  ")) 
          "")
        "}" LINE_SEP))))

(defn- format-json-stacktrace-elements
  [elements initial-spacing]
  (apply str
         (for [e elements]
           (str initial-spacing
                (format (str "{\"className\":\"%s\", \"fileName\":\"%s\","
                             "\"lineNumber\":%s, \"methodName\":\"%s\"}")
                        (get e 0)
                        (get e 1)
                        (get e 2)
                        (get e 3))
                LINE_SEP))))

(defn- format-json-exception
  [st indent]
  (str 
    indent "{ \"class\":\"" (first st) "\"," LINE_SEP
    indent "  \"message\":\"" (second st) "\"," LINE_SEP
    indent "  \"stacktrace\": [" LINE_SEP
    (format-json-stacktrace-elements (get st 2) (str indent "    "))
    indent "   ]" LINE_SEP
    (if (get st 3)
      (str indent " \"cause\":" LINE_SEP
           (format-json-exception (get st 3) (str indent "    ")))
      "")
    indent "}" LINE_SEP))

(defn create-json-formatter
  "Creates a new formatter with output in JSON format (http://www.json.org). An
optional parameter :timestamp-format accepts a string in java.text.SimpleDateFormat
format. The default value is yyyy-MM-dd HH:mm:ss.SSSZ. 

E.g. (stacktraces are truncated in this example):  
{ \"timestamp\":\"2012-12-09 21:19:20.841-0800\", 
 \"log-level\":\"ERROR\", 
 \"category\":\"my-log\", 
 \"file-name\":\"sample_logger.clj\", 
 \"line-number\": 35, 
 \"message\":\"An error was encountered!\",
 \"exception\":    { \"class\":\"java.lang.IllegalStateException\",
      \"message\":\"Divide by zero!\",
      \"stacktrace\": [
        {\"className\":\"sample_logger$eval4086\", \"fileName\":\"sample_logger.clj\",\"lineNumber\":33, \"methodName\":\"invoke\"}
        {\"className\":\"clojure.lang.Compiler\", \"fileName\":\"Compiler.java\",\"lineNumber\":6511, \"methodName\":\"eval\"}
       ]
     \"cause\":
        { \"class\":\"java.lang.ArithmeticException\",
          \"message\":\"Divide by zero\",
          \"stacktrace\": [
            {\"className\":\"clojure.lang.Numbers\", \"fileName\":\"Numbers.java\",\"lineNumber\":156, \"methodName\":\"divide\"}
            {\"className\":\"clojure.lang.Numbers\", \"fileName\":\"Numbers.java\",\"lineNumber\":3691, \"methodName\":\"divide\"}
           ]
        }
    }
}

"
  [& {timestamp-format :timestamp-format 
      :or {timestamp-format "yyyy-MM-dd HH:mm:ss.SSSZ"}}]
  (let [ts-format (SimpleDateFormat. timestamp-format)]
    (fn [{:keys [time-ms log-lvl category msg exception file-name line-number]}]
      (str
        "{ \"timestamp\":\"" (.format ts-format time-ms) "\", " LINE_SEP
        " \"log-level\":\"" (first (get valid-log-levels log-lvl)) "\", " LINE_SEP
        " \"category\":\"" category "\", " LINE_SEP
        " \"file-name\":\"" file-name "\", " LINE_SEP
        " \"line-number\":" line-number ", " LINE_SEP
        " \"message\":\"" msg "\"" LINE_SEP  
        (if exception (str ",\"exception\":" (format-json-exception (create-stacktrace-tree exception) "    "))
          "")
        "}" LINE_SEP))))

