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
           (fn [t l c m e] (.format fmt t)))
    "lvl" (fn [t l c m e] (first (get valid-log-levels l)))
    "cat" (fn [t l c m e] c)
    "msg" (fn [t l c m e] m)
    "ex" (fn [t l c m e] (if-not (nil? e) 
                           (if (= param "msg")
                             (.getLocalizedMessage e)
                             (str LINE_SEP (create-stacktrace-string e)))
                           ""))
    "n" (fn [t l c m e] LINE_SEP)
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

E.g. [${ts:yyyy-MM-dd HH:mm:ss.SSSZ}][${lvl}] ${msg} ${ex}${n}
     => [2012-11-28 15:55:58.985][DEBUG] this is a log message

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
    ${n}   - platform line ending

Remember to put a newline at the end if you want each message on a separate
line!.
"
  [format-str]
  (let [[line-template replacement-funcs] (parse-line-formatter-string format-str)]
    (fn [timestamp log-level category msg ex]
      (String/format line-template 
                     (object-array 
                       (for [func replacement-funcs] 
                         (func timestamp log-level category msg ex)))))))

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
    (if (nil? (get cause 3)) "" (format-causes (get cause 3)))
    ))

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
  []
  (fn [timestamp log-level category msg ex]
    (str 
      "<log-message>" LINE_SEP
      "    <timestamp>" timestamp "</timestamp>" LINE_SEP
      "    <log-level>" log-level "</log-level>" LINE_SEP
      "    <category>" category "</category>" LINE_SEP
      "    <message>" msg "</message>" LINE_SEP
      (if ex 
        (str 
          "    <exception>" LINE_SEP
          (format-xml-exception (create-stacktrace-tree ex) "    ")
          "    </exception>" LINE_SEP) 
        "")
      "</log-message>" LINE_SEP)))

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
  []
  (fn [timestamp log-level category msg ex]
    (str
      "---" LINE_SEP
      "timestamp:    " timestamp LINE_SEP
      "log-level:    " log-level LINE_SEP
      "category:     " category LINE_SEP
      "message:      " msg LINE_SEP
      (if ex 
        (format-yaml-exception 
          (create-stacktrace-tree ex) "    ") 
        ""))))

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
  []
  (fn [timestamp log-level category msg ex]
    (str
      "{:timestamp \" " timestamp "\"" LINE_SEP
      " :log-level " log-level LINE_SEP
      " :category \"" category  "\"" LINE_SEP
      " :message  \"" msg "\"" LINE_SEP
      (if ex (str ":exception" 
                  (format-clojure-exception (create-stacktrace-tree ex) "  ")) 
        "")
      "}" LINE_SEP
      )))


(defn create-json-formatter
  []
  (fn [timestamp log-level category msg ex]
    (str
      )))
