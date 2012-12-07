(ns logger4clj.appenders
  "Appender and formatter definitions"
  (:use [logger4clj.logger]
        [logger4clj.formatters])
  (:import [java.io File BufferedWriter FileWriter PrintWriter]
           [java.util Date]
           [java.text SimpleDateFormat]))

(defn- get-file-and-validate
  [filename-or-file]
  (let [file (if (instance? File filename-or-file) 
               filename-or-file 
               (if (instance? String filename-or-file) 
                 (File. filename-or-file)
                 (throw (IllegalArgumentException. 
                          "create-file-listener accepts either a java.util.File or a string!"))))
        parent (.getParentFile file)]
    (if (and (.exists parent) (.isDirectory parent) (.canWrite parent))
      file
      (throw (IllegalArgumentException. 
               (format "Log file [%s] has invalid parent folder [%s]. It may not exist or may not be writable!"
                       file parent))))))

(defn- create-print-writer
  [file]
  (PrintWriter. (BufferedWriter. (FileWriter. file true)) true))

(defn- get-rollover-type
  "return either :size or :time"
  [rollover-every])

(def size-units {:byte 1 :bytes 1 
                 :kbyte 1024 :kbytes 1024
                 :mbyte 1048576 :mbytes 1048576
                 :gbyte 1073741824 :gbytes 1073741824})

(def time-units { :minute 13
                 :hour 11
                 :day 8
                 :month 6
                 :year 4})

(defn- should-rollover-by-size? 
  [rollover-every log-file]
  (let [rollover-bytes (* (first rollover-every) (get size-units (second rollover-every)))]
    (> (.length log-file) rollover-bytes)))

(def LOG_EXT_FORMAT (SimpleDateFormat. "yyyyMMdd-HHmm"))

(defn- trim-to-significant-field
  [rollover-period timestamp]
  (.substring timestamp 0 (get time-units rollover-period)))

(defn- should-rollover-by-time?
  [rollover-every previous-timestamp current-timestamp]
  (let [fmt (fn [ts] (trim-to-significant-field 
                    rollover-every 
                    (.format LOG_EXT_FORMAT (Date. ts))))]
    (not= (fmt previous-timestamp) (fmt current-timestamp))))

(defn- is-time-rollover? 
  [rollover-spec]
  (and (keyword? rollover-spec) 
       (some #{rollover-spec} (keys time-units))))

(defn- is-size-rollover?
  [rollover-spec]
  (and (vector? rollover-spec) 
       (some #{(second rollover-spec)} (keys size-units))
       (integer? (first rollover-spec))))

(defn- do-rollover
  "Performs rollover and returns new file to which to write log messages."
  [log-file timestamp]
  (let [new-ext (.format LOG_EXT_FORMAT (Date. timestamp))
        old-file-name (.getAbsolutePath log-file)
        new-file (File. (str old-file-name "." new-ext))]
    (.renameTo log-file new-file)
    (File. old-file-name))) 

(defn- rollover-check
  "Performs rollover if necessary and returns nil or the new file to which to
write log messages (because old file was renamed during rollover."
  [log-file rollover-every previous-timestamp]
  (let [current-timestamp (System/currentTimeMillis)]
    (if (is-time-rollover? rollover-every)
      (when (should-rollover-by-time? rollover-every previous-timestamp current-timestamp)
        (do-rollover log-file current-timestamp))
      (when (and (is-size-rollover? rollover-every)
                 (should-rollover-by-size? rollover-every log-file))
        (do-rollover log-file current-timestamp)))))

(defn- clean-up-old-logs
  [log-file max-logs]
  )

(defn create-file-appender
  "Create file listener that writes to the given file at the given log-level. 
The file stream will remain open until the logger is closed with :close.

Supported options: (see further down for details on each)
     :formatter
     :rollover-every
     :max-logs

An optional :formatter parameter accepts a formatter as its value, e.g.

(create-file-appender \"logs/program.log\" 
       :formatter (create-line-formatter
                    \"[${ts:yyyy-MM-dd HH:mm:ss.SSSZ}][${lvl}] ${msg} ${ex}${n}\"))

This example shows the default formatter. See specific formatter documentation 
for more information on how to configure a formatter.  

Rollover Support (:rollover-every)
---------------------------------

Other options allow rolling over of the logfile when a time or file size limit 
is exceeded.

To do this, use the :rollover-every property, e.g.

(create-file-appender \"logs/program.log\"
        :rollover-every :day) ;; only single units allowed

    -or-

(create-file-appender \"logs/program.log\"
        :rollover-every [10 :mbytes]) ;; must use vector format

Supported units are:

    Time               File size
    ----               ---------
    :minute            :byte / :bytes
    :hour              :kbyte / :kbytes
    :day               :mbyte / :mbytes
    :month             :gbyte / :gbytes
    :year

Singular and plural forms are interchangeable and provided only for readability.

When a log file is 'rolled-over', it is given a timestamp extension (yyyyMMdd-HHmm). 
E.g.

            program.log                
            program.log.20121221-1400
            program.log.20121221-1300
            program.log.20121221-1200

Cleaning Up Old Log Files (:max-logs)
-------------------------------------

By use the :max-logs option, you can specify the number of log files to keep. This 
calculation will be performed after each roll-over and the oldest files will be
deleted from disk. E.g.:

(create-file-appender \"logs/program.log\"
        :rollover-every [10 :mbytes]
        :max-logs 10)

When this option is not specified, log files will accumulate indefinitely. 
"
  [filename-or-file & 
   {formatter :formatter 
    rollover-every :rollover-every
    max-logs :max-logs
    :or 
    {formatter (create-line-formatter 
                 "[${ts:yyyy-MM-dd HH:mm:ss.SSSZ}][${lvl}] ${msg} ${ex}${n}")
     rollover-every nil
     max-logs -1}}]
  (let [io-agent (agent nil)
        file (get-file-and-validate filename-or-file)]
    (create-appender
      ;; does not exit until stream is created
      (fn [] (let [pw  (create-print-writer file)] 
               (await (send io-agent (fn [x] pw)))))
      (fn [timestamp log-lvl category msg exception]
        (when rollover-every
          (when-let [new-file (rollover-check file rollover-every)]
            (let [pw (create-print-writer new-file)]
              (await (send io-agent (fn [x] pw))))
            (when (> max-logs 0) 
              (clean-up-old-logs new-file max-logs))))
        (await (send io-agent 
                     (fn [out]
                       (when out
                         (.print out (formatter timestamp log-lvl category msg exception)))
                       out)))
        ()) 
      ;; does not exit function until stream is closed
      (fn [] (await (send io-agent (fn [out]
                                     (when out 
                                       (.flush out)
                                       (.close out)) 
                                     nil)))))))

(defn create-console-appender
  "Creates a console listener that writes to stdout. 

An optional :formatter parameter accepts a formatter as its value, e.g.

(create-console-appender 
       :formatter (create-line-formatter
                    \"[${ts:yyyy-MM-dd HH:mm:ss.SSSZ}][${lvl}] ${msg} ${ex}${n}\"))

This example shows the default formatter. See specific formatter documentation 
for more information on how to configure a formatter.  
"
  [ & 
   {formatter :formatter :or 
    {formatter (create-line-formatter 
                 "[${ts:yyyy-MM-dd HH:mm:ss.SSSZ}][${lvl}] ${msg} ${ex}${n}")}}]
  (create-appender
    (fn [])
    (fn [timestamp log-level category msg exception]
      (print (formatter timestamp log-level category msg exception)))
    (fn [])))


