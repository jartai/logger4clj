(ns sample-logger
  (:use 
    [logger4clj.logger]
    [logger4clj.appenders]
    [logger4clj.formatters]))


(try
  
  (def-logger io-log)
  
  (def-logger session-log
    (bind-logger io-log))
  
  (def-logger my-log
    (register-appender :session-log  
                       (create-file-appender 
                         "/home/joseph/testlog.log"
                         :formatter (create-line-formatter  
                                      "[${ts:yyyy-MM-dd HH:mm:ss.SSSZ}][${lvl}] ${msg} ${ex}${n}")))
    
    (register-appender :console  
                       (create-console-appender
                         :formatter (create-clojure-formatter)))
    (bind-logger session-log
                 :with-appender [:session-log :info])
    (with-appenders 
      [:console :debug] 
      [:session-log :error])
    (start-logger))

  (try 
    ;; create a nested stack trace
    (/ 0 0)
    (catch Exception e
      (throw (IllegalStateException. "Divide by zero!" e))))
  
  (catch Exception e
    (my-log :error "An error was encountered!" e)))
