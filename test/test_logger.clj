(ns logger4clj.test-logger
  (:use 
    [logger4clj.logger]))


(try
  
  (def-logger io-log)
  
  (def-logger session-log
    (bind-logger io-log))
  
  (def-logger my-log
    (register-appender :session-log  
                       (create-file-appender "/home/joseph/testlog.log"))
    
    (register-appender :console  
                       (create-console-appender
                         :date-format "HH:mm:ss.SSS"))
    (bind-logger session-log
                 :with-appender [:session-log :info])
    (with-appenders 
      [:console :debug] 
      [:session-log :error])
    (start-logger))
  
  (catch Exception e
    (.printStackTrace e)))
