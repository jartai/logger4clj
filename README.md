##Logger4clj 0.1
###Pure Clojure Logging API

Clojure needs its own logging solution 

###Features

*  Pure Clojure (requires only clojure.core and JDK1.6)
*  Loggers may be 'chained/bound together' (see second example below)
*  Log messages are passed to a blocking queue, where a separate thread
   will handle expensive I/O operations
*  No external 'properties' configurations; clojure code externalizes nicely 
   by itself!
*  Different log levels (:debug, :info, :warning, :error, :fatal)
*  Comes with two appenders at the moment (file and console), but custom 
   appenders are easy to 'plug in'
   
###Wish List

*  Expanded documentation
*  More versatile appenders, and more of them
*  Configurable formatters for appended data
*  It may be yet be possible, but very tricky, to only get the logging message
   parameter to evaluate if the logging level is high enough to warrant it. Currently
   this would require the courage to attempt writing a macro that produces a macro. 


###Examples

In the first example, a logger is created with a file-appender and used to write
various log messages to a file. An appender is 'registered' and then the 
'with-appender' tells the logger to use that appender. You'll see why these are
separate statements later.

__Basic Example:__

    (ns mypackage.myfile
      (:use 
        [logger4clj.logger]))
        
    (def-logger logger
      (register-appender :file-appender
        (create-file-appender "/home/johnd/logs/mylog.log"))
      (with-appenders
        [:file-appender :error]))
        
    (try
      (logger :info "Program is starting...")
      
      (println (+ 1 1))
      
      (catch Exception e
        (logger :error "An error occurred!" e)))
        
The following demonstrates a logger being defined in some third-party API and
then being 'bound' to a logger defined by the client program.

Note that the def-logger in the second namespace registers two appenders and
then binds the API's logger (some-api-logger) to itself, using one of its
registered appenders to capture some-api-logger's messages. Following that, it
calls 'with-appenders' to use both appenders for itself as well.
   
__More Complicated Example:__        
        
    (ns com.some-company.some-api
      (:use
        [logger4clj.logger]))
        
    (def-logger some-api-logger)
    
    ;; do stuff
    
    
    (ns com.another-company.some-program
      (:use
        [logger4clj.logger])
      (:require 
        [com.some-company.some-api :as some-api))
        
    (def-logger logger
      (register-appender :console
        (create-console-appender))
      (register-appender :log-file
        (create-file-appender 
          "/var/log/some-program/output.log" 
          :date-format "yyyy/MM/dd HH:mm:ss"))
      (bind-logger some-api/some-api-logger
        :with-appender [:log-file :error])
      (with-appenders
        [:log-file :info]
        [:console :debug]))
        
    ;; do stuff
        
