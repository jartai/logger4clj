##Logger4clj 0.2
###Clojure Logging API

A fast and versatile logging API written entirely in clojure.core and JDK6. 

Available in leiningen via clojars:

````clojure
  [logger4clj "0.2"]
````

Read the [Logger4clj Manual](https://github.com/jkauzlar/logger4clj/wiki/Logger4clj-Manual) 
for an in-depth introduction.

Additionally, API documentation is available under /doc, generated using Codox, however
I don't think github allows me to serve the pages as html here :(

###Features 0.2

*  File appender supports time- or size-based rollover
*  File appender can clean up old logs
*  File and console appenders accept numerous 'formatters': xml, yaml, 
   json, clojure or formatted string text
*  file name and line number of log statement now log-able
*  category is now the namespace in which the logger was defined, instead of
   the logger name

###Features 0.1

*  Mostly-pure Clojure and a single source file (requires only clojure.core and JDK6)
*  Loggers may be 'chained/bound together' (see second example below)
*  Log messages are passed to a blocking queue, where separate thread(s) will 
   handle expensive I/O operations
*  No external 'properties' configurations; clojure code externalizes nicely by itself!
*  Different log levels (:trace :debug, :info, :warning, :error, :fatal)
*  Comes with two appenders (file and console), but custom appenders are easy 
   to create

###Wish List

*  Expanded documentation
*  printf-style logging messages e.g. (log :error "Invalid parameter [%s]!" 
   parm) => "Invalid parameter [value of parm]!"
*  clojure/java.jdbc-based database logging appender
*  ability to log current thread and namespace
*  Message bundles for externalizing messages e.g. (log :error :invalid-parm-error parm)
*  It may be yet be possible, but very tricky, to only get the logging message 
   parameter to evaluate if the logging level is high enough to warrant it. 
   Currently this would require the courage to attempt writing a macro that produces a macro.



###Examples

__Basic Example:__

In the first example, a logger is created with a file-appender and used to write
various log messages to a file. An appender is 'registered' and then the 
'with-appender' tells the logger to use that appender. You'll see why these are
separate statements later.

````clojure
    (ns mypackage.myfile
      (:use 
        [logger4clj.logger])
      (:require
        [logger4clj.appenders :as apps]))
        
    (def-logger my-logger
      ;; create the appender
      (register-appender :file-appender
        (apps/create-file-appender "/home/johnd/logs/mylog.log"))
        
      ;; use the appender with my-logger
      (with-appenders
        [:file-appender :error])
      (start-logger))
        
    (try
      (my-logger :info "Program is starting...")
      
      (println (+ 1 1))
      
      (catch Exception e
        (my-logger :error "An error occurred!" e)))
````
__More Complicated Example:__        

The following demonstrates a logger being defined in some third-party API and
then being 'bound' to a logger defined by the client program.

Note that the def-logger in the second namespace registers two appenders and
then binds the API's logger (some-api-logger) to itself, using one of its
registered appenders to capture some-api-logger's messages. Following that, it
calls 'with-appenders' to use both appenders for itself as well.

#####file one
````clojure
    (ns com.some-company.some-api
      (:use
        [logger4clj.logger]))
    
    ;; just create a plain logger with no appenders, without starting it    
    (def-logger some-api-logger)
    
    ;; use it
    (defn my-func 
        [x y]
        (try
          (/ x (- y 1))
          (catch ArithmeticException e
              (some-api-logger :error "The value of y cannot be 1" e))))
    
    ;; do other stuff
````    
#####file two
````clojure
    (ns com.another-company.some-program
      (:use
        [logger4clj.logger])
      (:require 
        [com.some-company.some-api :as some-api]
        [logger4clj.appenders :as apps]
        [logger4clj.formatters :as frms]))
        
    (def-logger my-logger
      (register-appender :console
        (apps/create-console-appender
          ;; add a formatter instead of using default
          :formatter (frms/create-line-formatter 
                        "${ts:HH:mm:ss.SSSZ} -  (${fn}:${ln}) ${msg}${n}"))
                        
      (register-appender :log-file
        (apps/create-file-appender 
          "/var/log/some-program/output.log"
          ;; format log msgs as XML
          :formatter (frms/create-xml-formatter)))
      
      ;; use the :log-file appender to write messages from some-api-logger
      ;;   and set the log level to :error for some-api-logger 
      (bind-logger some-api/some-api-logger
        :with-appender [:log-file :error])
        
      ;; for my-logger, append to both console and the log file
      (with-appenders
        [:log-file :info]
        [:console :debug])
        
      ;; don't forget to start the logger - this will start the queue reading
      ;;    threads. You can also stop it if you want, but that's not necessary
      (start-logger))
       
    ;; use the logger 
    (my-logger :info "Logger has been initialized and started!")
    
    ;;the bound-logger (some-api-logger) is now active for logging
    (some-api/my-func 10 1)
    
    ;; do other stuff
````        
Note that it's possible to chain an arbitary number of loggers together; lower-level loggers may even define their 
own appenders and start themselves before being bound a client.

##License

Source Copyright © 2013 Joe Kauzlarich. 
Distributed under the Eclipse Public License, the same as Clojure uses. 
See the file COPYING.




