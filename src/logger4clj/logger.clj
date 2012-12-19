(ns logger4clj.logger
  "
File:     logger4clj/logger.clj
Website:  http://github.com/jkauzlar/logger4clj
Author:   Joe Kauzlarich

Description: 
  Logger4clj is a hierarchical logger for the clojure language. See the 
  website listed above or README.md in this project for more information.
"
  (:import [java.util.concurrent LinkedBlockingQueue]))

(def CURRENT_VERSION "0.2")

(def valid-log-levels 
  {:trace '("TRACE" 0)
   :debug '("DEBUG" 1) 
   :info '("INFO" 2) 
   :warning '("WARNING" 3) 
   :error '("ERROR" 4) 
   :fatal '("FATAL" 5) 
   :none '("no-logging-at-this-level", -1)})

(def logging-thread-group (ThreadGroup. "logger4clj-threadgroup"))

(defmacro def-logger
  "Constructs two vars in the namespace in which this is called: one is a 
function named by the logger-var; the other is the data, def'ed by the name 
'logger-var'-var. Beware not to redefine these vars.

E.g. (def-logger my-logger) 
     ;; => produces
     (def my-logger-var ...)
     (defn my-logger ...) 

NOTE: it should be possible to use a defmacro instead of a defn to delay
expansion of the arguments until log-lvl has been tested.
"
  [logger-var & opts]
  (let [logger-name (str logger-var)
        logger-var-name (str logger-var "-var")]
    `(do
       (def ~(symbol logger-var-name) (-> {:name ~logger-name
                                           
                                           ;; Queue for this node 
                                           :queue  (LinkedBlockingQueue.)
                                           
                                           ;; map of appender-ids -> [appender-def log-lvl] 
                                           ;;   used by this logger
                                           :appenders (atom {})
                                           
                                           ;; map of appender-ids to appender definitions, registered
                                           ;;  through register-appender on this instance
                                           :registered-appenders (atom {})
                                           
                                           ;; map of bound-logger-name to bound-logger
                                           :bound-loggers (atom {})
                                           
                                           ;; runtime state of logger
                                           ;;   either :stopped or :running
                                           :logger-state (atom :stopped)}
                                        ~@opts))
       (defn ~(symbol logger-name)
         ([opt#]
           (condp = opt#
             :-internal ~(symbol logger-var-name)))
         ([log-lvl# msg#]
           (~logger-var log-lvl# msg# nil))
         ([log-lvl# msg# exc#]
           (when-let [q# (:queue ~(symbol logger-var-name))]
             (let [stacktrace# (apply vector 
                                      (seq (.getStackTrace 
                                             (Thread/currentThread))))
                   ;; if 2-arg method was called, then item is at 4th level, otherwise 3rd
                   stacktrace-item# (if (= (get stacktrace# 1) (get stacktrace# 2))
                                      (get stacktrace# 3)
                                      (get stacktrace# 2))
                   full-msg# {:time-ms (System/currentTimeMillis)
                              :log-lvl log-lvl#
                              :category ~logger-name
                              :msg msg#
                              :exception exc#
                              :file-name (.getFileName stacktrace-item#)
                              :line-number (.getLineNumber stacktrace-item#)}]
               (.offer q# full-msg#))))))))

(defn- get-logger-var 
  "Takes either the logger function or logger data and returns the logger data. This
allows some flexibility in function arguments as to which is provided."
  [logger-or-data]
  (if (map? logger-or-data)
    logger-or-data
    (logger-or-data :-internal)))

(defn- comma-separated
  [list-of-strings]
  (apply str (apply concat (interpose "," (map str list-of-strings)))))

(defn print-logger-info
  [logger]
  (let [logger-var (get-logger-var logger)]
    (println (format "Logger name: [%s]" (:name logger-var)))
    (println (format "State: [%s]" (deref (:logger-state logger-var))))
    (println (str "Bound loggers: " 
                  (comma-separated (keys (deref (:bound-loggers logger-var))))))
    (println (str "Local appenders: " 
                  (comma-separated (keys (deref (:appenders logger-var))))))
    (println (str "Registered appenders: " 
                  (comma-separated (keys (deref (:registered-appenders logger-var))))))))

(defn register-appender
  "Register an appender to this logger, which can be referenced by the appender-id
in bind-logger or with-root-appenders clauses. Return the given logger."
  [logger appender-id appender]
  (let [logger-var (get-logger-var logger)] 
    (swap! (:registered-appenders logger-var) assoc appender-id appender))
  logger)

(defn- maybe-get-appender
  [logger-var appender-id]
  (when-not (nil? appender-id)
    (if-let [appender (get (deref (:registered-appenders logger-var)) appender-id)]
      appender
      (throw (IllegalArgumentException. 
               (format "Appender ID [%s] isn't registered!" appender-id))))))

(defn- push-appender
  "recursively pushes an appender downward through bound loggers, adding the
appender to each"
  [logger-var appender-id appender log-lvl]
  (swap! (:appenders logger-var)
             assoc
             appender-id 
             [appender log-lvl])
  (doseq  [l (vals (deref (:bound-loggers logger-var)))]
    (push-appender l appender-id appender log-lvl)))

(defn bind-logger
  "Binds a logger to another logger. Optionally use :with-appender [appender-id log-lvl] 
(with the appender-id defined in the register-appender clauses) to attach an
appender to messages coming from the bound logger. At (at least) the top of a 
chain of bound loggers, a :with-appender clause must be used when messages must be 
logged somewhere.

If no appender is specified, then this binding will only be used when this logger
is bound by another logger.

Note: this implementation currently does not check for cycles in binding chains; a stack
      overflow would be the likely outcome."
  [logger target-logger & opts]
  (let [logger-var (get-logger-var logger)
        target-logger-var (get-logger-var target-logger)
        [appender-id log-lvl] (if (= (first opts) :with-appender)
                                (second opts)
                                [nil nil])
        appender (maybe-get-appender logger-var appender-id)]
    (swap! (:bound-loggers logger-var) 
           assoc (:name target-logger-var) target-logger-var)
    (when-not (nil? appender-id)
      (push-appender target-logger-var appender-id appender log-lvl)))
  logger)

(defn with-appenders
  "Assigns registered appenders to this logger. Takes the logger and one or more 
vectors [appender-id log-lvl]. Appender-id must be defined beforehand using 
register-appender."
  [logger & appender-specs]
  (let [logger-var (get-logger-var logger)]
    (swap! (:appenders logger-var) 
         merge 
         (reduce (fn [app-map [appender-id log-lvl]]
                   (let [app (get (deref (:registered-appenders logger-var)) appender-id)]
                     (assoc app-map appender-id [app log-lvl]))) 
                 {} appender-specs)))
  logger)

(defn set-log-level
  "Used to reset the log-lvl of an appender at runtime."
  [logger appender-id log-lvl]
  (let [logger-var (get-logger-var logger)]
    (if-let [current-def (get (:appenders logger-var) appender-id)]
      (swap! (:appenders logger-var)
             (fn [appenders]
               (assoc appender-id [(first current-def) log-lvl])))
      (throw (IllegalArgumentException. 
               (format "Appender [%s] is not defined in logger [%s]!" 
                       appender-id (:name logger-var)))))))

(defn is-running?
  [logger]
  (let [logger-var (get-logger-var logger)] 
    (= (deref (:logger-state logger-var)) 
       :running)))

(defn- set-logger-state
  [logger-var state]
  (swap! (:logger-state logger-var) (fn [x] state)))

(defn- get-lvl-idx
  "Returns a numeric value for the logging level, so that it may be compared
against listener thresholds."
  [lvl]
  (if (contains? valid-log-levels lvl)
    (second (get valid-log-levels lvl))
    -1))

(defn- check-log-lvl
  "Returns true if the listener's lvl permits the msg based on the msg-lvl"
  [listener-lvl msg-lvl]
  (let [listener-idx (get-lvl-idx listener-lvl)
        msg-idx (get-lvl-idx msg-lvl)]
    (<= listener-idx msg-idx)))

(defn- handle-log-message
  "Passes the log message information to any listener that will accept it"
  [logger msg-parms]
  (when-not (= (:category msg-parms) :-stop-logger)
    (doseq [appender-def (vals (deref (:appenders logger)))] 
      (when (check-log-lvl (second appender-def) (:log-lvl msg-parms))
        (println "Message parameters: " msg-parms)
        ((:do-log (first appender-def)) msg-parms)))))

(defn- init-logging-thread
  "Initializes and starts the queue reader thread for the logger. "
  [logger]
  (let [queue (:queue logger)]
    (.start (Thread. logging-thread-group
                     (fn []
                       (loop []
                         (when (is-running? logger)
                           (handle-log-message logger (.take queue))
                           (recur))))))))

(defn- doto-all-appenders
  [logger op]
  (doseq [appender-def (vals (deref (:appenders logger)))]
    ((get (first appender-def) op))))

(defn- doto-all-bound-loggers
  [logger-var op]
  (doseq [bound-logger (vals (deref (:bound-loggers logger-var)))]
    (op bound-logger)))

(defn- send-stop-message
  "This will unblock the queue, but not send anything, so the thread exits"
  [logger-var]
  (.offer (:queue logger-var) [-1 :none :-stop-logger "" nil]))

(defn start-logger
  "Starts this logger and all bound loggers. Recursively creates a thread for 
each bound logger, loggers bound to boung-loggers, etc."
  [logger]
  (let [logger-var (get-logger-var logger)]
    (if-not (is-running? logger-var)
      (doto logger-var 
        (doto-all-appenders :init)
        (set-logger-state :running)
        (init-logging-thread)
        (doto-all-bound-loggers start-logger))
      (throw (IllegalStateException. 
               (format "Logger [%s] is already running when trying to start!" 
                       (:name logger-var))))))
  logger)

(defn stop-logger
  "Stops this logger and all bound loggers. Doesn't complain if the logger is
already stopped. 

NOTE: Currently, messages may still be logged to a stopped logger; if
the logger is started again, these messages will be delivered."
  [logger]
  (let [logger-var (get-logger-var logger)] 
    (when (is-running? logger-var)
      (doto logger-var 
        (doto-all-appenders :clean-up)
        (set-logger-state :stopped)
        (send-stop-message)
        (doto-all-bound-loggers stop-logger))))
  logger)

(defn create-appender
  "Define a custom appender. This accepts three functions, to be defined as follows:
   * init:   () -> ()
   * do-log: (msg-parms) -> ()
   * clean-up: () -> ()

The msg-parms is a map containing the following keys:
   :time-ms
   :log-lvl 
   :category 
   :msg
   :exception
   :file-name
   :line-number
"
  [init do-log clean-up]
  (do
    (assert (fn? init) 
            (str "init parameter must be a function, but instead is a " (type init) "!"))
    (assert (fn? do-log) 
            (str "do-log parameter must be a function, but instead is a " (type do-log) "!"))
    (assert (fn? clean-up) 
            (str "clean-up parameter must be a function, but instead is a " (type clean-up) "!"))
    {:map-type ::logger4clj-appender
     :init init
     :do-log do-log
     :clean-up clean-up}))

