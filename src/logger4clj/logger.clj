(ns logger4clj.logger
  (:import [java.io File BufferedWriter FileWriter PrintWriter]
           [java.text SimpleDateFormat]
           [java.util.concurrent LinkedBlockingQueue]
           [java.util Date]))

(def valid-log-levels 
  {:debug '("DEBUG" 0) 
   :info '("INFO" 1) 
   :warning '("WARNING" 2) 
   :error '("ERROR" 3) 
   :fatal '("FATAL" 4) 
   :none '("no-logging-at-this-level", 5)})

(def logging-thread-group (ThreadGroup. "logging-threadgroup"))

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
                                           
                                           ;; Queue for this node and mutable tree
                                           ;;   of bound logger queues
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
             (let [full-msg# [(System/currentTimeMillis) log-lvl# ~logger-name msg# exc#]]
               (.offer q# full-msg#))))))))

(defn- get-logger-var 
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
  "Binds a logger to another logger. Use :with-appender [appender-id log-lvl] 
(with the appender-id defined in the register-appender clauses) to attach an
appender to messages coming from the bound logger. At (at least) the top of a 
chain of bound loggers, a :with-appender clause must be used so that the messages 
are logged somewhere."
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
  "Takes the logger and one or more vectors [appender-id log-lvl]. Appender-id
must be defined beforehand using register-appender."
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
  (let [listener-n (get-lvl-idx listener-lvl)
        msg-n (get-lvl-idx msg-lvl)]
    (<= listener-n msg-n)))

(defn- handle-log-message
  "Passes the log message information to any listener that will accept it"
  [logger [time-ms log-lvl category msg exception]]
  (when-not (= category :-stop-logger)
    (let [appenders (deref (:appenders logger))]
      (doseq [appender-def (vals appenders)] 
        (when (check-log-lvl (second appender-def) log-lvl)
          ((:do-log (first appender-def)) time-ms log-lvl category msg exception))))))

(defn- init-logging-thread
  "Initializes the queue reader thread for the logger. "
  [logger]
  (let [queue (:queue logger)]
    (Thread. logging-thread-group
           (fn []
             (loop []
               (when (is-running? logger)
                 (handle-log-message logger (.take queue))
                 (recur)))))))

(defn- doto-all-appenders
  [logger op]
  (doseq [appender-def (vals (deref (:appenders logger)))]
    ((get (first appender-def) op))))

(defn start-logger
  "Starts this logger and all bound loggers. Recursively creates a thread for 
each bound logger, loggers bound to boung-loggers, etc."
  [logger]
  (let [logger-var (get-logger-var logger)]
    (if-not (is-running? logger-var)
      (do 
        (doto-all-appenders logger-var :init)
        (swap! (:logger-state logger-var) (fn [x] :running))
        (.start (init-logging-thread logger-var))
        (doall (map start-logger (vals (deref (:bound-loggers logger-var))))))
      (throw (IllegalStateException. 
               (format "Logger [%s] is already running when trying to start!" 
                       (:name logger-var))))))
  logger)

(defn stop-logger
  "Stops this logger and all bound loggers. Doesn't complain if the logger is
already stopped."
  [logger]
  (let [logger-var (get-logger-var logger)] 
    (when (is-running? logger-var)
      (doto-all-appenders logger-var :init)
      ;; send stop message to unblock the queue so the thread exits
      (.offer (:queue logger-var) [-1 :none :-stop-logger "" nil])
      (swap! (:logger-state logger-var) (fn [x] :stopped))
      (doall (map stop-logger (vals (deref (:bound-loggers logger-var)))))))
  logger)

;; ############# Appenders #################====================================

(defn- get-file-and-validate
  [filename-or-file]
  (let [file (if (instance? File filename-or-file) 
               filename-or-file 
               (if (instance? String filename-or-file) 
                 (File. filename-or-file)
                 (throw (IllegalArgumentException. 
                          "create-file-listener accepts either a File or String!"))))
        parent (.getParentFile file)]
    (if (and (.exists parent) (.isDirectory parent) (.canWrite parent))
      file
      (throw (IllegalArgumentException. 
               (format "Log file [%s] has invalid parent folder [%s]. It may not exist or may not be writable!"
                       file parent))))))

(defn create-appender
  [init do-log clean-up]
  {:init init
   :do-log do-log ;; function should accept timestamp, log-lvl, category, msg, exception
   :clean-up clean-up})

(defn- create-print-writer
  [filename-or-file]
  (PrintWriter. (BufferedWriter. (FileWriter. (get-file-and-validate filename-or-file) true)) true))

(defn create-file-appender
  "Create file listener that writes to the given file at the given log-level. The file stream
will remain open until the logger is closed with :close."
  [filename-or-file & 
   {date-fmt-str :date-format :or 
    {date-fmt-str "yyyy-MM-dd HH:mm:ss.SSSZ"}}]
  (let [io-agent (agent nil)
        date-fmt (SimpleDateFormat. date-fmt-str)]
    (create-appender
      ;; does not exit until stream is created
      (fn [] (when-let [pw  (create-print-writer filename-or-file)] 
               (await (send io-agent (fn [x] pw)))))
      (fn [timestamp log-lvl category msg exception]
        (send io-agent 
              (fn [out]
                (when out
                  (.println out
                    (format "[%s]{%s}[%s] %s" 
                            (.format date-fmt timestamp)
                            category
                            (first (get valid-log-levels log-lvl))
                            msg))
                  (when-not (nil? exception)
                    (.printStackTrace exception out))
                  ;; don't return anything
                  out))))
      ;; does not exit function until stream is closed
      (fn [] (await (send io-agent (fn [out]
                                     (when out 
                                       (.flush out)
                                       (.close out)
                                       nil))))))))

(defn create-console-appender
  "Creates a console listener that writes to stdout at the given log level"
  [ & 
   {date-fmt-str :date-format :or 
    {date-fmt-str "yyyy-MM-dd HH:mm:ss.SSSZ"}}]
  (let [date-fmt (SimpleDateFormat. date-fmt-str)]
    (create-appender
      (fn [])
      (fn [timestamp log-level category msg exception]
                         (do
                           (println 
                             (format "[%s]{%s}[%s] %s" 
                                     (.format date-fmt timestamp)
                                     category
                                     (first (get valid-log-levels log-level))
                                     msg))
                           (if (not (nil? exception))
                             (.printStackTrace exception))))
      (fn []))))


