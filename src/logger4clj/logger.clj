(ns logger4clj.logger
  (:import [java.io File BufferedWriter FileWriter PrintWriter]
           [java.text SimpleDateFormat]
           [java.util.concurrent LinkedBlockingQueue]
           [java.util Date]))

;; Example usage:
;; (def log (create-logger 
;;             (create-file-listener "/home/person/my-log.log" :info) 
;;             (create-console-listener :info)))
;;
;; (log :error "this is an error message")
;;
;; (try
;;     (do-something)
;;     (catch Exception e
;;         (log :fatal "something went wrong!"  e)))
;;
;; (def session-log (create-category-logger "session"))
;; 
;; ; bind session-log to original log 
;; (session-log log)
;; (session-log :info "User logged in")
;;
;; ; close logging resources 
;; (log :close)
;;

(def valid-log-levels 
  {:debug '("DEBUG" 0) :info '("INFO" 1) :warning '("WARNING" 2) :error '("ERROR" 3) :fatal '("FATAL" 4)})

(def logging-thread-group (ThreadGroup. "logging-threadgroup"))

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
  [listener-lst [time-ms log-lvl category msg exception]]
  (dorun (map 
           (fn [l] 
             (if (check-log-lvl ((:get-log-level-fn l)) log-lvl) 
               ((:write-log-msg-fn l) time-ms log-lvl category msg exception))) 
              listener-lst)))

(defn- offer-queue
  "Offers vector containing log msg information to the logging queue, then
returns the queue. Used by an agent 'send' function."
  [q log-lvl category msg exception]
  (do 
    (.offer q [(System/currentTimeMillis) log-lvl category msg exception])
    q))

(defn create-logger
  "Creates and starts a logger thread with the given list of listeners. The 
returned value is a function ([log-lvl category msg][log-lvl msg][option]) -> nil.
The available option in the last case is :close, which stops the logging thread 
and calls the clean-up-fn on all the listeners. Once the logging thread is
stopped, it cannot be restarted."
  [& listeners]
  (let [log-queue (agent (LinkedBlockingQueue.))
        stop-logger (atom false)
        logging-thread (Thread. 
                         logging-thread-group
                         (fn [] 
                           (loop []
                             (if (not @stop-logger)
                               (do 
                                 (handle-log-message listeners (.take @log-queue))
                                 (recur))))))]
    (do
      (dorun (map (fn [l] ((:init-fn l))) listeners))
      (doto logging-thread
        (.setDaemon true) ;; shutdown on jvm exit
        (.setName "Logger")
        (.setPriority Thread/MIN_PRIORITY)
        (.start))
      (fn 
        ([log-lvl msg exception]
          (do (send log-queue offer-queue log-lvl "root" msg exception)
            ;; don't return agent ref
            nil))
        ([log-lvl  msg]
          (do (send log-queue offer-queue log-lvl "root" msg nil)
            ;; don't return agent ref
            nil))
        ([option]
          (condp = option
            :close (do
                     (reset! stop-logger true)
                     (dorun (map (fn [l] ((:clean-up-fn l))) listeners)))
            :internal {:queue log-queue 
                       :thread logging-thread 
                       :stop-logger stop-logger
                       :category-name "root"}
            :listeners listeners
            (throw (IllegalArgumentException. 
                   (str "Categorized logger option [" option "] unknown!")))))))))



(defn create-category-logger
  "Creates a logger that must be bound to a root logger (i.e. a logger created
using create-logger). For example,
        (def logger (create-logger (create-console-listener :debug)))
        (def subsystem-logger (create-category-logger \"subsystem-logger\"))
        
        ;; bind subsystem-logger to logger
        (subsystem-logger logger)

        ;; use subsystem-logger
        (subsystem-logger :info \"Starting subsystem...\")

An unbound category logger will have do nothing when attempting to log to it.
"
  [category-name]
  (let [internal (atom nil)]
    (fn 
      ([log-lvl msg exception]
        (if (not (nil? @internal))
          (do 
            (send (:queue @internal) offer-queue log-lvl category-name msg exception)
            nil)))
      ([log-lvl msg]
        (if (not (nil? @internal))
          (do
            (send (:queue @internal) offer-queue log-lvl category-name msg nil)
            nil)))
      ([option]
        (if (= option :internal)
          @internal
          (if (ifn? option)
            (reset! internal (option :internal))
            (throw (IllegalArgumentException. 
                   (str "Categorized logger option [" option "] unknown!")))))))))


(defn create-file-listener
  "Create file listener that writes to the given file at the given log-level. The file stream
will remain open until the logger is closed with :close."
  [file-name log-level]
  (let [file (File. file-name)
        ;; create PrintWriter with autoflush true
        out (PrintWriter. (BufferedWriter. (FileWriter. file true)) true)
        date-fmt (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss.SSSZ")]
    {:init-fn (fn [] )
     :write-log-msg-fn
     (fn [timestamp log-level category msg exception] 
       (do
         (.println out 
           (str "["
                (.format date-fmt timestamp) "]{"
                category
                "}["
                (first (get valid-log-levels log-level)) "] "
                msg))
         (if (not (nil? exception))
           (.printStackTrace exception out))
         (.flush out))
       ;; don't return 'out'
       nil)
       
     :get-log-level-fn
     (fn [] log-level)
       
     :clean-up-fn
     (fn [] (.close out))
     }))

(defn create-console-listener
  "Creates a console listener that writes to stdout at the given log level"
  [log-level]
  (let [date-fmt (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss.SSSZ")]
    {
     :init-fn (fn [])
     :write-log-msg-fn (fn [timestamp log-level category msg exception]
                         (do
                           (println 
                             (str "["
                                  (.format date-fmt timestamp) "]{"
                                  category
                                   "}["
                                  (first (get valid-log-levels log-level)) "] "
                                  msg))
                           (if (not (nil? exception))
                             (.printStackTrace exception))))
     :get-log-level-fn (fn [] log-level)
     :clean-up-fn (fn [])
     }))


