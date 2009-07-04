(ns
    #^{:author "Samuel May"
       :doc "Tools for estimating a functions' computational complexity"}
  functerpolate.complexity
  (:refer-clojure :exclude (time))
  (:use (clojure.contrib [def :only (defmacro- defnk)]
			 shell-out)
	(functerpolate regression plot))
  (:import javax.swing.JFrame))

;; Yay OS dependance. I used to do the executing and piping in pure java, using
;; Runtime.exec and streams, but the overhead was ridiculous.
(defmacro os-program-runner [executable command-format]
  `(fn [program# input-generator# input-size#]
     (sh ~@executable 
	 (format ~command-format 
		 program# input-generator# (str input-size#)))))

(def run-program 
     (let [name (System/getProperty "os.name")]
       (cond
	 (some #(= name %) (list "Linux" "Mac OS X"))
	 (os-program-runner ("/bin/sh" "-c") "%2$s %3$s | %1$s > /dev/null")
	 ;; THIS HAS NOT ACTUALLY BEEN TESTED ON WINDOWS
	 (some #(= name %) (list "Windows XP" "Windows Vista"))
	 (os-program-runner ("cmd.exe" "\\C") "%2$s %3$s | %1$s > NUL"))))

(def *runs* 10)

(defmacro- time [expr]
  `(let [start#  (System/nanoTime)
	 result#      ~expr] ;; we time this bit
     ;; return time in seconds
     (/ (double (- (System/nanoTime) start#)) 1000000000.0)))

(defmacro- average-multiple-runs [form]
  `(let [results# (for [i# (range *runs*)]
		    (time ~form))]
     (/ (apply + results#) *runs*)))

(defmulti run-time
  (fn [function input-generator input-size]
    (cond (and (fn? function) 
	       (fn? input-generator))     :clojure-function
	  (and (string? function) 
	       (string? input-generator)) :external-program)))

(defmethod run-time :clojure-function 
  [function input-generator input-size]
  (average-multiple-runs
   (apply function (input-generator input-size))))

(defmethod run-time :external-program
  [program input-generator input-size]
  (average-multiple-runs
   (run-program program input-generator input-size)))

(def *big-O-map*
     {:Linear		"O(N), or linear-time",
      :Logarithmic	"O(log(N)), or logarithmic-time",
      :Exponential	"O(e^N), or exponential-time",
      :Linearithmic	"O(N log(N)), or linearithmic-time",
      :Quadratic	"O(N^2), or quadratic-time",
      :Cubic		"O(N^3), or cubic-time"})

(defn- printdata [n runtimes]
  (when (and (not (empty? n)) (not (empty? runtimes)))
    (println (format "%-12d  %fs" (first n) (first runtimes)))
    (recur (rest n) (rest runtimes))))

(defnk complexity 
  [function input-generator :start 10 :limit 100 :npoints 10 :runs 10
   :plot false :print true :extrapolate []]
  (binding [*runs* runs]
    (let [step       (/ (- limit start) (dec npoints))
	  n          (map int (range start (inc limit) step))
	  runtimes   (map (partial run-time function input-generator) n)
	  regression (fit-model :Best n runtimes)
	  predicted  (map (:function regression) extrapolate)
	  bigO       ((:model regression) *big-O-map*)]
      ;; more needs to be done here
      (when plot
	(. (:frame (plot-fitted-curve regression "Plot of Time Complexity" 
				      n runtimes)) 
	   setDefaultCloseOperation JFrame/EXIT_ON_CLOSE))
      (when print
	(println (format "The suggested complexity is %s." bigO))
	(println (format "Input size    Average runtime over %d runs" *runs*))
	(printdata n runtimes)
	(when (not (empty? extrapolate))
	  (println "Input size    Predicted runtime")
	  (printdata extrapolate predicted)))
      regression)))
