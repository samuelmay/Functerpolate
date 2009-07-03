(ns
    #^{:author "Samuel May"
       :doc "Tools for estimating a functions' computational complexity"}
  functerpolate.complexity
  (:refer-clojure :exclude (time))
  (:use (clojure.contrib [def :only (defnk)])
	(functerpolate interpolate plot)))

(defn- time [function input]
  (let [start-time (System/currentTimeMillis)
	result (apply function input) ;; we time this bit!
	end-time (System/currentTimeMillis)]
    (- end-time start-time)))


(defn average-run-time [function input nruns]
  ;; run function nruns times and get list of results
  (let [results (for [i (range nruns)] (time function input))]
    ;; return average value of results
    (/ (apply + results) (count results))))

(defn function-complexity [function input-generator limit npoints nruns]
  (let [x (rest (range 0 (inc limit) (/ limit npoints)))
	inputs (map input-generator x)
	y (map #(average-run-time function % nruns) inputs)]
    (lagrange-interpolation-fn (map vector x y))))
