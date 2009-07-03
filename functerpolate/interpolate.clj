(ns 
    #^{:author "Samuel May"
       :doc "Functional interpolation and linear regression tools"}
  functerpolate.interpolate 
  (:use [clojure.contrib.seq-utils :only (rotations)]))

;; References:
;; R. Hornbeck, "Numerical Methods", pg. 43
(defn lagrange-interpolation-fn [xseries yseries]
  (let [Pi (map (fn [[[xj fj] & other-points]]
		  (let [xi (map first other-points)]
		    (fn [x]
		      (* fj (/ (apply * (map #(- x  %) xi))
			       (apply * (map #(- xj %) xi)))))))
		(rotations (map vector xseries yseries)))]
    (fn [x]
      (apply + (map #(% x) Pi)))))
