(ns 
    #^{:author "Samuel May"
       :doc "Functions for interpolation of data"}
  functerpolate.interpolate 
  (:use [clojure.contrib.seq-utils :only (rotations)]))

;; References:
;; R. Hornbeck, "Numerical Methods", pg. 43
(defn lagrange-interpolation-fn [xseries yseries]
  "Given a sequence of n x-values and n corresponding-y values, returns a 
clojure function implementing a nth-order polynomial, that passes through all 
the given points. The method of Lagrange polynomials is used."
  (let [Pi (map (fn [[[xj fj] & other-points]]
		  (let [xi (map first other-points)]
		    (fn [x]
		      (* fj (/ (apply * (map #(- x  %) xi))
			       (apply * (map #(- xj %) xi)))))))
		(rotations (map vector xseries yseries)))]
    (fn [x]
      (apply + (map #(% x) Pi)))))
