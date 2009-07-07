(ns
    #^{:author "Samuel May"
       :doc "A simple Clojure wrapper around the JOpenChart2 library"}
  functerpolate.tests
  (:require [clojure.contrib.generic.collection :as gc])
  (:use (functerpolate interpolate regression complexity plot)
	(clojure.contrib.probabilities 
	 [random-numbers :only (rand-stream)]
	 [monte-carlo    :only (random-stream normal)])))

;; Apply uniform gaussian noise of a given mean magnitude to a sequence
(defn add-noise 
  ([seq magnitude] 
     (map + seq (gc/seq (random-stream (normal 0 magnitude) rand-stream))))
  ([seq]
     ;; if no magnitude given, guess one
     (let [range (- (apply max seq) (apply min seq))
	   mag   (/ range (count seq))]
       (add-noise seq mag))))

(defn plot-with-noise [title function x]
  (let [a (first x)
	b (last x)
	noisy-y (add-noise (map function x))]
    (with-new-plot
     (add-label title)
     (add-function "Function" function a b)
     (add-data "Noisy data" x noisy-y))))

;; Example 11.2, pg 493, "Applied Statistics for Engineers and Scientists",
;; Devore/Farnum.
(def dataset-1 
     [[5.7 6.8 9.6 10.0 10.7 12.6 14.4 15.0 15.3 16.2 17.8 18.7 19.7 20.6
       25.0]
      [119.0 121.3 118.2 124.0 112.3 114.1 112.2 115.1 111.3 107.2 108.9 107.8 
       111.0 106.2 105.0]])

(defn linear-regression-1 []
  (let [[x y] dataset-1
	regression (fit-model :Linear x y)]
    (with-new-plot
     (add-label {:title "Clay Brick Masonry Weight Variation"
		 :xaxis "mortar air content (%)"
		 :yaxis "mortar dry density (lb/ft^3)"})
     (add-function "Line of best fit" (:function regression) 
		   (first x) (last x))
     (add-data "Data" x y))))

;; Section 11.1 Exercise 4, pg 499, "Applied Statistics for Engineers and
;; Scientists", Devore/Farnum.
(def dataset-2
     [[5.0 12.0 14.0 17.0 23.0 30.0 40.0 47.0 55.0 67.0 72.0 81.0 96.0 112.0 
       127.0]
      [4.0 10.0 13.0 15.0 15.0 25.0 27.0 46.0 38.0 46.0 53.0 70.0 82.0 99.0 
       100.0]])

(defn linear-regression-2 []
  (let [[x y] dataset-2
	regression (fit-model :Best x y)]
    (with-new-plot
     (add-label {:title "Highway Runoff"
		 :xaxis "Rainfall volume (m^3)"
		 :yaxis "Runoff volume (m^3)"})
     (add-function "Best fitted curve" (:function regression) 
		   (first x) (last x))
     (add-data "Data" x y))))

(defn interpolated-fn-plot [title x y]
  (let [function (lagrange-interpolation-fn x y)]
    (with-new-plot
     (add-label title)
     (add-function "Interpolated Function" function (first x) (last x))
     (add-data "Given Points" x y))))

(defn interpolated-fn-1 []
  (let [x [1 2 4 8]
	y [1 3 7 11]]
    (interpolated-fn-plot "Example Interpolated Function" x y)))

(defn interpolated-fn-2 []
  (let [x [0     1.2   1.7    2.8    4.4   5.8   7.0   8.0]
	y [1.000 0.671 0.398 -0.185 -0.342 0.092 0.300 0.172]]
    (interpolated-fn-plot "Another Example Interpolated Function" x y)))
