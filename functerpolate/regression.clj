(ns
    #^{:author "Samuel May"
       :doc "Tools for linear regression, non-linear regression using linear transforms, and least-squares curve fitting"}
  functerpolate.regression
  (:use [clojure.contrib.def :only (defmacro-)]))

;; References
;; J. Devore, and N. Farnum, "Applied Statistics for Engineers and Scientists"
;; chapter 11.
;; also look up 'Least Squares Fitting' on mathworld.com

(defn- mean [series]
  (/ (apply + series)
     (count series)))

(defmacro- sum [series]
  `(apply + ~series))

;; gives you the least squares line fitting the two data series.
;; see pg 117, Devore/Farnum
(defn- sum [s]
  (apply + s))

(defn- sumproduct [s1 s2]
  ;; they should both have the same length   
  (sum (map * s1 s2)))

(defn- sum-of-squares [s1 s2]
  (let [n (count s1)]
    (- (sumproduct s1 s2) (/ (* (sum s1) (sum s2)) n))))

(defn linear-regression [x y]
  "Returns a vector of a,b, coefficient of detirmination, and regression sum of
squares"
  (let [xbar    (mean x)
	ybar    (mean y)
	Sxy     (sum-of-squares x y)
	Syy     (sum-of-squares y y)
	Sxx     (sum-of-squares x x)
	b       (/ Sxy Sxx)
	a       (- ybar (* b xbar))
	SSResid (- Syy (* b Sxy))
	SSTo    Syy]
    [a b (- 1 (/ SSResid SSTo)) (- SSTo SSResid)]))

(defn- log [x]
  (Math/log x))

(defn- exp [x]
  (Math/exp x))

;; TODO: power law fitting:
;; http://mathworld.wolfram.com/LeastSquaresFittingPowerLaw.html
;; should should be linear regression of log(x) and log(y)

;; TODO: linearithmic fitting - important for many algorithms. Research this.

(def *models* (vector :Linear :Logarithmic :Exponential))

;; type is a keyword, name is a string
(defstruct regression-fit :model :name :function :formula :r2 :SS)

;; returns a regression-fit struct
(defmulti fit-model (fn [model x y] model))

(defmethod fit-model :Linear [model x y]
  (let [[a b r2 SS] (linear-regression x y)]
    (struct regression-fit model "linear"
	    (fn [x] (+ a (* b x)))
	    (format "y = %fx%+f" b a)
	    r2 SS)))

(defmethod fit-model :Exponential [model x y]
  (let [[a b r2 SS] (linear-regression x (map log y))
	alpha (exp a)
	beta b]
    (struct regression-fit model "exponential"
	    (fn [x] (* alpha (exp (* beta x))))
	    (format "y = %fe^(%fx)" alpha beta)
	    r2 SS)))

(defmethod fit-model :Logarithmic [model x y]
  (let [[a b r2 SS] (linear-regression (map log x) y)]
    (struct regression-fit model "logarithmic"
	    (fn [x] (+ a (* b (log x))))
	    (format "y = %f%+fln(x)" a b)
	    r2 SS)))

(defmethod fit-model :Best [best x y] 
  (let [{:keys [model name function formula r2 SS]} 
	(last (sort-by :r2 (map #(fit-model % x y) *models*)))]
    (struct regression-fit model "best"
	    function
	    (str formula " (suggested " name " fit)")
	    r2 SS)))
