(use '(functerpolate interpolate 
		     regression
		     plot)
     '(clojure.contrib command-line))
(import 'javax.swing.JFrame)

(defn read-input []
  (loop [input [() () ()] next-line (read-line)]
    (if (nil? next-line)
      (let [[x y unknowns] input]
	[(into [] (reverse x))
	 (into [] (reverse y))
	 (into [] (reverse unknowns))])
      (let [data (map #(Double/parseDouble %) 
		      ;; Aaah regular expressions. Larry Wall is *not* allowed
		      ;; to talk about Lisp being ugly.
		      (re-seq #"[-+]?[0-9]+(?:\.[0-9]+)?" next-line))
	    [x y unknowns] input]
	(recur (if (== (count data) 1)
		 [x y (cons (first data) unknowns)]
		 [(cons (first data) x) (cons (second data) y) unknowns]) 
	       (read-line))))))

(defn write-output [x y]
  (when (and (not (empty? x)) (not (empty? y)))
    (println (first x) (first y))
    (recur (rest x) (rest y))))

(defmacro with-input [function & args]
  `(let [[x# y# unknowns#] (read-input)]
     (~function x# y# unknowns# ~@args)))

(def *make-plot*)

;; extension of reduce to deal with sequences of sequences
(defn funky-reduce [f & colls]
  (apply f (map #(apply f %) 
		(filter (complement empty?) colls))))

(defn eval-unknown-values [function x unknowns]
  [(map function unknowns)
   (funky-reduce min x unknowns)
   (funky-reduce max x unknowns)])

(defn perform-interpolation [x y unknowns]
  (let [function (lagrange-interpolation-fn x y)
	interpolated-values (map function unknowns)
	[interpolated-values a b] (eval-unknown-values function x unknowns)]
    (if *make-plot*
      ;; 'with-new-plot' should return the JFrame
      (let [plot (with-new-plot "Results Of Interpolation"
		   (add-function "Interpolated Function" function a b)
		   (add-data "Given Points" x y)
		   (add-data "Interpolated Points" unknowns 
			     interpolated-values))]
	(. plot setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))
    (println "Interpolated values:")
    (write-output unknowns interpolated-values)))

(defn perform-regression [x y unknowns model]
  (let [regression (fit-model model x y)
	{:keys [name function formula r2 SS]} regression
	[interpolated-values a b] (eval-unknown-values function x unknowns)]
    (if *make-plot*
      (plot-fitted-curve regression "Results Of Regression" x y))
    (println "Interpolated values:")
    (write-output unknowns interpolated-values)))

;; Commas as whitespace and Ruby-style keyword literals. AS IF THIS IS NOT THE
;; BEST LISP DIALECT EVER
(def regression-map
     {"linear"      :Linear,
      "log"         :Logarithmic,
      "logarithmic" :Logarithmic,
      "exp"         :Exponential,
      "exponential" :Exponential,
      "best"        :Best})

(with-command-line *command-line-args*
  "interpolator -- interpolate and extrapolate values from a dataset"
  [[regression r "Use the given regression method to fit a least-squares line of best fit. Linear, logarithmic, and exponential regressions are available, as well as 'best' which will try to choose the correct regression."]
   [interpolate? l? "Fit an polynomial passing through all the given points, using Lagrange interpolation."] 
   [plot? p? "Plot the data set, fitted function, and interpolated points."]
   values]
  (binding [*make-plot* plot?]
    (cond
      interpolate? (with-input perform-interpolation)
      (contains? regression-map regression) (with-input 
					     perform-regression
					     (regression-map regression))
      :else (println 
	     "Please specify either 'interpolate' or a regression type")))
)
