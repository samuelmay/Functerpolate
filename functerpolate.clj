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

(def *make-plot* false)
(def *quiet* false)

;; extension of reduce to deal with sequences of sequences
(defn funky-reduce [f & colls]
  (apply f (map #(apply f %) 
		(filter (complement empty?) colls))))

(defn eval-unknown-values [function x unknowns]
  [(map function unknowns)
   (funky-reduce min x unknowns)
   (funky-reduce max x unknowns)])

;; perform-regression will check if unknowns is empty and adapt accordingly;
;; perform-interpolation won't, it assumes you wanted to interpretate at least a
;; few values. Of course, you might just want the plot, but in that corner case
;; I think you can deal with the pointless "Interpolated values:" printout (hey,
;; just run it with --quiet).
(defn perform-interpolation [x y unknowns]
  (let [function (lagrange-interpolation-fn x y)
	interpolated-values (map function unknowns)
	[interpolated-values a b] (eval-unknown-values function x unknowns)]
    (if *make-plot*
      ;; with-new-plot returns a 'plot' struct with a bunch of things in it,
      ;; including the JFrame the plot is embedded in.
      (let [plot (with-new-plot "Results Of Interpolation"
		   (add-function "Interpolated Function" function a b)
		   (add-data "Given Points" x y)
		   (add-data "Interpolated Points" unknowns 
			     interpolated-values))]
	(. (:frame plot) setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))
    (when (not *quiet*)
      (println "Interpolated values:"))
    (write-output unknowns interpolated-values)))

(defn perform-regression [x y unknowns model]
  (let [regression (fit-model model x y)
	{:keys [name function formula r2 SS]} regression
	[interpolated-values a b] (eval-unknown-values function x unknowns)]
    (if *make-plot*
      (let [plot (plot-fitted-curve regression "Results Of Regression" x y)]
	;; add in interpolated values if so desired
	(when (not (empty? unknowns))
	  (with-plot plot (add-data "Interpolated Points" unknowns
				    interpolated-values)))
	(. (:frame plot) setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)))
    (when (not *quiet*)
      (println "-- Results of " name "regression --")
      (println "Function:                     " formula)
      (println "Coefficient of detirmination: " r2)
      (println "Regression sum of squares:    " SS)
      (if (not (empty? unknowns)) (println "Interpolated values:")))
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
   [interpolate? i? "Fit an polynomial passing through all the given points, using Lagrange interpolation."] 
   [plot? p? "Plot the data set, fitted function, and interpolated points."]
   [quiet? q? "Just print out the interpolated data, nothing else thanks"]
   values]
  (binding [*make-plot* plot? *quiet* quiet?]
    (cond
      interpolate? (with-input perform-interpolation)
      (contains? regression-map regression) (with-input 
					     perform-regression
					     (regression-map regression))
      :else (println 
	     "Please specify either 'interpolate' or a regression type")))
)
