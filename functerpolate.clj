(use '(functerpolate interpolate 
		     regression
		     plot)
     '(clojure.contrib command-line))

(defn parse-numbers [arg]
  (cond (string? arg)
	(map #(Double/parseDouble %) 
	     ;; Aaah regular expressions. Larry Wall is *not* allowed to talk
	     ;; about Lisp being ugly.
	     (re-seq #"[-+]?[0-9]+(?:\.[0-9]+)?" arg))
	(coll? arg)
	(apply concat (map parse-numbers arg))))

(defn read-input []
  (loop [input [() () ()] next-line (read-line)]
    (if (nil? next-line)
      (let [[x y unknowns] input]
	[(into [] (reverse x))
	 (into [] (reverse y))
	 (into [] (reverse unknowns))])
      (let [data (parse-numbers next-line)
	    [x y unknowns] input]
	(recur (if (== (count data) 1)
		 [x y (cons (first data) unknowns)]
		 [(cons (first data) x) (cons (second data) y) unknowns]) 
	       (read-line))))))

(defn write-output [x y]
  (when (and (not (empty? x)) (not (empty? y)))
    (println (first x) (first y))
    (recur (rest x) (rest y))))

(defmacro with-input [values [function & args]]
  `(let [[x# y# unknowns#] (read-input)]
     (~function x# y# (concat (parse-numbers ~values) unknowns#) ~@args)))

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
      (with-new-plot
       (add-label  "Results Of Interpolation")
       (add-function "Interpolated Function" function a b)
       (add-data "Given Points" x y)
       (add-data "Interpolated Points" unknowns interpolated-values)
       (exit-on-close)))
    (when (not (or *quiet* (empty? unknowns)))
      (println "Interpolated values:"))
    (write-output unknowns interpolated-values)))

(defn perform-regression [x y unknowns model]
  (let [regression (fit-model model x y)
	{:keys [rmodel name function formula r2 SS]} regression
	[interpolated-values a b] (eval-unknown-values function x unknowns)]
    (when *make-plot*
      (with-new-plot
       (add-label    :title "Results Of Regression")
       (add-function (str name " fitted curve"))
       (add-data     "Given Data Points" x y)
       (add-data     "Interpolate Points" unknowns interpolated-values)
       (add-caption  (str "Formula" formula))
       (exit-on-close)))
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
  [[regression r "Use the given regression method to fit a least-squares
                          line of best fit. Linear, logarithmic, and exponential
                          regressions are available, as well as 'best' which 
                          will try to choose the best regression."]
   [interpolate? i? "Instead of performing a least-squares regression, fit
                          a polynomial that passes through all the given points
                          exactly, using Lagrange interpolation."] 
   [plot? p? "Plot the data set, fitted function, and interpolated
                          points."]
   [quiet? q? "Print out just the interpolated data, nothing else 
                          thanks."]
   values]
  (binding [*make-plot* plot? *quiet* quiet?]
    (cond
      interpolate? 
      (with-input values (perform-interpolation))

      (contains? regression-map regression) 
      (with-input values (perform-regression (regression-map regression)))

      :else (println 
	     "Unknown regression type.")))
)
