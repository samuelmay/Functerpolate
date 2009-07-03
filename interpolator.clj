(use 'functerpolate.interpolate
     '(clojure.contrib command-line))

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

(def *make-plot*)

(defn interpolate 
  ([interpolator]
     )
  ([interpolator plotter]
     (let [[xseries yseries unknowns] (read-input)
	   function (interpolator xseries yseries)]
       (doseq [unknown unknowns]
	 (println unknown (function unknown))))))

(with-command-line *command-line-args*
  "interpolator -- interpolate and extrapolate values from a dataset"
  [[lagrange? g? "Use lagrange interpolation to find a polynomial through the given points."]
   [linear? l? "Find the line of best fit for the given points. Outputs the equation of the line as well as interpolated values."] 
   [plot? p? "Plot the data set, interpolated function and points."]]
  (cond
    lagrange? (interpolate lagrange-interpolation-fn)
    linear? (interpolate linear-regression-fn)
    :else (println "Please specify either 'linear' or 'lagrange' an interpolation method"))
)
