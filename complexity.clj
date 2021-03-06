(use '(functerpolate regression 
		     complexity
		     plot)
     '(clojure.contrib command-line))

(defn parse-number-list [lst]
  (map #(Integer/parseInt %) 
       (re-seq #"[-+]?[0-9]+(?:\.[0-9]+)?" lst)))

(with-command-line *command-line-args*
  "Attempt to detirmine the time complexity vs input size for a given program.
Usage: complexity.clj [options] test-program input-generator

The required arguments are:
 - an executable program to test
 - a second program that, given a number as an argument, will generate 
   appropriate input of that size for the first program.
"
  [[start s "starting input size" "100"]
   [limit l "final input size" "1000"]
   [points p "number of data points to collect" "10"]
   [runs r "how many times to run the program for each data point" "10"]
   [plot? p? "if given, create a graphical plot of the results" false]
   [extrapolate e "a list of bigger input sizes to extrapolate for"]
   rest-of-args]
  (let [[program input-generator & dont-care] rest-of-args
	numerical-opts (interleave [:start :limit :npoints :runs]
				   (map #(Integer/parseInt %)
					(list start limit points runs)))
	analysis (apply complexity
			program
			input-generator 
			:plot plot?
			;; nil counts as an empty collection, which will be
			;; handled properly by the function
			:extrapolate (parse-number-list extrapolate)
			numerical-opts)]
    (if plot?
      (with-plot (:plot analysis) (exit-on-close)))))
