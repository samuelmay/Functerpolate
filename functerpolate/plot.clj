(ns
    #^{:author "Samuel May"
       :doc "A simple Clojure wrapper around the JOpenChart2 library"}
  functerpolate.plot
  (:use (clojure.contrib [def :only (defnk)])
	(functerpolate interpolate regression))
  (:import [java.awt           BorderLayout Dimension]
	   [javax.swing        JFrame JLabel]
	   [javax.swing.border EmptyBorder]
	   [com.approximatrix.charting        CoordSystem]
	   [com.approximatrix.charting.model  MultiScatterDataModel]
	   [com.approximatrix.charting.render MultiScatterChartRenderer]
	   [com.approximatrix.charting.swing  ChartPanel]))

;; WARNING: HEAVY JAVA INTEROP AHEAD

(defn function-plot-series 
  ([function a b step]
     (let [x (into (vector) (range a (+ b step) step))
	   y (into (vector) (map function x))]
       [x y]))
  ([function a b]
     ;; 100 ticks by default. We can't just use a constant like 0.01, because
     ;; for large ranges we get thousands of points and this freaks out the
     ;; java2d engine and exceptions are thrown.
     (function-plot-series function a b (/ (- b a) 100))))

(defnk add-data [model name x y :line false :markers true]
  (let [x (into-array Double/TYPE (map float x))
	y (into-array Double/TYPE (map float y))]
    (doto model
      (.addData x y name)
      ;; eh, by default let's turn them both on
      (.setSeriesLine name line)
      (.setSeriesMarker name markers))))

;; TODO: clean up. there's only one variable difference here and a lot of
;; duplication
(defn add-function
  ([model name function a b step]
     (let [[x y] (function-plot-series function a b step)]
       (add-data model name x y :line true :markers false)))
  ([model name function a b]
     (let [[x y] (function-plot-series function a b)] 
       (add-data model name x y :line true :markers false))))

;; Make the graph 'honest' (i.e., include the origin). Also sometimes the last
;; data point will be partially cut off, so manually set the biggest
;; values. Should be put last in the 'with-new-plot' macro body, otherwise your
;; other data won't be scaled properly!
(defn make-honest [model] 
  (doto model
    (.setAutoScale false)
    (.setMinimumValueX 0)
    (.setMinimumValueY 0)))

(defn plot-data-model [model title]
  (let [coords   (new CoordSystem model)
	renderer (new MultiScatterChartRenderer coords model)
	chart    (doto (new ChartPanel model title)
		   (.setCoordSystem coords)
		   (.addChartRenderer renderer 0))
	frame (new JFrame "Functerlate")] 
    (doto frame
      (.setSize 800 500)
      (.setResizable true)
      (.add chart)
      (.setVisible true))))

(defmacro with-new-plot [title & body]
  `(let [model# (new MultiScatterDataModel)]
     ;; The '->' macro is pretty crazy. Note that we will actually be using the
     ;; return value of each body form (i.e., the return value from add-function
     ;; or add-data) as the model input for the next body form. As 'doto'
     ;; returns the object it's working on, this should be ok.
     (-> model#
	 ~@body
	 (plot-data-model ~title))))

(defn interpolated-fn-plot [title x y]
  (let [interpolated-fn (lagrange-interpolation-fn x y)]
    (with-new-plot title
     (add-function "Interpolated function" interpolated-fn (first x) (last x))
     (add-data "Points" x y :line false))))

(defmulti plot-fitted-curve 
  (fn [model title x y] 
    (cond
      (keyword? model) :model
      (map? model) :regression)))

(defmethod plot-fitted-curve :regression
  [{:keys [name function formula r2 SS]} title x y]
  (let [caption (new JLabel (str "Function: " formula))
	plot (with-new-plot title
			    (add-data "Data" x y)
			    (add-function (str name " fitted curve") 
					  function 
					  (first x) (last x)))]
    ;; set the minimum size of the caption to prevent covering
    (. caption setMinimumSize (new Dimension 800 40))
    (. caption setBorder (new EmptyBorder 8 20 8 20))
    (doto plot
      (.add caption BorderLayout/PAGE_END)
      (.setSize 800 540))))

(defmethod plot-fitted-curve :model
  ([model title x y]
     (plot-fitted-curve (fit-model model x y) title x y)))
