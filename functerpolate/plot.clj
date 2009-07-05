(ns
    #^{:author "Samuel May"
       :doc "A simple Clojure wrapper around the JOpenChart2 library"}
  functerpolate.plot
  (:use (clojure.contrib [def :only (defnk)]
			 [fcase :only (case)])
	(functerpolate interpolate regression))
  (:import [java.awt           BorderLayout Dimension]
	   [javax.swing        JFrame JLabel]
	   [javax.swing.border EmptyBorder]
	   [com.approximatrix.charting        CoordSystem Title]
	   [com.approximatrix.charting.model  MultiScatterDataModel]
	   [com.approximatrix.charting.render MultiScatterChartRenderer]
	   [com.approximatrix.charting.swing  ChartPanel]))

;; WARNING: HEAVY JAVA INTEROP AHEAD

;; I've tried to make this not *too* reliable on openchart2, but obviously I
;; won't really know until I try to port it to, say, plplot.

;; Main representation of a plot, with all the fiddly openchart2 internals.
(defstruct plot :model :coords :renderer :chart :frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions that operate on plots. By convention, they should:
;;   - all take a 'plot' struct as the *first* argument (so we can use the '->'
;;     macro later on in our main plot creation and modification macros).
;;   - all return the resulting plot, to retain some semblance of function
;;     programming. As the Java interop is not functional, you basically just
;;     return the plot argument you got given.

(defnk add-data [plot name x y :line false :markers true]
  ;; x and y must be the same length, and not empty. If this is false, don't add
  ;; anything to the plot. (In other words, silently fail. This is a feature,
  ;; not a bug)
  (do (when (and (== (count x) (count y))
		 (not (empty? y)))
	(let [x (into-array Double/TYPE (map float x))
	      y (into-array Double/TYPE (map float y))]
	  (doto (:model plot)
	    (.addData x y name)
	    ;; eh, by default let's turn them both on
	    (.setSeriesLine name line)
	    (.setSeriesMarker name markers))))
      plot))

;; helper for 'add-function'. It may be of general use, so I won't make it
;; private/inline.
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

(defn add-function [plot name function a b & step]
  ;; the optional-args list is a bit overkill here, all we really have is one
  ;; optional 'step' parameter. 'apply' should still work correctly if the
  ;; optional-args list is nill.
  (let [[x y] (apply function-plot-series function a b step)]
    (add-data plot name x y :line true :markers false)))

;; takes either: a label position keyword and string
;;           or: a map of label positions to strings
;;           or: a title string
(defn add-label
  ([plot position string]
     (case position
       :title (.. (:chart plot) getTitle (setText string))
       :xaxis (.setXAxisUnit (:coords plot) string)
       :yaxis (.setYAxisUnit (:coords plot) string)) 
     plot)
  ([plot title]
     (cond 
       (map? title) (doseq [entry title]
		      (add-label plot (key entry) (val entry)))
       (string? title) (add-label plot :title title))
     plot))

;; Make the graph 'honest' (i.e., include the origin). Also sometimes the last
;; data point will be partially cut off, so manually set the biggest
;; values. Should be put last in the 'with-new-plot' macro body, otherwise your
;; other data won't be scaled properly!
(defn make-honest [plot] 
  (doto (:model plot)
    (.setAutoScale false)
    (.setMinimumValueX 0)
    (.setMinimumValueY 0))
  plot)

(defn make-visible [plot]
  (.setVisible (:frame plot) true) plot)

(defn update [plot]
  (.repaint (:chart plot)) plot)

(defn exit-on-close [plot]
  (.setDefaultCloseOperation (:frame plot) JFrame/EXIT_ON_CLOSE)
  plot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main interface macros, which provide a framework to use all the previous
;; functions.
(defmacro with-new-plot [& body]
  `(let [model#    (new MultiScatterDataModel)
	 coords#   (new CoordSystem model#)
	 renderer# (new MultiScatterChartRenderer coords# model#)
	 chart#    (doto (new ChartPanel model# "New Plot")
		     (.setCoordSystem coords#)
		     (.addChartRenderer renderer# 0))
	 frame#    (doto (new JFrame "Functerlate")
		     (.setSize 800 500)
		     (.setResizable true)
		     (.add chart#))
	 plot#     (struct plot model# coords# renderer# chart# frame#)]
     ;; The '->' macro is pretty crazy. Note that we will actually be using the
     ;; return value of each body form (i.e., the return value from add-function
     ;; or add-data) as the model input for the next body form. As 'doto'
     ;; returns the object it's working on, this should be ok.
     (-> plot#
	 ~@body
	 (make-visible))))

(defmacro with-plot [plot & body]
  ;; takes a 'plot' struct, apply plot functions, and update ui
  `(-> ~plot ~@body (update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now we get to specific types of plotting.
(defmulti plot-fitted-curve 
  (fn [model title x y] 
    (cond
      (keyword? model) :model
      (map? model) :regression)))

(defmethod plot-fitted-curve :regression
  [{:keys [model name function formula r2 SS]} title x y]
  (let [caption (new JLabel (str "Function: " formula))
	plot (with-new-plot
	      (add-label title)
	      (add-data "Data" x y)
	      (add-function (str name " fitted curve") 
			    function 
			    (first x) (last x)))]
    ;; set the minimum size of the caption to prevent covering
    (. caption setMinimumSize (new Dimension 800 40))
    (. caption setBorder (new EmptyBorder 8 20 8 20))
    (doto (:frame plot)
      (.add caption BorderLayout/PAGE_END)
      (.setSize 800 540))
    plot))

(defmethod plot-fitted-curve :model
  ([model title x y]
     (plot-fitted-curve (fit-model model x y) title x y)))
