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
	   ;; JFreeChart classes
	   [org.jfree.chart ChartFactory ChartPanel JFreeChart]
	    org.jfree.chart.axis.ValueAxis
	   [org.jfree.chart.plot XYPlot PlotOrientation]
	    org.jfree.chart.renderer.xy.XYItemRenderer
	   [org.jfree.data.xy XYSeries XYSeriesCollection]))

;; WARNING: HEAVY JAVA INTEROP AHEAD

;; Main representation of a plot, with all the fiddly jfreechart internals.
(defstruct plot :plot-obj :chart-obj :panel-obj :frame-obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions that operate on plots. By convention, they should:
;;   - all take a 'plot' struct as the *first* argument (so we can use the '->'
;;     macro later on in our main plot creation and modification macros).
;;   - all return the resulting plot, to retain some semblance of function
;;     programming. As the Java interop is not functional, you basically just
;;     return the plot argument you got given.

;; create an XYSeries, populate it and add it to the dataset. Then get the
;; XYPlot and renderer, and call setSeriesLinesVisible and
;; setSeriesShapesVisible
(defnk add-data [plot name x y :line false :markers true]
  ;; x and y must be the same length, and not empty. If this is false, don't add
  ;; anything to the plot. (In other words, silently fail. This is a feature,
  ;; not a bug)
  (if (and (== (count x) (count y)) (not (empty? y)))
    (let [series       (new XYSeries name)
	  dataset      (. (:plot-obj plot) getDataset)
	  series-index (do
			 (. dataset addSeries series)
			 (. dataset indexOf series))
	  renderer     (. (:plot-obj plot) getRenderer)]
      ;; add x and y to the new data series
      (loop [xi x yi y]
	(when (not (empty? xi))
	  (. series add (first xi) (first yi))
	  (recur (rest xi) (rest yi))))
      ;; update the renderer flags for this series 
      (. renderer setSeriesLinesVisible  series-index line) 
      (. renderer setSeriesShapesVisible series-index markers)))
  plot)

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
       :title (.  (:chart-obj plot) setTitle string)
       :xaxis (.. (:plot-obj plot)  getDomainAxis (setLabel string))
       :yaxis (.. (:plot-obj plot)  getRangeAxis  (setLabel string))) 
     plot)
  ([plot title]
     (cond 
       (map? title) (doseq [entry title]
		      (add-label plot (key entry) (val entry)))
       (string? title) (add-label plot :title title))
     plot))

(defn make-visible [plot]
  (.setVisible (:frame-obj plot) true) plot)

(defn update [plot]
  (.repaint (:panel-obj plot)) plot)

(defn exit-on-close [plot]
  (.setDefaultCloseOperation (:frame-obj plot) JFrame/EXIT_ON_CLOSE)
  plot)

(defn add-caption [plot string]
  (let [caption (new JLabel string)]
    (doto caption
      (.setMinimumSize (new Dimension 800 40))
      (.setBorder      (new EmptyBorder 8 20 8 20)))
    (doto (:frame-obj plot)
      (.add caption BorderLayout/PAGE_END)
      (.setSize 800 540))
    plot))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main interface macros, which provide a framework to use all the previous
;; functions.
(defmacro with-new-plot [& body]
  `(let [dataset-obj# (new XYSeriesCollection)
	 ;; have to use the dot special form manually here, as the namespace /
	 ;; syntax for static members doesn't seem to work when the macro is
	 ;; expanded in a different namespace.
	 chart-obj#   (. ChartFactory createXYLineChart 
		       "New Plot"
		       ;; no axis labels
		       nil nil
		       dataset-obj#
		       (. PlotOrientation VERTICAL)
		       true false false)
	 plot-obj#    (. chart-obj# getXYPlot)
	 panel-obj#   (new ChartPanel chart-obj#)
	 frame-obj#   (doto (new JFrame "Functerlate")
			(.setSize 800 500)
			(.setResizable true)
			(.add panel-obj#))
	 plot#        (struct plot 
			      plot-obj# chart-obj# panel-obj# frame-obj#)]
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
