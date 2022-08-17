(ns stylo.util
  (:refer-clojure :exclude [format])
  (:require [garden.color :refer [hex->rgb]]
            [garden.units :as units]
            #?(:cljs [goog.string :as gstring])
            #?(:cljs [goog.string.format])
            #?(:clj [garden.def]))
  #?(:cljs (:require-macros [garden.def])))

(defn format
  "Crossplatform text format function"
  [format-str & args]
  #?(:clj (apply clojure.core/format format-str args)
     :cljs (apply gstring/format format-str args)))

(def ratio-regex
  #"(-?\d+)/(\d+)")

(defn with-alpha
  "Converts CSS hex color to RGB value format also adding an alpha value and making it depending on CSS variable"
  [color variable]
  (if-let [{:keys [red green blue]} (hex->rgb color)]
    (format "rgba(%d,%d,%d,var(%s))"
            red green blue (name variable))
    color))

(defn str-ratio?
  "Checks if a string has ratio format like `-22/2`, `11/8`, etc."
  [s]
  (and (string? s)
       (re-matches ratio-regex s)))

(defn parse-str-ratio
  "Cross platform function for parsing ratios form string."
  [s]
  (let [[_ n d] (re-matches ratio-regex s)]
    #?(:clj (/ (Double/parseDouble n) (Double/parseDouble d))
       :cljs (/ (js/parseFloat n) (js/parseFloat d)))))

(defn as-unit
  "Converts a value to a preffered Garden units
  - Units are converted to units
  `(units/px 20) => 20px`
  - Ratio strings are converted to percent
   `\"3/4\" => \"75%\"`
  - Other strings are converted to themselves
   `\"20px\" => \"20px\"`
  - Numbers are converted to units defined with `preferred-unit` -
  units are adjusted to grid size if `:rem` is specified"
  ([v]
   (as-unit v :rem))
  ([v preferred-unit]
   (cond
     (units/unit? v) v
     (str-ratio? v) (units/percent (* 100 (parse-str-ratio v)))
     (string? v) v
     :else (case preferred-unit
             :ms (units/ms v)
             :px (units/px v)
             :deg (units/deg v)
             :rem (units/rem (* v 0.25))
             :percent (units/percent v)))))

(comment
  (garden.compiler/render-css (units/percent 42))
  (garden.compiler/render-css (as-unit "3/4")))
