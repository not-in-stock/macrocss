(ns components.core
  (:require [app.pages :refer [pages all-pages]]
            [stylo.core :refer [c]]
            [clojure.string :as str]
            [routing.core :refer [routes]]
            [components.hiccup :refer [href]]
            [components.store :refer [components]]
            [re-frame.core :as rf]))

(defn contains-item? [k item]
  (->> pages
       k
       (some #{item})))

(defn clicked? [bool]
  (if bool
    (c [:text :blue-600] [:pseudo :hover [:text :blue-300]])
    (c [:pseudo :hover [:text :blue-300]])))

(defn menu-item
  ([page k]
   (menu-item page k (-> k
                         name
                         str/capitalize)))
  ([page k description]
   [:li
    [:a
     {:class (clicked? k)
      :href (-> k
                routes
                href)
      :on-click (dispatch-doc-click k)}
     description]]))

(defn create-menu [page items]
  (apply conj [:ul]
         (mapv (partial menu-item page) items)))

(defn default-menu [page]
  (let [default-menu-items (:default pages)]
    (create-menu page default-menu-items)))

(defn extended-menu
  [page]
  (create-menu page all-pages))

(defn render-default [k]
  (k components))

(defn render-doc [k]
  (or (render-default k)
      (:installation components)))

(defn render-page [c]
  (cond (default-menu-item? c) (render-default c)
        (documentation-item? c) (render-doc c)))

(defn render-menu [c m]
  (if-not m
    (default-menu c)
    (extended-menu c)))
