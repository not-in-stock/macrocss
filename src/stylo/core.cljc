(ns stylo.core
  (:require [clojure.string :as str]
            [garden.core]
            [garden.stylesheet]
            [stylo.rule :as rule]
            [stylo.tailwind.accessibility]
            [stylo.tailwind.background]
            [stylo.tailwind.border]
            [stylo.tailwind.effect]
            [stylo.tailwind.flex]
            [stylo.tailwind.grid]
            [stylo.tailwind.interactivity]
            [stylo.tailwind.layout]
            [stylo.tailwind.preflight]
            [stylo.tailwind.sizing]
            [stylo.tailwind.spacing]
            [stylo.tailwind.svg]
            [stylo.tailwind.table]
            [stylo.tailwind.transform]
            [stylo.tailwind.transition]
            [stylo.tailwind.typography]
            [stylo.tailwind.variant]
            [stylo.util :as u])
  #?(:cljs (:require-macros [stylo.core])))

(defonce
  ^{:doc "An atom which holds CSS styles for generated classes in a Garden format."}
  *styles
  (atom {}))

(defonce
  ^{:doc "An atom which holds CSS media rules for generated classes in a Garden format."}
  *media-styles
  (atom {}))

(defonce *media
  ^{:doc "An atom which contains predefined media rules for different screen sizes."}
  (atom {:screen {:screen true}
         :smartphone {:max-width "415px"}
         :ereader {:max-width "481px"}
         :p-tablets {:max-width "768px"}
         :l-tablets {:max-width "1025px"}
         :desktop {:min-width "1200px"}}))

(defn- garden-readable
  [media-rules]
  (reduce (fn [acc [f s :as r]]
            (if (string? f)
              (conj acc [(keyword f) (second s)])
              (conj acc r)))
          [] media-rules))

(defn- media-query
  [media-specs class-name rules]
  (garden.stylesheet/at-media
   media-specs
   [[class-name (-> rules
                    rule/join-rules
                    garden-readable)]]))

(defn- defmediarules
  [media]
  (doseq [[k v] media]
    (defmethod rule/rule k [_ & rules]
      (fn [class-name]
        (media-query v class-name rules)))))

(defmediarules @*media)

(defn set-own-mediarules!
  [rules]
  (reset! *media {})
  (swap! merge *media rules)
  (defmediarules @*media)
  @*media)

(defn extend-media-rules!
  [rules]
  (swap! merge *media rules)
  (defmediarules @*media)
  @*media)

(defn- media-rule?
  [k]
  (when (keyword? k)
    (-> @*media
        keys
        (->> (apply hash-set)
             k))))

(defn- create-located-classname
  "Creates a class name string based on the namespace, line and column
  in the source code based on a macro env bindings.
  If bindings unavailable `nil` is returned."
  [env]
  (when-let [ns-name (get-in env [:ns :name])]
    (u/format ".%s-%s-%s"
              (str/replace ns-name #"\." "_")
              (:line env)
              (:column env))))

(defn- create-hashed-classname
  "Creates a class name string based on a hashcode of supplied `rules`"
  [rules]
  (str ".c" (hash rules)))

(defn- create-classname
  [env rules]
  (keyword (or (create-located-classname env)
               (create-hashed-classname rules))))

(defn- divide-rules
  "Divides rules and media query rules.
  Returns a map with keys `:rules` and `media-rules`"
  [rules]
  (reduce
   (fn [acc rule]
     (if (and (sequential? rule)
              (-> rule first media-rule?))
       (update acc :media-rules conj rule)
       (update acc :rules conj rule)))
   {:rules []
    :media-rules []} rules))

(defn- inject-media-rules
  [class-name garden-obj]
  (swap! *media-styles dissoc class-name)
  (swap! *media-styles assoc-in [class-name
                                 (-> garden-obj
                                     :value
                                     :media-queries)]
         garden-obj))

(defn- create-media-rules
  [class-name media-rules]
  (if-not (empty? media-rules)
    (->> media-rules
         (mapv (partial apply rule/rule))
         (mapv (fn [f]
                 (f class-name)))
         (mapv (fn [g]
                 (inject-media-rules class-name g))))
    (swap! *media-styles dissoc class-name)))

(defn- rules-with-location
  [env rules]
  (with-meta (rule/join-rules rules)
    {:location [(:name (:ns env))
                (:line env)
                (:column env)]}))

(defn- create-rules [env rules]
  (when-not (empty? rules)
    (let [class-name (create-classname env rules)]
      (swap! *styles dissoc class-name)
      (swap! *styles assoc class-name (rules-with-location env rules))
      class-name)))

(defn- normalize-classname
  "Removes `.` form the begining of class name keyword
  `:.class-name` `:class-name`"
  [classname]
  (-> classname
      name
      (subs 1)
      keyword))

(defn- c-fn
  [env rs]
  (let [{:keys [media-rules rules]} (divide-rules rs)
        class-name (or (create-rules env rules)
                       (create-classname env media-rules))
        _ (create-media-rules class-name media-rules)]
    (normalize-classname class-name)))

(defmacro c
  "A macro that generates a class based on `rules`
  and returns a name of a generated class or location in code and properties."
  [& rules]
  (c-fn &env rules))

(defmacro c-eco
  "Uses only hashed version of classname. Is recomended for release purposes, because it minimizes resulting CSS file."
  [& rules]
  (c-fn nil rules))

(defmacro c? [& rs]
  (let [{:keys [rules media-rules]} (divide-rules rs)
        class-name (if-not (empty? rules)
                     (create-classname &env rules)
                     (create-classname &env media-rules))

        compute-rules (fn [r]
                        (->> r
                             rule/join-rules
                             (into [class-name])
                             garden.core/css
                             boolean))
        compute-media-rules (fn [m]
                              (->> m
                                   (mapv (partial apply rule/rule))
                                   (mapv (fn [f] (f class-name)))
                                   garden.core/css
                                   boolean))]
    (cond
      (and (empty? media-rules)
           (empty? rules))
      true

      (empty? media-rules)
      (compute-rules rules)

      (empty? rules)
      (compute-media-rules media-rules)

      :else
      (and (compute-rules rules)
           (compute-media-rules media-rules)))))

(defn- join-lines
  [s]
  (str/replace s #"\n" ""))

(defn- collapse-adjacent-spaces
  [s]
  (str/replace s #"\s{2,}" " "))

(defn- format-style-definitions
  [css-string]
  (str/replace css-string #"\{|\}|\@"
               {"}" "}\n"
                "{" "{\n"
                "@" "\n\n@"}))

(defn- prettify [css-string]
  (-> css-string
      join-lines
      collapse-adjacent-spaces
      format-style-definitions
      str/join))

(defn- css-media-styles
  [media-styles]
  (->> media-styles
       vals
       (mapcat vals)
       garden.core/css
       prettify))

(defn- css-rules
  [styles]
  (garden.core/css
   (concat stylo.tailwind.preflight/preflight
           (->> styles
                (sort-by (comp :location meta))
                (mapv (fn [[k v]]
                        (into [k] v)))))))

(defn compile-styles
  [styles media-styles]
  (str (css-rules styles)
       (css-media-styles media-styles)))
