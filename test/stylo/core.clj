(ns stylo.core
  (:require [clojure.test :refer [deftest is are]]
            [stylo.core :as stylo]))

(deftest class-generation
  (let [*styles (atom {})
        *media-styles (atom {})]
    (with-redefs [stylo/*styles *styles
                  stylo/*media-styles *media-styles]
     (are [created-class] (true? created-class)
       (c? [:text :blue-300] [:smartphone [:text :blue-500]])
       (c? [:smartphone [:text :blue-500] {:font-weight "500"}]
           [:screen [:text :pink-200] {:font-weight "300"}])
       (c? [:smartphone [:bg :red-500] [[:.some-arbitrary-class {:bg :blue-400}]]])
       (c? [:progress-bar [:bg :red-500]] {:font-weight "500"})
       (c? [:progress-bar [:bg :red-500]])
       (c? [:disabled [:hover [:bg :red-500]]])
       (c? [:bg :red-500] [[:.some-arbitrary-class {:bg :blue-400}]])
       (c? [:bg :red-500]
           [:hover [[:.some-arbitrary-class {:bg :blue-400}]]])
       (c? [:pseudo ":nth-child(2)" [:hover [:bg :red-500]]])
       (c? [[:& {:color "red"}]
            [:&:target {:color "green"}]])
       (c? {:color "red"})
       (c? [:hover [:placeholder :white] [:mb 1]])
       (c? [:p 1])
       (c? [:placeholder :white])
       (c? [:divide-x 2])
       (c? :sr-only)))))
