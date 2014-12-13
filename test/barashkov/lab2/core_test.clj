(ns barashkov.lab2.core-test
  (:require [clojure.test :refer :all]
            [barashkov.lab2.core :refer :all]
            [net.cgrand.enlive-html :as html]))

(deftest parse-html
  (testing "Parse html page correctly."
    (let [content (html/html-snippet (slurp "fake-page.html"))
          body (:body content)
          urls (get-urls-from-body body)]
      (is (= (count urls) 4)))))

(deftest page-not-found
  (testing "Page not found"
    (let [server-respone (fetch-url "http://alskdjflaskjdf.asfaslfdjalskdf")]
      (is (= (:status server-respone) 404)))))
