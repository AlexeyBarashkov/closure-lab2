(ns barashkov.lab2.core
  (:import (java.net UnknownHostException))
  (:require
    [clj-http.client :as client]
    [net.cgrand.enlive-html :as html]))


(defn fetch-url [url]
  (try (client/get url {:throw-exceptions false})
       (catch UnknownHostException e {:status 404})
       (catch Exception e {:status 500})))


(defn prn-tree
  [tree]
  (println (:url tree) " - status: " (:status tree))
  (let [children @(:children tree)]
    (if (not (= (count children) 0))
      (doseq [node children] (if (not (= node nil)) (prn-tree node))))))



(defn get-urls-from-body [body]
  (if (not (= body nil))
    (let [as (html/select body #{[:a]})
          hrefs (map (fn [a] (:href (:attrs a))) as)]
      (filter (fn [href]
                (and (not (clojure.string/blank? href)) (.startsWith href "http://")))
              hrefs)
      )
    []))


(defn get-body [content]
  (if (= (:status content) 200)
    (html/html-snippet (:body content))
    nil))


(defn create-node [urls depth parent status url]
  {:urls urls :depth depth :parent parent :children (atom []) :status status :url url})


(defn exec-node [node]
  (if (> (:depth node) 0)
    (doseq [node (pmap
                   (fn [url]
                     (let [content (fetch-url url)
                           body (get-body content)
                           urls (get-urls-from-body body)
                           child (create-node urls (dec (:depth node)) node (:status content) url)]
                       (swap! (:children node) conj child)
                       child))
                   (:urls node))] (exec-node node))
    node ))


(defn create-root [urls depth]
  (create-node urls depth nil "" ""))


(defn get-tree
  [urls depth]
  (let [root (create-root urls depth)]
    (exec-node root)
    root))


(defn get-urls [path]
  (line-seq (clojure.java.io/reader path)))


(defn -main [path depth]
  {:pre [(.exists (clojure.java.io/as-file path))]}
  (let [urls (get-urls path)
        depth-number (Integer/parseInt depth)
        tree (get-tree urls depth-number)]
    (println "========== Results: =========")
    (prn-tree tree)))