(ns akvo.flow-services.releases
  (:require [cheshire.core :as json]
    [clj-http.client :as http]
    [markdown.core :as md]
    [hiccup.page :as h]))

(def releases-uri "https://api.github.com/repos/akvo/%s/releases")

(defn get-releases
  "For each repo in the sequence make a HTTP request to get the releases"
  [repos]
  (reduce-kv
    (fn [m k v]
      (assoc m v (json/parse-string (:body (http/get (format releases-uri v)))))) {} repos))

(defn convert-links
  "Convert the [#number] to proper issue links"
  [repo s]
  (s/replace s #"#(\d+)" (format "[#$1](https://github.com/akvo/%s/issues/$1)" repo)))

(defn get-content
  "Extract the content from the begining to the next H2 tag"
  [body]
  (let [idx (.indexOf body "##" 2)
        ndx (.indexOf body "\n")]
    (if (neg? idx)
      body
      (.substring body ndx idx))))

(comment
  
  (spit (java.io.File. "/tmp/test.html")
  (h/html5
    [:head
     (h/include-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.0/css/bootstrap.min.css")]
   [:div
    (for [r (get x "akvo-flow") :let [body (get r "body")] :when (re-find #"(?i)noteworthy" body)]
     [:div
      [:div [:h2 (get r "name")]]
      [:div
      (md/md-to-html-string (convert-links "akvo-flow" (get-content (get r "body"))))]])]))
  
  )
