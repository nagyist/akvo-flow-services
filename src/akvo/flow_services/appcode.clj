
(ns akvo.flow-services.appcode
  (:require [clojure.walk :refer (stringify-keys)]
    [clojure.java.jdbc :refer [db-do-commands create-table-ddl execute! query print-sql-exception]]
    [clojure.java.io :as io]
    [clojure.data.codec.base64 :as b64]
    [ring.util.response :refer (response charset content-type status)]
    [cheshire.core :as json]
    [akvo.flow-services [config :as config]]
    [taoensso.timbre :as timbre :refer (debugf errorf)])
  (:import javax.crypto.spec.SecretKeySpec
           javax.crypto.Mac
           java.sql.SQLException))

(defonce auth false); Temporary var to bypass authentication. We need to figure out which auth mechanism we'll use first.

(defonce db-path ".")
(defonce db-name "appcodes.sqlite")
(defonce db-spec
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname  (str db-path "/" db-name)
   :db-name  db-name})

(defn json-response [data]
  (->
    data
    json/generate-string
    response
    (content-type "application/json")
    (charset "UTF-8")))

(defn error-response [status-code msg]
  (->
    {:error msg}
    json-response
    (status status-code)))

(defn generate-hmac [message secret]
  (let [mac (Mac/getInstance "HmacSHA1")
        secret-key (SecretKeySpec. (.getBytes secret) (.getAlgorithm mac))]
    (.init mac secret-key)
    (.doFinal mac (.getBytes message))))

(defn validate-auth [authorization method date uri instance]
  (let [msg (str method "\n" date "\n" uri)
        secret (get-in @config/configs [instance :apiKey])
        hmac (String. (b64/encode (generate-hmac msg secret)) "UTF-8")]
    (debugf (str "Authorization: " authorization))
    (debugf (str "HMAC: " hmac))
    true))

(defn authenticate [req]
  (if auth
    (let [authorization (get-in req [:headers "authorization"])
          date (get-in req [:headers "date"])
          uri (:uri req)
          method "POST"
          instance (get-in req [:params :instance])]
      (if (and authorization uri method date instance)
        (validate-auth authorization method date uri instance)
        false)))
    true)

(defn- create-db []
  (debugf "Creating db %s" db-spec)
  (try
    (db-do-commands db-spec
      (create-table-ddl :code
        [:code :integer "PRIMARY KEY"]
        [:instance :text]; TODO: Add an index in this column
        [:name :text]))
       (catch Exception e
         (errorf e "Error creating database %s" db-spec))))

(defn generate-code [instance name]
  (let [code (rand-int 1000000000)
        res (first (execute! db-spec ["INSERT OR IGNORE INTO code(code, instance, name) VALUES(?,?,?)" code instance name]))]
    (if (= 1 res)
      code
      (generate-code instance name))))

(defn appconfig [code]
  (let [row (first (query db-spec ["SELECT instance FROM code WHERE code = ?" code]))
        instance (:instance row)
        conf (get @config/configs instance)]
    (if conf
      (json-response (assoc conf
                            :serverBase (str "https://" (:appId conf) ".appspot.com" )))
      (error-response 404 "not found"))))

(defn get-code [code]
  (let [row (first (query db-spec ["SELECT * FROM code WHERE code = ?" code]))]
    (if row
      (json-response row)
      (error-response 404 "code not found"))))

(defn create-code [params]
  (let [{instance :instance name :name} params
        found (get @config/configs instance)]
    (if found
      (get-code (generate-code instance name))
      (error-response 400 "invalid appId"))))

(defn list-codes [params]
  (let [{instance :instance} params
        rows (query db-spec ["SELECT code, name FROM code WHERE instance = ?" instance])]
    (json-response rows)))

(defn delete-code [code]
  (let [rows (first (execute! db-spec ["DELETE FROM code WHERE code = ?" code]))]
    (if (pos? rows)
      (json-response {})
      (error-response 404 "code not found"))))

(defn init []
  ; Initialise SQLite DB, if necessary
  (if-not (.exists (io/as-file (:subname db-spec)))
    (create-db)))
