
(ns akvo.flow-services.appcode
  (:require [clojure.walk :refer (stringify-keys)]
    [clojure.java.jdbc :refer [db-do-commands create-table-ddl execute! query print-sql-exception]]
    [clojure.java.io :as io]
    [clojure.data.codec.base64 :as b64]
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
        [:expiration :integer]))
       (catch Exception e
         (errorf e "Error creating database %s" db-spec))))

(defn generate-code [instance expiration]
  (let [code (rand-int 1000000000)
        rows (first (execute! db-spec ["INSERT OR IGNORE INTO code(code, instance, expiration) VALUES(?,?,?)" code instance expiration]))]
    (if (= 1 rows)
      (str "" code)
      (generate-code instance expiration))))

(defn- get-instance [code]
  (:instance (first (query db-spec ["SELECT instance FROM code WHERE code = ?" code]))))

(defn appcode [code]
  (debugf (str "load-conf: " code))
  (let [instance (get-instance code)
        conf (get @config/configs instance)]
        (if conf
          (str (json/generate-string conf))
          (str "instance " code " not found"))))

(defn create-code [req]
  (if (authenticate req)
    (let [instance (get-in req [:params :instance]); TODO: would it make sense to return this var from the authentication function?
          expiration (get-in req [:params :expiration])]
      (generate-code instance expiration))
    "Unauthorized"))

(defn init []
  ; Initialise SQLite DB, if necessary
  (if-not (.exists (io/as-file (:subname db-spec)))
    (create-db)))
