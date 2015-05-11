;  Copyright (C) 2015 Stichting Akvo (Akvo Foundation)
;
;  This file is part of Akvo FLOW.
;
;  Akvo FLOW is free software: you can redistribute it and modify it under the terms of
;  the GNU Affero General Public License (AGPL) as published by the Free Software Foundation,
;  either version 3 of the License or any later version.
;
;  Akvo FLOW is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;  See the GNU Affero General Public License included below for more details.
;
;  The full license text can also be seen at <http://www.gnu.org/licenses/agpl.html>.

(ns akvo.flow-services.auth
  (:require [clojure.string :as str]
    [akvo.flow-services [config :as config]]
    [taoensso.timbre :as timbre :refer (debugf errorf)]
    [ring.util.response :refer (response status)])
  (:import [com.nimbusds.jwt SignedJWT]
           [com.nimbusds.jose.jwk RSAKey]
           [com.nimbusds.jose.crypto RSASSAVerifier]))

(def rsa (RSAKey/parse (slurp "keycloak.cert")))

(defn validate-token [token]
  (debugf (str "Validating token: " token))
  (let [jwt (SignedJWT/parse token)
        verifier (RSASSAVerifier. (.toRSAPublicKey rsa))]
    (.verify jwt verifier)))

(defn authorized? [req]
  (let [auth-header (get-in req [:headers "authorization"])
        token (if (and (not (str/blank? auth-header))
                       (.startsWith auth-header "Bearer "))
                (.substring auth-header 7)
                nil)]
    (if token
      (validate-token token)
      false)))

(defn wrap-auth [handler]
  (fn [req]
    (debugf (str req))
    (if (authorized? req)
      (handler req)
      (-> (response "Access Denied")
          (status 403)))))
