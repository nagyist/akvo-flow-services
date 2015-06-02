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

(ns akvo.flow-services.authorization
  (:require [ring.util.response :refer (response status)])
  (:import com.nimbusds.jwt.SignedJWT))

(defn get-roles [token]
  (if-let [jwt (if token (SignedJWT/parse token) nil)]
    (set (get-in 
           (-> jwt .getJWTClaimsSet (.getClaim "resource_access"))
           ["akvoflow" "roles"]))))

(defn authorized? [req]
  (let [appId (get-in req [:params :appId])
        token (:jwt req)
        roles (get-roles token)]
    (contains? roles appId)))

(defn wrap-authorization [handler]
  (fn [req]
    (if (authorized? req)
      (handler req)
      (-> (response "Non authorized")
          (status 403)))))
