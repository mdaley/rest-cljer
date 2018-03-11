(ns rest-cljer.server
  (:require [ring.adapter.jetty :refer [run-jetty]]))

(def expectations (atom {}))

(defn handler [server-id]
  (fn [request]
    (let [expectations (get @expectations server-id)
          incoming-path (:uri request)]
      (println "HANDLER FOR SERVER" server-id)
      (println "HANDLER EXPECTATIONS" expectations)
      (println "REQUEST" request)
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body "Hello World"})))

(defn start-server
  [options]
  (let [server-id (str (java.util.UUID/randomUUID))]
    (println "STARTING SERVER WITH ID" server-id)
    (doto (run-jetty (handler server-id) (merge {:join? false
                                                 :port 8082}
                                                options))
      (.setAttribute "RestServerId" server-id))))

(defn stop-server
  [server]
  (let [server-id (.getAttribute server "RestServerId")]
    (println "STOPPING SERVER WITH ID" server-id)
    (swap! expectations #(dissoc % server-id))
    (.stop server)))

(defn add-expectations
  [server pairs]
  (println "ADDING EXPECTATIONS" pairs)
  (let [server-id (.getAttribute server "RestServerId")
        exp (->> (partition 2 pairs)
                 (map (fn [[req resp]] {:request req :response resp :info {}})))]
    (swap! expectations #(assoc % server-id exp))))

(defn verify
  [server]
  (println "VERIFYING"))
