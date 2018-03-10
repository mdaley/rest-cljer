(ns rest-cljer.test.core
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [environ.core :refer [env]]
            [clojure.test :refer :all]
            [rest-cljer.core :refer [rest-driven string-capture]])
  (:import [com.github.restdriver.clientdriver ClientDriver ClientDriverRequest$Method]
           [com.github.restdriver.clientdriver.exception ClientDriverFailedExpectationException]
           [java.net SocketTimeoutException]))

(def free-port
  "Returns a constant free port"
  (memoize
   (fn []
     (let [port (ClientDriver/getFreePort)]
       ;; Usually it's enough to simply define the rest-driver port in your project
       ;; file (or make it available to environ in some way - e.g. environment variable
       ;; system property, project.clj)
       ;;
       ;; From there you can simply define your rest dependencies as on that port
       ;;
       ;; However in this case I don't want to assume that anyone has a particular port
       ;; free on their machine so asking the OS for a free port, then forcing this into
       ;; env using alter-var-root. The tests below then build up their URIs to point
       ;; to the same port on localhost
       ;;
       ;; If you don't want to use environ or any of the functions it provides you could
       ;; make use of this approach in your project
       (alter-var-root (var env) assoc :restdriver-port port)
       port))))

(defn local-path
  "Returns a URI to a resource on a free port, on localhost with the supplied postfix"
  [postfix]
  (str "http://localhost:" (free-port) postfix))

(deftest rest-driven-call-succeeds-with-exceptions
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path}
                  {:status 204}]
      (is (= 204 (:status (http/post url)))))))

(deftest rest-driven-call-with-binary-body-succeeds-without-exceptions
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)
        bytes (byte-array [(byte 10) (byte 20) (byte 30)])]
    (rest-driven [{:method :POST
                   :url resource-path}
                  {:status 200 :body bytes}]
      (is (= (seq bytes) (-> (http/post url) :body (.getBytes) seq))))))

(deftest unexpected-rest-driven-call-should-fail-with-exception
  (let [url (local-path "/")]
    (is (thrown? RuntimeException (rest-driven [] (http/post url))))))

(deftest test-json-document-matching
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path
                   :body {:ping "pong"}}
                  {:status 204}]
      (is (= 204 (:status (http/post url {:content-type :json
                                          :body (json/generate-string {:ping "pong"})
                                          :throw-exceptions false})))))))

(deftest check-body-via-predicate
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path
                   :body #(apply = (map sort [[3 2 1] %]))}
                  {:status 204}]
      (is (= 204) (:status (http/post url {:content-type :json
                                           :body (json/generate-string [1 3 2])
                                           :throw-exceptions false}))))))

(deftest check-body-via-predicate-with-type-as-string
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path
                   :body [#(= "Hi" %) "application/text"]}
                  {:status 204}]
      (is (= 204) (:status (http/post url {:content-type "application/text"
                                           :body "Hi"
                                           :throw-exceptions false}))))))

(deftest check-body-via-predicate-order-independent
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path
                   :body [#(= "Hi" %) "application/text"]}
                  {:status 204}
                  {:method :POST
                   :url resource-path
                   :body [#(= "not-hi" %) "application/text"]}
                  {:status 204}]
      (http/post url {:content-type "application/text"
                      :body "not-hi"
                      :throw-exceptions false})
      (is (= 204 (:status (http/post url {:content-type "application/text"
                                          :body "Hi"
                                          :throw-exceptions false})))))))

(deftest json-document-capture-as-a-string
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)
        capturer (string-capture)]
    (rest-driven [{:method :POST
                   :url resource-path
                   :body {:ping "pong"}
                   :capture capturer}
                  {:status 204}]
      (is (= 204 (:status (http/post url {:content-type :json
                                          :body (json/generate-string {:ping "pong"})
                                          :throw-exceptions false}))))
      (is (= "{\"ping\":\"pong\"}" (capturer))))))

(deftest sweetening-of-response-definitions
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :GET
                   :url resource-path}
                  {:body {:inigo "montoya"}}]
      (let [{:keys [status body headers]} (http/get url)]
        (is (= 200 status))
        (is (= "application/json" (get headers "Content-Type")))
        (is (= {:inigo "montoya"} (json/parse-string body true)))))))

(deftest sweetening-of-response-does-not-override-explicit-http-status
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :GET
                   :url resource-path}
                  {:status 400
                   :body {:inigo "montoya"}}]
      (let [{:keys [status body headers]} (http/get url {:throw-exceptions false})]
        (is (= 400 status))
        (is (= "application/json" (get headers "Content-Type")))
        (is (= {:inigo "montoya"} (json/parse-string body true)))))))

(deftest post-processing-of-request-and-response-replacing-initial-values-with-new-ones-using-the-:an-function
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :GET
                   :url resource-path
                   :and #(.withMethod % ClientDriverRequest$Method/POST)}
                  {:status 204 :and #(.withStatus % 205)}]
      (is (= 205 (:status (http/post url)))))))

(deftest response-can-be-repeated-any-times
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :PUT
                   :url resource-path}
                  {:status 204 :times :any}]
      (dotimes [n 3]
        (is (= 204 (:status (http/put url))))))))

(deftest response-can-be-repeated-a-specific-number-of-times
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path}
                  {:status 200 :times 2}]
      (dotimes [n 2]
        (is (= 200 (:status (http/post url))))))
    (is (thrown? Exception (rest-driven [{:method :POST :url resource-path}
                                         {:status 200 :times 2}]
                             (dotimes [n 2]
                               (is (= 200 (:status (http/post url)))))
                             (http/post url))))))

(deftest rest-driven-call-with-expected-header-succeeds
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path
                   :headers {"from" "mytest"
                             "with" "value"}}
                  {:status 204}]
      (is (= 204 (:status (http/post url {:headers {"from" "mytest"
                                                    "with" "value"}})))))))

(deftest rest-driven-call-with-missing-header-throws-exception
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (is (thrown? Exception (rest-driven [{:method :POST
                                          :url resource-path
                                          :headers {"From" "origin"}}
                                         {:status 204}]
                             (http/post url))))))

(deftest rest-driven-call-without-header-that-is-expected-to-be-absent-succeeds
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path
                   :not {:headers {"myheader" "myvalue"}}}
                  {:status 204}]
      (is (= 204 (:status (http/post url)))))))

(deftest rest-driven-call-with-header-that-is-expected-to-be-absent-throw-exception
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (is (thrown? RuntimeException (rest-driven [{:method :POST
                                                 :url resource-path
                                                 :not {:headers {"myheader" "myvalue"}}}
                                                {:status 204}]
                                    (http/post url {:headers {"myheader" "myvalue"}}))))))

(deftest rest-driven-call-with-vector-of-headers-that-are-expected-to-be-absent-succeeds
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path
                   :not {:headers ["myheader"]}}
                  {:status 204}]
      (is (= 204 (:status (http/post url)))))))

(deftest rest-driven-call-with-vector-of-headers-that-are-expected-to-be-absent-throw-exception
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (is (thrown? RuntimeException (rest-driven [{:method :POST
                                                 :url resource-path
                                                 :not {:headers ["myheader"]}}
                                                {:status 204}]
                                    (http/post url {:headers {"myheader" "myvalue"}}))))))

(deftest rest-driven-call-with-response-headers-succeeds
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :POST
                   :url resource-path}
                  {:status 204
                   :headers {"from" "rest-cljer", "with" "value"}}]
      (let [{:keys [status headers]} (http/post url)]
        (is (= 204 status))
        (is (= "rest-cljer" (get headers "From")))
        (is (= "value" (get headers "with")))))))

(deftest can-supply-params-using-keywords-as-well-as-strings
  (let [resource-path "/some/resource/path"
        url (local-path (str resource-path "?a=a&b=b"))]
    (rest-driven [{:method :GET
                   :url resource-path
                   :params {:a "a" "b" "b"}}
                  {:status 200}]
      (is (= 200 (:status (http/get url)))))))

(deftest can-specify-:any-params
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method :GET
                   :url resource-path
                   :params :any}
                  {:status 200}]
      (is (= 200 (:status (http/get url)))))))

(deftest request-method-is-:GET-by-default
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:url resource-path}
                  {:status 200}]
      (is (= 200 (:status (http/get url)))))))

(deftest request-response-can-be-paired-as-a-vector
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [[{:url resource-path} {:status 201}]
                  [{:url resource-path} {:status 202}]]
      (is (= 201 (:status (http/get url))))
      (is (= 202 (:status (http/get url)))))))

(deftest expected-rest-driven-call-with-an-unusual-http-method-succeeds
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:method "PATCH"
                   :url resource-path}
                  {:status 204}]
      (is (= 204 (:status (http/patch url)))))))

(deftest expected-rest-driven-call-times-out-if-after-is-to-larger-than-socket-timeout
  (let [resource-path "/some/resource/path"
        url (local-path resource-path)]
    (rest-driven [{:url resource-path}
                  {:status 200
                   :after 200}]
      (is (thrown? SocketTimeoutException (http/get url {:socket-timeout 100}))))))
