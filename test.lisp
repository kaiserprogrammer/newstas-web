(in-package #:newstas-web)

(def-suite newstas-web)
(in-suite newstas-web)


(test simple-success-case
  (start *server*)
  (finishes
    (with-selenium-session (selenium:*selenium-session* "*firefox" (puri:parse-uri "http://localhost:8080"))
      (do-open "/")
      (do-wait-for-page-to-load 5000)
      (is (string= "Newstas" (do-get-title)))
      (is (do-is-text-present "Login") "Login not present")

      (do-type "name" "blub")
      (do-type "password" "secret")
      (do-click "register")

      (do-wait-for-page-to-load 5000)
      (is (do-is-text-present "Notifications") "Welcome message not present")

      (do-type "url" "http://example.com")
      (let ((*data-retriever*
             (lambda (url)
               (declare (ignore url))
               "No Change")))
        (do-click "new_site"))
      (do-wait-for-page-to-load 5000)


      (is (do-is-text-present "Notifications") "Notifications not present")
      (is (not (do-is-text-present "http://example.com")) "example.com Site still in memory")

      (let ((*data-retriever*
             (lambda (url)
               (declare (ignore url))
               "Changed")))
        (check-site "http://example.com"))
      (do-refresh)
      (do-wait-for-page-to-load 5000)
      (is (do-is-text-present "http://example.com") "example.com Site was not added")
      (do-click "clear")
      (do-wait-for-page-to-load 5000)
      (is (not (do-is-text-present "http://example.com")) "example.com was not removed"))))
(test logging-in
  (start *server*)
  (finishes
    (with-selenium-session (selenium:*selenium-session* "*firefox" (puri:parse-uri "http://localhost:8080"))
      (do-open "/")
      (do-wait-for-page-to-load 5000)
      (is (do-is-text-present "Login") "Login not present")
      (do-type "name" "blub")
      (do-type "password" "secret")
      (do-click "login")
      (do-wait-for-page-to-load 5000)
      (is (do-is-text-present "Notifications") "Welcome message not present")

      (do-open "/")
      (do-wait-for-page-to-load 5000)
      (is (do-is-text-present "Notifications") "Logged in user not forwarded"))))

(let ((newstas:*db* (make-instance 'memory-db)))
  (run!))
