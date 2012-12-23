(defpackage #:newstas-web
  (:use #:cl #:newstas #:stampede #:silcro #:lisperati #:fiveam #:selenium)
  (:shadow :s-get :s-post))

(in-package :newstas-web)

(defmacro s-unsecure-get (spec &body body)
  `(silcro:s-get ,spec
     ,@body))

(defmacro s-unsecure-post (spec &body body)
  `(silcro:s-post ,spec
     ,@body))

(defmacro s-get (spec &body body)
  `(silcro:s-get ,spec
     (if (get-session "id")
         (progn ,@body)
         (redirect-to "/"))))

(defmacro s-post (spec &body body)
  `(silcro:s-post ,spec
     (if (get-session "id")
         (progn ,@body)
         (redirect-to "/"))))

(defvar *server* (make-http-server 8080))

(defparameter *db* (make-instance 'durable-db))

(defvar *title* "Newstas")
(defvar *name*)
(defvar *notes*)

(defrenderer-with-inner-template
    #.(relative-file "templates/application.html.lr")
  #.(relative-file "templates")
  :match "\\.lr$")

(s-unsecure-get (*server* "/")
  (if (get-session "id")
      (redirect-to (concatenate 'string "/" (get-session "id")))
      (render-root-index)))

(s-get (*server* "/user/logout")
  (set-session "id" nil)
  (redirect-to "/"))

(s-unsecure-post (*server* "/login")
  (cond ((param "register")
         (progn (add-user (param "name") (param "password"))
                (set-session "id" (param "name"))
                (redirect-to (concatenate 'string "/" (param "name")))))
        ((param "login")
         (if (verify-user (param "name") (param "password"))
             (progn (set-session "id" (param "name"))
                    (redirect-to (concatenate 'string "/" (param "name"))))
             (redirect-to "/")))
        (t (redirect-to "/"))))

(s-post (*server* "/user/clear")
  (clear-notification (get-session "id") (param "url"))
  (redirect-to (concatenate 'string "/" (get-session "id"))))

(s-get (*server* "/:id")
  (when (string= (get-session "id") (param :id))
    (let* ((*name* (param :id))
           (*notes* (get-notifications *name*)))
      (render-user-show))))

(s-post (*server* "/site")
  (add-site (get-session "id") (param "url"))
  (redirect-to (concatenate 'string "/" (get-session "id"))))

(def-suite newstas-web)
(in-suite newstas-web)

(let ((*db* (make-instance 'memory-db)))
  (start *server*)
  (test simple-success-case
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

  (run!))
