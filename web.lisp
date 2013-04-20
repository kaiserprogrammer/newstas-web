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

(defparameter *db* (make-instance 'memory-db))

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
