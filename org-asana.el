;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'json)
(require 'url)

(defvar org-asana/me nil
  "Information about the user.")

(defun org-asana/data (object)
  "Access the data key in OBJECT."
  (alist-get 'data object))

(cl-defun org-asana/request (endpoint &key (method "GET"))
  (with-current-buffer
      (let ((url-request-method method)
            (url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " org-asana/token)))))
        (url-retrieve-synchronously (concat "https://app.asana.com/api/1.0" endpoint)))
   (goto-char (1+ url-http-end-of-headers))
   (org-asana/data (json-read-object))))

(defun org-asana/about-me ()
  (org-asana/request "/users/me"))

(defun org-asana/set-me! ()
  "Set value of org-asana/me if not set."
  (or org-asana/me
     (setq org-asana/me (org-asana/about-me))))

(cl-defmacro org-asana/with-me (&body body)
  "Bind anaphoric value `me' in BODY."
  `(let ((me (alist-get 'data (org-asana/set-me!))))
     ,@body))

(defun org-asana/my-workspaces ()
  "Get all workspaces associated with org-asana/me."
  (org-asana/with-me
   (alist-get 'workspaces me)))

(defun org-asana/my-tasks (workspace)
  "Fetch tasks for `org-asana/me' in WORKSPACE."
  (org-asana/with-me
   (let* ((assignee (alist-get 'gid me))
          (workspace-id (alist-get 'gid workspace))
          (endpoint (concat "/workspaces/" workspace-id "/tasks/search"))
          (query-string (url-build-query-string `(("assignee.any" ,assignee)))))
     (org-asana/request (concat endpoint "?" query-string)))))

(defun org-asana/subtasks (task)
  "Fetch subtasks of TASK."
  (let-alist task
    (org-asana/request (concat "/tasks/" .gid "/subtasks"))))

