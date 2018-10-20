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
   (seq-into (org-asana/data (json-read-object)) 'list)))

(defun org-asana/about-me ()
  (org-asana/request "/users/me"))

(defun org-asana/set-me! ()
  "Set value of org-asana/me if not set."
  (or org-asana/me
     (setq org-asana/me (org-asana/about-me))))

(cl-defmacro org-asana/with-me (&body body)
  "Bind anaphoric value `me' in BODY."
  `(let ((me (org-asana/set-me!)))
     ,@body))

(defun org-asana/my-workspaces ()
  "Get all workspaces associated with org-asana/me."
  (org-asana/with-me
   (seq-into
    (alist-get 'workspaces org-asana/me)
    'list)))

(defun org-asana/root-tasks (workspace)
  "Fetch tasks that aren't subtasks for `org-asana/me' in WORKSPACE."
  (org-asana/with-me
   (let* ((assignee (alist-get 'gid me))
          (workspace-id (alist-get 'gid workspace))
          (endpoint (concat "/workspaces/" workspace-id "/tasks/search"))
          (query-string (url-build-query-string `(("assignee.any" ,assignee) ("is_subtask" "false")))))
     (org-asana/request (concat endpoint "?" query-string)))))

(defun org-asana/subtasks (task)
  "Fetch subtasks of TASK."
  (let-alist task
    (org-asana/request (concat "/tasks/" .gid "/subtasks"))))

(defun org-asana/task-tree (task)
      (acons 'subtasks
             (mapcar #'org-asana/task-tree
                     (org-asana/subtasks task))
             task))

(defun org-asana/task-forest (tasks)
  "Get task trees for every task in TASKS."
  (mapcar #'org-asana/task-tree tasks))

