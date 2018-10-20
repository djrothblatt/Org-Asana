;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'dash)
(require 'let-alist)

(defvar org-asana/me nil
  "Information about the user.")

(defvar org-asana/workspaces nil
  "The user's workspaces.")

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

(defun org-asana/set-workspaces! ()
  "Set value of org-asana/workspaces if not set."
  (or org-asana/workspaces
     (setq org-asana/workspaces (org-asana/my-workspaces))))

(cl-defmacro org-asana/with-workspaces (&body body)
  `(let ((workspaces (org-asana/set-workspaces!)))
     ,@body))

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
  "Embed TASK's subtasks under a `subtask' key."
  (acons 'subtasks
         (mapcar #'org-asana/task-tree
                 (org-asana/subtasks task))
         task))

(defun org-asana/task-forest (tasks)
  "Get task trees for every task in TASKS."
  (mapcar #'org-asana/task-tree tasks))

(defun org-asana/workspace-tasks (workspaces)
  "Get task trees for every task in WORKSPACES."
  (mapcan (-compose #'org-asana/task-forest #'org-asana/root-tasks)
          workspaces))

(cl-defun org-asana/org-task-tree (task &optional (level 1))
  "Convert TASK (potentially a tree) into a (possibly nested) Org element."
  (let-alist task
    (let ((props (list :title .name :todo-keyword "TODO" :level level))
          (subtasks (mapcar (-partial (-flip #'org-asana/org-task-tree)
                                      (1+ level))
                            .subtasks)))
      (apply (-partial #'org-element-create
                       'headline
                       props)
             subtasks))))

(defun org-asana/workspace-org-task-forest (workspace)
  "Get task forest as Org from tasks in WORKSPACE."
  (mapcar #'org-asana/org-task-tree (org-asana/workspace-tasks (list workspace))))

(defun org-asana/org-task-data (org-task-forest)
  "Wrap ORG-TASK-FOREST in a data element."
  (apply (-partial #'org-element-create
                   'data
                   nil)
         org-task-forest))

(defun org-asana/workspace-name-org-task-data (workspace-name)
  "Get org tasks from workspace going by WORKSPACE-NAME."
  (org-asana/with-workspaces
   (let ((workspace (find-if (lambda (x) (string-equal (alist-get 'name x) workspace-name))
                             workspaces)))
     (org-element-interpret-data
      (org-asana/org-task-data
       (org-asana/workspace-org-task-forest workspace))))))

(defun org-asana/pull-tasks (workspace-name)
  "Pull tasks for WORKSPACE-NAME from Asana and write as Org."
  (interactive (list
                (org-asana/with-workspaces
                 (completing-read "Workspace: "
                                  (mapcar (-partial #'alist-get 'name)
                                          workspaces)))))
  (insert
   (org-asana/workspace-name-org-task-data workspace-name)))
