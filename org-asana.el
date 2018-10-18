(require 'json)
(require 'url)

(cl-defun org-asana/request (endpoint &key (method "GET"))
  (with-current-buffer
      (let ((url-request-method method)
            (url-request-extra-headers
            `(("Authorization" . ,(concat "Bearer " org-asana/token)))))
        (url-retrieve-synchronously (concat "https://app.asana.com/api/1.0" endpoint)))
   (goto-char (1+ url-http-end-of-headers))
   (json-read-object)))

(defun org-asana/me ()
  (org-asana/request "/users/me"))
