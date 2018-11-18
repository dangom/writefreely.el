;;; write-as.el --- Push your Org files as markdown to write.as

;; Copyright (C) 2018 Daniel Gomez

;; Author: Daniel Gomez <d.gomez at posteo dot org>
;; Created: 2018-16-11
;; URL: https://github.com/dangom/write-as.el
;; Version: 0.01
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Copyright Notice:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Please see "Readme.org" for detailed introductions.
;; The API Documentation can be found here:
;; <https://developers.write.as/docs/api/>

;;; Code:

(require 'ox-md)
(require 'json)
(require 'request)


(defvar write-as-api-endpoint "https://write.as/api/posts"
  "URL of the write.as API endpoint")

(defvar write-as-request-default-header
  '(("Content-Type" . "application/json"))
  "Default request header")

(defvar write-as-auth-token nil
  "User authorization token.
  See https://developers.write.as/docs/api/ for instructions.")


(defun write-as-get-orgmode-keyword (key)
  "To get the #+TITLE of an org file, do
   (get-orgmode-keyword \"#+TITLE\")
  "
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (k)
      (when (string= key (org-element-property :key k))
        (org-element-property :value k)))
    nil t))


(defun write-as-get-post-url (post-id)
  (concat write-as-api-endpoint "/" post-id))


(defun write-as-post-link-for-visit (post-id)
  (concat "https://write.as/" post-id ".md"))


(defun write-as-generate-request-header ()
  "If a write-as-auth-token is available, then add
the authorization to the header."
  (if write-as-auth-token
      (cons `("Authorization" .
              ,(concat "Token " write-as-auth-token))
            write-as-request-default-header)
    write-as-request-default-header))


(defun write-as-org-to-md-string ()
  "Return the current Org buffer as a md string."
  (let ((org-buffer (current-buffer))
        (md-buffer (org-md-export-as-markdown)))
    (let ((md-string
           (with-current-buffer md-buffer
             (buffer-substring (point-min) (point-max)))))
      (set-buffer org-buffer)
      (kill-buffer md-buffer)
      md-string)))


(defun write-as-post-publish-request (title body)
  "Send POST request to the write.as API endpoint with title and body as data.
   Return parsed JSON response"
  (request-response-data
   (request
    write-as-api-endpoint
    :type "POST"
    :parser #'json-read
    :data (json-encode
           `(("title" . ,title)
             ("body" . ,body)))
    :headers (write-as-generate-request-header)
    :sync t
    :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                        (message "Got error: %S" error-thrown))))))


;; To update a post
(defun write-as-post-update-request (post-id post-token title body)
  "Send POST request to the write.as API endpoint with title and body as data.
   Message post successfully updated."
  (request
   (write-as-get-post-url post-id)
   :type "POST"
   :parser #'json-read
   :data (json-encode
          `(("title" . ,title)
            ("token" . ,post-token)
            ("body" . ,body)))
   :headers (write-as-generate-request-header)
   :success (function*
             (lambda (&key data &allow-other-keys)
               (message "Post successfully updated.")))
   :error (function*
           (lambda (&key error-thrown &allow-other-keys&rest _)
             (message "Got error: %S" error-thrown)))))


(defun write-as-publish-buffer ()
  "Publish the current Org buffer to write.as."
  (let* ((title (write-as-get-orgmode-keyword "TITLE"))
         (body (write-as-org-to-md-string))
         ;; POST the blogpost with title and body
         (response (write-as-post-publish-request title body))
         ;; Get the id and token from the response
         (post-id (assoc-default 'id (assoc 'data response)))
         (post-token (assoc-default 'token (assoc 'data response))))
    ;; Use setq-local as well because otherwise the local variables won't be
    ;; evaluated.
    (setq-local write-as-post-id post-id)
    (add-file-local-variable 'write-as-post-id post-id)
    (setq-local write-as-post-token post-token)
    (add-file-local-variable 'write-as-post-token post-token)))


;;;###autoload
(defun write-as-publish-or-update ()
  (interactive)
  (when (y-or-n-p "Do you really want to publish this file to write-as? ")
    (if (and (boundp 'write-as-post-id)
             (boundp 'write-as-post-token))
        (let ((title (write-as-get-orgmode-keyword "TITLE"))
              (body (write-as-org-to-md-string)))
          (write-as-post-update-request write-as-post-id
                                        write-as-post-token
                                        title
                                        body))
      (write-as-publish-buffer))))


;;;###autoload
(defun write-as-visit-post ()
  (interactive)
  (if (and (boundp 'write-as-post-id)
           (boundp 'write-as-post-token))
      (let ((browse-program
             (cond
              ((eq system-type 'darwin) "open")
              ((eq system-type 'linux) (executable-find "firefox")))))
        (shell-command
         (concat browse-program
                 " "
                 (write-as-post-link-for-visit write-as-post-id))))))


(provide 'write-as)
;;; write-as.el ends here
