;; -*- lexical-binding: t -*-
;;; writefreely.el --- Push your Org files as markdown to a writefreely instance

;; Copyright (C) 2018 Daniel Gomez

;; Author: Daniel Gomez <d.gomez at posteo dot org>
;; Created: 2018-16-11
;; URL: https://github.com/dangom/writefreely.el
;; Package-Requires: ((emacs "24.3") (org "9.0") (ox-gfm "0.0") (request "0.3"))
;; Version: 0.1.0
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

(require 'ox-gfm)
(require 'json)
(require 'request)


;;; User-Configurable Variables

(defgroup writefreely nil
  "Publish org-mode files to write.as"
  :tag "WriteFreely"
  :group 'convenience
  :version "24.3")

(defcustom writefreely-auth-token nil
  "User authorization token.
  See https://developers.write.as/docs/api/ for instructions."
  :type 'string)

(defcustom writefreely-always-confirm-submit t
  "When nil, ask for confirmation before submission."
  :type 'bool)

(defcustom writefreely-instance-url "https://write.as"
  "URL of the writefreely instance. You may
   change the endpoint in case your blog runs in a different
   writefreely instance."
  :type 'string)

(defcustom writefreely-instance-api-endpoint "https://write.as/api"
  "URL of the writefreely API endpoint. You may
   change the endpoint in case your blog runs in a different
   writefreely instance."
  :type 'string)


;;; Constants


(defconst writefreely-request-default-header
  '(("Content-Type" . "application/json"))
  "Default request header")


;;; Support Functions

(defun writefreely--api-get-post-url (post-id)
  (concat writefreely-instance-api-endpoint "/posts/" post-id))


;; from http://lists.gnu.org/archive/html/emacs-orgmode/2018-11/msg00134.html
(defun writefreely--get-orgmode-keyword (key)
  "To get the #+TITLE of an org file, do
   (writefreely-get-orgmode-keyword \"#+TITLE\")"
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (k)
      (when (string= key (org-element-property :key k))
	      (org-element-property :value k)))
    nil t))


(defun writefreely--generate-request-header ()
  "If a writefreely-auth-token is available, then add
the authorization to the header."
  (if writefreely-auth-token
      (cons `("Authorization" .
              ,(concat "Token " writefreely-auth-token))
            writefreely-request-default-header)
    writefreely-request-default-header))


(defun writefreely--org-as-md-string ()
  "Return the current Org buffer as a md string."
  (save-window-excursion
    (let* ((org-buffer (current-buffer))
           (md-buffer (org-gfm-export-as-markdown))
           (md-string
            (with-current-buffer md-buffer
              (buffer-substring-no-properties (point-min) (point-max)))))
      (set-buffer org-buffer)
      (kill-buffer md-buffer)
      md-string)))


(defun writefreely--get-user-collections ()
  "Retrieve a user writefreely collections"
  (if writefreely-auth-token
      (let ((response (request-response-data
                       (request
                        (concat writefreely-instance-api-endpoint "/me/collections")
                        :type "GET"
                        :parser #'json-read
                        :headers (writefreely--generate-request-header)
                        :sync t
                        :error (function*
                                (lambda (&key error-thrown &allow-other-keys&rest _)
                                  (message "Got error: %S" error-thrown)))))))
        (mapcar #'(lambda (x) (assoc-default 'alias x))
                (assoc-default 'data response)))
    (message "Cannot get user collections if not authenticated.")))


(defun writefreely--json-encode-data (title body &optional post-token)
  "Encode data as json for request."
  (let* ((alist `(("title" . ,title)
                  ("body" . ,body)))
         (token-alist (if post-token
                          (cons `("token" . ,post-token) alist)
                        alist)))
    (json-encode token-alist)))


(defun writefreely--update-org-buffer-locals (post-id post-token)
  "Setq-local and add-file-local variables for writefreely post"
  (setq-local writefreely-post-id post-id)
  (add-file-local-variable 'writefreely-post-id post-id)
  (setq-local writefreely-post-token post-token)
  (add-file-local-variable 'writefreely-post-token post-token))


(defun* writefreely--publish-success-fn (&key data &allow-other-keys)
  (message "Post successfully published."))


(defun* writefreely--update-success-fn (&key data &allow-other-keys)
  (message "Post successfully updated."))


(defun* writefreely--error-fn (&key error-thrown &allow-other-keys&rest _)
  (message "Got error: %S" error-thrown))


;;; Non-interactive functions

(defun writefreely-publication-link (post-id)
  "Return the publication link from a given post-id."
  (concat writefreely-instance-url "/" post-id ".md"))


(defun writefreely-post-publish-request (title body &optional collection)
  "Send POST request to the write.as API endpoint with title and body as data.
   Return parsed JSON response"
  (let ((endpoint
         (concat writefreely-instance-api-endpoint
                 (when collection (concat "/collections/" collection))
                 "/posts"))
        (data (writefreely--json-encode-data title body))
        (headers (writefreely--generate-request-header)))
    (request-response-data
     (request
      endpoint
      :type "POST"
      :parser #'json-read
      :data data
      :headers headers
      :sync t
      :success #'writefreely--publish-success-fn
      :error #'writefreely--error-fn))))


;; To update a post
(defun writefreely-post-update-request (post-id post-token title body)
  "Send POST request to the write.as API endpoint with title and body as data.
   Message post successfully updated.
   Note that this function does not return the response data, as in the
   case of writefreely-post-publish-request, as we already have the information
   we need, i.e., post-id and post-token."
  (let ((endpoint (writefreely--api-get-post-url post-id))
        (data (writefreely--json-encode-data title body post-token))
        (headers (writefreely--generate-request-header)))
    (request
     endpoint
     :type "POST"
     :parser #'json-read
     :data data
     :headers headers
     :success #'writefreely--update-success-fn
     :error #'writefreely--error-fn)))


(defun writefreely-publish-buffer (&optional collection)
  "Publish the current Org buffer to write.as."
  (let* ((title (writefreely--get-orgmode-keyword "TITLE"))
         (body (writefreely--org-as-md-string))
         ;; POST the blogpost with title and body
         (response (writefreely-post-publish-request title body collection))
         ;; Get the id and token from the response
         (post-id (assoc-default 'id (assoc 'data response)))
         (post-token (assoc-default 'token (assoc 'data response))))
    ;; Use setq-local as well because otherwise the local variables won't be
    ;; evaluated.
    (if post-id
        (writefreely--update-org-buffer-locals post-id post-token)
      (error "Post ID missing. Request probably went wrong."))))


;;; Interactive functions


;;;###autoload
(defun writefreely-publish-or-update ()
  "Publish or update Org file to write.as. This function
   will attempt to update the contents of a blog post if it finds
   a post-id and post-token local variables, otherwise it'll publish
   the file as a new post."
  (interactive)
  (when (or  writefreely-always-confirm-submit
             (y-or-n-p "Do you really want to publish this file to writefreely? "))
    (if (and (boundp 'writefreely-post-id)
             (boundp 'writefreely-post-token))
        (let ((title (writefreely--get-orgmode-keyword "TITLE"))
              (body (writefreely--org-as-md-string)))
          (writefreely-post-update-request writefreely-post-id
                                        writefreely-post-token
                                        title
                                        body))
      (if writefreely-auth-token
          (let* ((anonymous-collection "-- submit post anonymously --")
                 (collection
                  (completing-read "Submit post to which collection:"
                                   (cons
                                    anonymous-collection
                                    (writefreely--get-user-collections)))))
            (if (string-equal anonymous-collection collection)
                (writefreely-publish-buffer)
              (writefreely-publish-buffer collection)))
        (writefreely-publish-buffer)))))


;;;###autoload
(defun writefreely-visit-post ()
  "Open the current post on a webbrowser for viewing."
  (interactive)
  (if (and (boundp 'writefreely-post-id)
           (boundp 'writefreely-post-token))
      (let ((browse-program
             (cond
              ((eq system-type 'darwin) "open")
              ((eq system-type 'linux) (executable-find "firefox")))))
        (shell-command
         (concat browse-program
                 " "
                 (writefreely-publication-link writefreely-post-id))))))


(provide 'writefreely)

;;; writefreely.el ends here