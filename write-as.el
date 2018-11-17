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


(defun write-as-get-orgmode-keyword (key)
  "To get the #+TITLE of an org file, do
   (get-orgmode-keyword \"#+TITLE\")
  "
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (k)
      (when (string= key (org-element-property :key k))
	      (org-element-property :value k)))
    nil t))


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

;; To post as a user:
;; "Authorization: Token 00000000-0000-0000-0000-000000000000"
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
    :headers '(("Content-Type" . "application/json"))
    :sync t
    :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                        (message "Got error: %S" error-thrown))))))


;;;###autoload
(defun write-as-publish-buffer ()
  "Publish the current Org buffer to write.as."
  (interactive)
  (when (y-or-n-p "Do you really want to publish this file to write-as? ")
    (let* ((title (write-as-get-orgmode-keyword "TITLE"))
           (body (write-as-org-to-md-string))
           ;; POST the blogpost with title and body
           (response (write-as-post-publish-request title body))
           ;; Get the id and token from the response
           (post-id (assoc-default 'id (assoc 'data response)))
           (post-token (assoc-default 'token (assoc 'data response))))
      (add-file-local-variable 'write-as-post-id post-id)
      (add-file-local-variable 'write-as-post-token post-token))))

(provide 'write-as)

;;; write-as.el ends here
