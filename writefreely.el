;;; writefreely.el --- Push your Org files as markdown to a writefreely instance -*- lexical-binding: t -*-

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
(require 'cl)


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

(defcustom writefreely-maybe-publish-created-date nil
  "When t, if #+DATE is found then use it as post creation date."
  :type 'bool)

(defcustom writefreely-instance-url "https://write.as"
  "URL of the writefreely instance.
You may change the endpoint in case your blog runs in a different
writefreely instance."
  :type 'string)

(defcustom writefreely-instance-api-endpoint "https://write.as/api"
  "URL of the writefreely API endpoint.
You may change the endpoint in case your blog runs in a different
writefreely instance."
  :type 'string)

(defcustom writefreely-posting-type 'single
  "Treat the single file as a post, or each L1 heading as a post?"
  :type '(choice (const :tag "Post the entire file" single)
		 (const :tag "Each L1 heading is a new post" multiple)))

(defcustom writefreely-property-instance-url "WRITEFREELY-INSTANCE"
  "Property name to record the instance the post was posted to."
  :type 'string)

(defcustom writefreely-property-post-id "WRITEFREELY-POST-ID"
  "Property name to record the post ID."
  :type 'string)

(defcustom writefreely-property-post-token "WRITEFREELY-POST-TOKEN"
  "Property name to record the post token."
  :type 'string)



;;; Constants


(defconst writefreely-request-default-header
  '(("Content-Type" . "application/json"))
  "Default request header.")

(defconst writefreely-org-ref-bibliography-entry-format
  '(("article" . "%a, %t, <i>%j</i>, <b>%v(%n)</b>, %p (%y). [doi](http://dx.doi.org/%D)")
    ("book" . "%a, %t, %u (%y).")
    ("techreport" . "%a, %t, %i, %u (%y).")
    ("proceedings" . "%e, %t in %S, %u (%y).")
    ("inproceedings" . "%a, %t, %p, in %b, edited by %e, %u (%y)"))
  "Have ox-gfm output reference links compatible with writefreely's Markdown.")


;;; Support Functions

(defun writefreely--api-get-post-url (post-id)
  "Get api endpoint for a given POST-ID."
  (concat writefreely-instance-api-endpoint "/posts/" post-id))


;; from http://lists.gnu.org/archive/html/emacs-orgmode/2018-11/msg00134.html
(defun writefreely--get-orgmode-keyword (key)
  "Return value of a orgmode keyword KEY."
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (k)
      (when (string= key (org-element-property :key k))
	      (org-element-property :value k)))
    nil t))


(defun writefreely--generate-request-header ()
  "Return request header with authorization token, if available.
Otherwise default header."
  (if writefreely-auth-token
      (cons `("Authorization" .
              ,(concat "Token " writefreely-auth-token))
            writefreely-request-default-header)
    writefreely-request-default-header))


(defun writefreely--find-enclosing-l1-headline ()
  "Return the l1 headline element containing point."
  (save-excursion
    (org-up-heading-safe)
    (when (not (eq (org-element-type (org-element-at-point)) 'headline))
      (user-error "Point must be within a headline tree"))
    (while (> (org-element-property :level (org-element-at-point)) 1)
      (org-up-heading-safe))
    (org-element-at-point)))


(defun writefreely--org-as-md-string ()
  "Return the current Org buffer as a md string."
  (save-window-excursion
    (let* ((org-buffer (current-buffer))
           (md-buffer
            ;; Do not let bibliography links be converted to HTML.
            (let ((org-ref-bibliography-entry-format
                   writefreely-org-ref-bibliography-entry-format))
	      (when (eq writefreely-posting-type 'multiple)
		(let ((headline (writefreely--find-enclosing-l1-headline)))
		    (goto-char (org-element-property :begin headline))
		    (org-narrow-to-subtree)))
              (org-gfm-export-as-markdown)))
           (md-string
            (with-current-buffer md-buffer
	      (if (eq writefreely-posting-type 'multiple)
		  ;; First line is also the heading, which is included
		  ;; as the post title, so skip over it.
		  (forward-line 1)
		(goto-char (point-min)))
              (buffer-substring-no-properties (point) (point-max)))))
      (set-buffer org-buffer)
      (kill-buffer md-buffer)
      (when (eq writefreely-posting-type 'multiple)
	(widen))
      md-string)))


(defun writefreely--formatted-date-from-keyword ()
  "Return an ISO8601 canonical date from an Org #+DATE keyword."
  (let ((post-creation-date (writefreely--get-orgmode-keyword "DATE")))
    (when post-creation-date
      (format-time-string
       "%FT%TZ"
      (apply #'encode-time
             (org-parse-time-string post-creation-date))
      t))))


(defun writefreely--get-user-collections ()
  "Retrieve a user's writefreely collections."
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
  "Encode TITLE and BODY string data as json for request.
If POST-TOKEN, encode it as well."
  (let* ((alist `(("title" . ,title)
                  ("body" . ,body)))
         (language (writefreely--get-orgmode-keyword "LANGUAGE"))
         (created-date (writefreely--formatted-date-from-keyword)))
    (when post-token
      (setq alist (append alist `(("token" . ,post-token)))))
    (when language
      (setq alist (append alist `(("lang" . ,language)))))
    (when (and writefreely-maybe-publish-created-date
               created-date)
      (setq alist (append alist `(("created" . ,created-date)))))
    (encode-coding-string (json-encode alist) 'utf-8)))


(defun writefreely--remove-post-metadata ()
  "Remove metadata associated with this post."
  (if (eq writefreely-posting-type 'multiple)
      (writefreely--remove-post-properties)
    (writefreely--remove-org-buffer-locals)))


(defun writefreely--remove-post-properties ()
  "Remove properites for the post associated with the current l1 headline."
  (let* ((headline (writefreely--find-enclosing-l1-headline))
	 (pom (org-element-property :begin headline)))
    (org-entry-delete pom writefreely-property-post-id)
    (org-entry-delete pom writefreely-property-post-token)))


(defun writefreely--remove-org-buffer-locals ()
  "Setq-local and add-file-local variables for writefreely post."
  (makunbound 'writefreely-post-id)
  (makunbound 'writefreely-post-token)
  (delete-file-local-variable 'writefreely-post-id)
  (delete-file-local-variable 'writefreely-post-token))


(defun writefreely--update-post-metadata (post-id post-token)
  "Update POST-ID and POST-TOKEN metadata for this post."
  (if (eq writefreely-posting-type 'multiple)
      (writefreely--update-post-properties post-id post-token)
    (writefreely--update-org-buffer-locals post-id post-token)))


(defun writefreely--update-post-properties (post-id post-token)
  "Update POST-ID and POST-TOKEN properties for the post associated with
the current L1 headline."
  (let* ((headline (writefreely--find-enclosing-l1-headline))
	 (pom (org-element-property :begin headline)))
    (org-entry-put pom writefreely-property-post-id post-id)
    (org-entry-put pom writefreely-property-post-token post-token)))


(defun writefreely--update-org-buffer-locals (post-id post-token)
  "Setq-local and add-file-local variables POST-ID and POST-TOKEN for writefreely post."
  (setq-local writefreely-post-id post-id)
  (add-file-local-variable 'writefreely-post-id post-id)
  (setq-local writefreely-post-token post-token)
  (add-file-local-variable 'writefreely-post-token post-token))


(defun writefreely--post-exists ()
  "Check whether a buffer is a post, i.e., has both a post-id and a post-token."
  (if (eq writefreely-posting-type 'multiple)
      (and (org-entry-get (point) writefreely-property-post-id t)
	   (org-entry-get (point) writefreely-property-post-token t))
    (and (boundp 'writefreely-post-id)
	 (boundp 'writefreely-post-token))))

(defun* writefreely--publish-success-fn (&key data &allow-other-keys)
  "Callback to run upon successful request to publish post.
DATA is the request response data."
  (message "Post successfully published."))


(defun* writefreely--update-success-fn (&key data &allow-other-keys)
  "Callback to run upon successful request to update post.
DATA is the request response data."
  (let ((id (assoc-default 'id (assoc-default 'data data))))
    (if (or (string-equal id "spamspamspamspam")
	    (string-equal id "contentisblocked"))
        (message "Post rejected for being considered spam. Contact write.as")
      (message "Post successfully updated."))))


(defun* writefreely--delete-success-fn (&key data &allow-other-keys)
  "Callback to run upon successful deletion of post.
DATA is the request response data."
  (writefreely--remove-post-metadata)
  (message "Post successfully deleted."))


(defun* writefreely--error-fn (&key error-thrown &allow-other-keys)
  "Callback to run in case of error request response.
ERROR-THROWN is the request response data."
  (message "Got error: %S" error-thrown))


(defun writefreely--get-post-id ()
  "Return the post-id for the current file or headline."
  (if (eq writefreely-posting-type 'multiple)
      (org-entry-get (point) writefreely-property-post-id t)
    writefreely-post-id))


(defun writefreely--get-post-token ()
  "Return the post-token for the current file or headline."
  (if (eq writefreely-posting-type 'multiple)
      (org-entry-get (point) writefreely-property-post-token t)
    writefreely-post-token))


(defun writefreely--get-post-title ()
  "Return the title for the current file or headline."
  (if (eq writefreely-posting-type 'multiple)
      (org-element-property :title (writefreely--find-enclosing-l1-headline))
    (writefreely--get-orgmode-keyword "TITLE")))


;;; Non-interactive functions

(defun writefreely-publication-link (post-id)
  "Return the publication link from a given POST-ID."
  (concat writefreely-instance-url "/" post-id ".md"))


(defun writefreely-publish-request (title body &optional collection)
  "Send post request to the write.as API endpoint with TITLE and BODY as data.
Optionally, if COLLECTION is given, publish to it.  Returns request response"
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
(defun writefreely-update-request (post-id post-token title body)
  "Send POST request to the write.as API endpoint with TITLE and BODY as data.
Message post successfully updated.
   Note that this function does not return the response data, as in the
   case of ‘writefreely-publish-request’, as we already have the information
   we need, i.e., POST-ID and POST-TOKEN."
  (let ((endpoint (writefreely--api-get-post-url post-id))
        (data
         (let ((writefreely-maybe-publish-created-date nil))
           (writefreely--json-encode-data title body post-token)))
        (headers (writefreely--generate-request-header)))
    (request
     endpoint
     :type "POST"
     :parser #'json-read
     :data data
     :headers headers
     :success #'writefreely--update-success-fn
     :error #'writefreely--error-fn)))


(defun writefreely-delete-request (post-id post-token)
  "Send POST request to the write.as API endpoint with title and body as data.
Message post successfully updated.
   Note that this function does not return the response data, as in the
   case of ‘writefreely-publish-request’, as we already have the information
   we need, i.e., POST-ID and POST-TOKEN."
  (let ((endpoint (concat
                   (writefreely--api-get-post-url post-id)
                   "?token="
                   post-token))
        (headers (writefreely--generate-request-header)))
    (request
     endpoint
     :type "DELETE"
     :headers headers
     :parser #'json-read
     :status-code '((204 . writefreely--delete-success-fn))
     :error #'writefreely--error-fn)))


(defun writefreely-publish-buffer (&optional collection)
  "Publish the current Org buffer to write.as anonymously, or to COLLECTION, if given."
  (let* ((title (writefreely--get-post-title))
	 (tbody (writefreely--org-as-md-string))
         (body (if (string-empty-p tbody) "-" tbody))
         ;; POST the blogpost with title and body
         (response (writefreely-publish-request title body collection))
         ;; Get the id and token from the response
         (post-id (assoc-default 'id (assoc 'data response)))
         (post-token (assoc-default 'token (assoc 'data response))))
    ;; Use setq-local as well because otherwise the local variables won't be
    ;; evaluated.
    (if post-id
        (writefreely--update-post-metadata post-id post-token)
      (error "Post ID missing. Request probably went wrong"))))


;;; Interactive functions


;;;###autoload
(defun writefreely-publish-or-update ()
  "Publish or update Org file to write.as.
This function will attempt to update the contents of a blog post if it finds
   a post-id and post-token local variables, otherwise it'll publish
   the file as a new post."
  (interactive)
  ;; TODO: When writefreely-posting-type is 'multiple this should ask
  ;; if you want to publish the heading (and display the name of the
  ;; heading)
  (when (or writefreely-always-confirm-submit
            (y-or-n-p "Do you really want to publish this file to writefreely? "))
    (if (writefreely--post-exists)
	;; TODO: When writefreely-posting-type is multiple this should
	;; get the text of the enclosing heading.
        (let ((title (writefreely--get-post-title))
              (body (writefreely--org-as-md-string)))
	  (message "Post title is [%s]" title)
          (writefreely-update-request (writefreely--get-post-id)
                                      (writefreely--get-post-token)
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
(defun writefreely-delete-post ()
  "Delete current post and clear local variables."
  (interactive)
  (if (writefreely--post-exists)
      (writefreely-delete-request
       (writefreely--get-post-id) (writefreely--get-post-token))
    (message "Cannot delete non-existing post.")))


;;;###autoload
(defun writefreely-clear-file-info ()
  "Dissociate current file or L1 heading from a writefreely post."
  (interactive)
  (writefreely--remove-post-metadata))


;;;###autoload
(defun writefreely-visit-post ()
  "Open the current post on a webbrowser for viewing."
  (interactive)
  (if (writefreely--post-exists)
      (browse-url
       (writefreely-publication-link (writefreely--get-post-id)))))

(defvar writefreely-mode-map (make-sparse-keymap)
  "Keymap for writefreely mode.")

;;;###autoload
(define-minor-mode writefreely-mode
  "Minor mode to support published orgmode documents on a writefreely instance"
  :lighter " WriteFreely"
  :keymap writefreely-mode-map
  (define-key writefreely-mode-map (kbd "C-c C-w s") 'writefreely-publish-or-update)
  (define-key writefreely-mode-map (kbd "C-c C-w d") 'writefreely-delete-post)
  (define-key writefreely-mode-map (kbd "C-c C-w v") 'writefreely-visit-post)
  (define-key writefreely-mode-map (kbd "C-c C-w c") 'writefreely-clear-file-info)
  )


(provide 'writefreely)

;;; writefreely.el ends here
