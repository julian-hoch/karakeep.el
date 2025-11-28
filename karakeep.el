;;; karakeep.el --- Karakeep → Org integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 artawower

;; Author: artawower <artawower33@gmail.com>
;; URL: https://github.com/julian-hoch/karakeep.el
;; Package-Requires: ((emacs "27.1") (org "9.4") (request "0.3.0"))
;; Version: 0.1.0
;; Keywords: convenience, outlines, hyperlinks

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Karakeep → Org integration utilities and API client.
;; - url.el-based client with Bearer token.
;; - Fetch bookmarks by tags/collection with basic pagination (one page by default).
;; - Shared utilities for renderers and org integrations.

;;; Code:

(require 'request)
(require 'auth-source)
(require 'json)
(require 'seq)
(require 'subr-x)  ;; string-trim, string-empty-p, when-let, if-let
(require 'rx)
(require 'cl-lib)

(defgroup karakeep nil
  "Karakeep → Org integration."
  :group 'convenience)

(defcustom karakeep-debug nil
  "Enable debug messages for karakeep operations.
This variable is kept for backward compatibility; prefer
`karakeep-debug-enable'."
  :type 'boolean
  :group 'karakeep)

(defcustom karakeep-api-base "http://localhost:8000/api"
  "Base URL for the locally hosted Karakeep REST API."
  :type 'string
  :group 'karakeep)

(defcustom karakeep-auth-source-host "karakeep.local"
  "DEPRECATED: single host name used in auth-source lookup.
Prefer `karakeep-auth-source-hosts`. Still used for backward compatibility."
  :type 'string
  :group 'karakeep)

(defcustom karakeep-auth-source-hosts '("karakeep.local" "localhost")
  "Host names used in auth-source to look up the token.
Local Karakeep deployments often use localhost-based endpoints; both are tried in order."
  :type '(repeat string)
  :group 'karakeep)

(defcustom karakeep-token-source '(auth-source env custom)
  "Ordered sources to retrieve Karakeep token from.
Supported: `auth-source', `env' (KARAKEEP_TOKEN), and `custom'."
  :type '(repeat (choice (const auth-source) (const env) (const custom)))
  :group 'karakeep)

(defcustom karakeep-custom-token nil
  "Custom token value if `karakeep-token-source' includes `custom'.
Avoid storing secrets in plain files; prefer auth-source."
  :type '(choice (const :tag "Unset" nil) (string :tag "Token"))
  :group 'karakeep)

(defcustom karakeep-request-timeout 15
  "Timeout in seconds for API requests."
  :type 'integer
  :group 'karakeep)

(defcustom karakeep-default-limit 100
  "Default maximum number of items to fetch (single page up to 100)."
  :type 'integer
  :group 'karakeep)

(defcustom karakeep-default-collection 0
  "Default collection ID used for queries.
Per the Karakeep spec, collectionId=0 means “all collections”. For default
behavior we treat 0 as “all”. Set a concrete numeric ID to query a single
collection."
  :type 'integer
  :group 'karakeep)

(defcustom karakeep-debug-enable t
  "Enable verbose debug messages for HTTP requests.
Takes precedence over the deprecated `karakeep-debug'."
  :type 'boolean
  :group 'karakeep)

(defconst karakeep--user-agent
  "karakeep.el/0.1.0 (+https://github.com/julian-hoch/karakeep.el)"
  "User-Agent header for Karakeep requests.")

(defvar karakeep--collections-cache nil
  "Cached list of collections as returned by `/collections` (value of 'items).")

(defvar karakeep--tags-cache nil
  "Cached list of tags as returned by `/tags` (value of 'items).")

(defvar karakeep--tags-loading nil
  "Flag indicating if tags are currently being loaded.")

(defvar karakeep--tags-ready nil
  "Flag indicating if tags have been loaded and cached.")

;;;; Small helpers (pure where possible)

(defun karakeep--debug (fmt &rest args)
  "Log debug message if debug logging is enabled."
  (when (or karakeep-debug-enable karakeep-debug)
    (message "karakeep.el: %s" (apply #'format fmt args))))

(defun karakeep--mask (s)
  "Return masked version of secret string S for logs."
  (when (stringp s)
    (let ((n (length s)))
      (cond
       ((<= n 6) "******")
       (t (concat (substring s 0 3) (make-string (- n 6) ?*) (substring s (- n 3))))))))

(defun karakeep--get-token-from-auth-source ()
  "Retrieve token from auth-source using configured hosts."
  (let* ((hosts (append (and (listp karakeep-auth-source-hosts)
                             karakeep-auth-source-hosts)
                        (list karakeep-auth-source-host)))
         token)
    (catch 'got
      (dolist (h hosts)
        (let* ((found (car (auth-source-search :host h :max 1 :require '(:secret))))
               (secret (when found (plist-get found :secret))))
          (cond
           ((functionp secret) (setq token (funcall secret)))
           ((stringp secret) (setq token secret)))
          (when (and token (stringp token) (> (length token) 0))
            (throw 'got token))))
      nil)))

(defun karakeep--get-token ()
  "Resolve Karakeep token according to `karakeep-token-source'."
  (catch 'done
    (dolist (src karakeep-token-source)
      (pcase src
        ('auth-source
         (let ((tok (karakeep--get-token-from-auth-source)))
           (when (and tok (stringp tok) (> (length tok) 0))
             (throw 'done tok))))
        ('env
         (let ((tok (getenv "KARAKEEP_TOKEN")))
           (when (and tok (stringp tok) (> (length tok) 0))
             (throw 'done tok))))
        ('custom
         (when (and karakeep-custom-token
                    (stringp karakeep-custom-token)
                    (> (length karakeep-custom-token) 0))
           (throw 'done karakeep-custom-token)))))
    (user-error "karakeep.el: Token not found. Configure auth-source or KARAKEEP_TOKEN.")))

(defun karakeep-parse-tags (tags)
  "Normalize TAGS param to a list of strings.
Accepts list of symbols/strings or a string. The string form supports:
- Comma-separated values: tag1, tag2
- Quoted tokens to allow spaces: `\"file manager\"`, cli
- Whitespace-separated tokens when there are no commas.
Prefer quoting for tags containing spaces or commas."
  (cond
   ((null tags) nil)
   ((listp tags)
    (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                (mapcar (lambda (tag) (if (symbolp tag) (symbol-name tag) tag)) tags)))
   ((stringp tags)
    (let* ((s (string-trim tags))
           (has-comma (string-match-p "," s))
           (re (if has-comma
                   (rx (or (seq "\"" (group (+ (not (any "\"")))) "\"")
                           (+ (not (any ",\t\n\r ")))))
                 (rx (or (seq "\"" (group (+ (not (any "\"")))) "\"")
                         (+ (not (any "\t\n\r ")))))))
           (start 0)
           (out '()))
      (if (string-empty-p s)
          nil
        (while (and (< start (length s)) (string-match re s start))
          (let* ((q (match-string 1 s))
                 (m (match-string 0 s))
                 (tok (or q m)))
            (push (if has-comma
                      (string-trim tok "[, \t\n\r]+" "[, \t\n\r]+")
                    tok)
                  out))
          (setq start (match-end 0)))
        (nreverse out))))
   (t (list (format "%s" tags)))))

(defun karakeep-parse-tags-with-exclusion (tags)
  "Parse TAGS into included and excluded tags.
Supports '-tag' syntax for exclusion in string form.
Returns plist with :tags and :excluded-tags keys."
  (let ((parsed-tags (karakeep-parse-tags tags)))
    (if (not (stringp tags))
        ;; For non-string input, no exclusion syntax
        (list :tags parsed-tags :excluded-tags nil)
      ;; For string input, separate excluded tags
      (let ((included '())
            (excluded '()))
        (dolist (tag parsed-tags)
          (if (string-prefix-p "-" tag)
              (push (substring tag 1) excluded)
            (push tag included)))
        (list :tags (nreverse included) 
              :excluded-tags (nreverse excluded))))))

;; replace the existing karakeep-parse-folders with this version
(defun karakeep-parse-folders (folders)
  "Normalize FOLDERS to a list of strings.
A single string is preserved verbatim (even if it contains spaces).
If the string contains commas, it is split on commas with quote support.
A list is normalized element-wise."
  (cond
   ((null folders) nil)
   ((listp folders)
    (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                (mapcar (lambda (f) (if (symbolp f) (symbol-name f) f)) folders)))
   ((stringp folders)
    (let* ((s (string-trim folders)))
      (cond
       ((string-empty-p s) nil)
       ((string-match-p "," s)
        (karakeep-parse-tags s))
       (t (list s)))))
   (t (list (format "%s" folders)))))

(defun karakeep--build-query (params)
  "Build URL query string from PARAMS alist ((key . value) ...)."
  (mapconcat (lambda (kv)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car kv)))
                       (url-hexify-string (format "%s" (cdr kv)))))
             (seq-filter (lambda (kv) (cdr kv)) params)
             "&"))

(defun karakeep--url (endpoint &optional query-alist)
  "Compose full URL for ENDPOINT with QUERY-ALIST."
  (concat (replace-regexp-in-string "/$" "" karakeep-api-base)
          (if (string-prefix-p "/" endpoint) endpoint (concat "/" endpoint))
          (when query-alist
            (concat "?" (karakeep--build-query query-alist)))))

(defun karakeep--collection-endpoint (collection)
  "Return the base endpoint for listing by COLLECTION."
  (let* ((id (cond
              ((numberp collection) collection)
              ((or (eq collection 'all) (eq collection :all) (null collection)) karakeep-default-collection)
              (t karakeep-default-collection))))
    (if (<= id 0)
        "/dashboard/bookmarks"
      (format "/dashboard/collections/%s/bookmarks" id))))

(defun karakeep--endpoint-for (collection search-present)
  "Pick the correct endpoint given COLLECTION and SEARCH-PRESENT."
  (ignore search-present)
  (karakeep--collection-endpoint collection))

(defun karakeep--collections-list ()
  "Return and cache the list of user's collections (list of alists)."
  (or karakeep--collections-cache
      (let* ((payload (karakeep-collections))
             (items (or (alist-get 'items payload)
                        (alist-get 'collections payload)
                        nil)))
        (setq karakeep--collections-cache (and (listp items) items))
        karakeep--collections-cache)))

(defun karakeep--resolve-collection-id (folder-name)
  (let* ((name (if (symbolp folder-name) (symbol-name folder-name) folder-name))
         (items (karakeep--collections-list))
         (match (seq-find (lambda (it)
                            (let ((title (alist-get 'title it)))
                              (and (stringp title)
                                   (or (string= title name)
                                       (string-equal (downcase title) (downcase name))))))
                          items)))
    (alist-get '_id match)))

;;;; Request-based HTTP implementation

(defun karakeep--make-headers (method)
  "Return an alist of HTTP headers for METHOD."
  (let ((headers `(("Authorization" . ,(concat "Bearer " (karakeep--get-token)))
                   ("Accept" . "application/json")
                   ("User-Agent" . ,karakeep--user-agent))))
    (if (eq method 'GET)
        headers
      (append headers '(("Content-Type" . "application/json"))))))


;;;; Request-based HTTP primitives

(defun karakeep-api-request (endpoint &optional method query-alist data)
  "Perform HTTP request to Karakeep API (synchronous).
Return parsed JSON as alist. Signal `user-error` for HTTP or parse errors."
  (let* ((method (or method 'GET))
         (url (karakeep--url endpoint query-alist))
         (token (karakeep--get-token))
         (headers `(("Authorization" . ,(concat "Bearer " token))
                    ("Content-Type" . "application/json")))
         (json-data (when data (json-encode data)))
         (type (pcase method ('GET "GET") ('PUT "PUT") ('POST "POST") ('DELETE "DELETE") (_ "GET")))
         (result-data nil)
         (result-error nil))
    (karakeep--debug "[sync] %s %s" type url)
    (request url
      :type type
      :headers headers
      :params nil
      :data json-data
      :parser 'json-read
      :encoding 'utf-8
      :timeout karakeep-request-timeout
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (setq result-data data)))
      :error (cl-function (lambda (&key error-thrown response &allow-other-keys)
                            (let ((status-code (when response (request-response-status-code response)))
                                  (err-msg (format "%s" error-thrown)))
                              (setq result-error (format "HTTP %s for %s: %s"
                                                         (or status-code "unknown") endpoint err-msg))))))
    (if result-error
        (user-error "karakeep.el: %s" result-error)
      result-data)))

(defun karakeep-api-request-async (endpoint &optional method query-alist data callback)
  "Asynchronous HTTP request to Karakeep API.
CALLBACK is called as (func RESULT ERR), where only one of RESULT/ERR is non-nil."
  (let* ((method (or method 'GET))
         (url (karakeep--url endpoint query-alist))
         (token (karakeep--get-token))
         (headers `(("Authorization" . ,(concat "Bearer " token))
                    ("Content-Type" . "application/json")))
         (json-data (when data (json-encode data)))
         (type (pcase method ('GET "GET") ('PUT "PUT") ('POST "POST") ('DELETE "DELETE") (_ "GET")))
         (cb (or callback #'ignore)))
    (karakeep--debug "[async] %s %s" type url)
    (request url
      :type type
      :headers headers
      :params nil
      :data json-data
      :parser 'json-read
      :encoding 'utf-8
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (funcall cb data nil)))
      :error   (cl-function (lambda (&key error-thrown &allow-other-keys)
                              (funcall cb nil (format "%s" error-thrown)))))))

;;;; Search parsing and query building

(defun karakeep--parse-search-input (s)
  "Parse search input S into tags, folders, excluded-tags, and text components.
Returns plist with :tags :excluded-tags :folders :text keys.
Supports '#tag', '#\"spaced tag\"', '-#tag', '-#\"spaced tag\"' for tags."
  (let* ((s (or s ""))
         (tags '())
         (excluded-tags '())
         (folders '())
         (texts '())
         (pos 0)
         (len (length s)))
    ;; Parse character by character to handle quoted tags
    (while (< pos len)
      (let ((char (aref s pos)))
        (cond
         ;; Skip whitespace
         ((memq char '(?\s ?\t))
          (setq pos (1+ pos)))
         
         ;; Handle excluded tags (-#tag or -#"quoted tag")
         ((and (< pos (- len 2)) (string= (substring s pos (+ pos 2)) "-#"))
          (setq pos (+ pos 2))
          (if (and (< pos len) (= (aref s pos) ?\"))
              ;; Quoted excluded tag: -#"tag with spaces"
              (let ((start (1+ pos))
                    (end (string-match "\"" s (1+ pos))))
                (if end
                    (progn
                      (push (substring s start end) excluded-tags)
                      (setq pos (1+ end)))
                  ;; No closing quote, treat as regular text
                  (push (substring s (- pos 2) (1+ pos)) texts)
                  (setq pos (1+ pos))))
            ;; Regular excluded tag: -#tag
            (let ((start pos)
                  (end pos))
              (while (and (< end len) 
                          (not (memq (aref s end) '(?\s ?\t))))
                (setq end (1+ end)))
              (when (> end start)
                (push (substring s start end) excluded-tags))
              (setq pos end))))
         
         ;; Handle tags (#tag or #"quoted tag")
         ((and (< pos len) (= (aref s pos) ?#))
          (setq pos (1+ pos))
          (if (and (< pos len) (= (aref s pos) ?\"))
              ;; Quoted tag: #"tag with spaces"
              (let ((start (1+ pos))
                    (end (string-match "\"" s (1+ pos))))
                (if end
                    (progn
                      (push (substring s start end) tags)
                      (setq pos (1+ end)))
                  ;; No closing quote, treat as regular text
                  (push (substring s (1- pos) (1+ pos)) texts)
                  (setq pos (1+ pos))))
            ;; Regular tag: #tag
            ;; Allow common punctuation used in Karakeep tags, including '/'
            (let ((start pos)
                  (end pos))
              (while (and (< end len)
                          (not (memq (aref s end) '(?\s ?\t)))
                          (string-match-p "[a-zA-Z0-9_+\-./]" (char-to-string (aref s end))))
                (setq end (1+ end)))
              (when (> end start)
                (push (substring s start end) tags))
              (setq pos end))))
         
         ;; Handle folders [folder name]
         ((and (< pos len) (= (aref s pos) ?\[))
          (let ((end (string-match "]" s pos)))
            (if end
                (progn
                  (push (substring s (1+ pos) end) folders)
                  (setq pos (1+ end)))
              ;; No closing bracket, treat as text
              (let ((start pos)
                    (end pos))
                (while (and (< end len) 
                            (not (memq (aref s end) '(?\s ?\t))))
                  (setq end (1+ end)))
                (push (substring s start end) texts)
                (setq pos end)))))
         
         ;; Regular text
         (t
          (let ((start pos)
                (end pos))
            (while (and (< end len) 
                        (not (memq (aref s end) '(?\s ?\t))))
              (setq end (1+ end)))
            (push (substring s start end) texts)
            (setq pos end))))))
    
    (list :tags (nreverse tags)
          :excluded-tags (nreverse excluded-tags)
          :folders (nreverse folders)
          :text (string-join (nreverse texts) " "))))

(defun karakeep--meaningful-search-input-p (parsed)
  "Check if PARSED search input has meaningful content for searching."
  (let ((text (plist-get parsed :text))
        (tags (plist-get parsed :tags))
        (excluded-tags (plist-get parsed :excluded-tags))
        (folders (plist-get parsed :folders)))
    (or (and text (>= (length text) 2))
        (and (listp tags) (> (length tags) 0))
        (and (listp excluded-tags) (> (length excluded-tags) 0))
        (and (listp folders) (> (length folders) 0)))))

(defun karakeep--compose-search-string (text tags &optional excluded-tags)
  "Compose a search string from TEXT, TAGS, and EXCLUDED-TAGS for Karakeep API."
  (string-join
   (delq nil
         (list (and (listp tags) tags (karakeep--build-tag-search tags 'all))
               (and (listp excluded-tags) excluded-tags (karakeep--build-excluded-tag-search excluded-tags))
               (and text (not (string-empty-p text)) text)))
   " "))

(defun karakeep--build-search-endpoint-and-query (collection-id search page &optional limit)
  "Build endpoint and query params for search request.
Returns cons (ENDPOINT . QUERY-PARAMS)."
  (let* ((cid (if (numberp collection-id) collection-id karakeep-default-collection))
         (pp (max 1 (min (or limit 50) 200)))
         (pg (max 0 (or page 0)))
         (endpoint (if (<= cid 0)
                        "/dashboard/bookmarks"
                      (format "/dashboard/collections/%d/bookmarks" cid)))
         (q (seq-filter #'consp
                        (append
                         (when (and search (not (string-empty-p search)))
                           (list (cons 'q search)))
                         (list (cons 'limit pp)
                               (cons 'page pg))))))
    (cons endpoint q)))

;;;; Search building

(defun karakeep--quote-tag (tag)
  "Convert TAG to Karakeep search token: #tag or #\"multi word\"."
  (let ((s (if (symbolp tag) (symbol-name tag) tag)))
    (if (string-match-p "[\t\n\r ]" s)
        (concat "#\"" (replace-regexp-in-string "\"" "\\\"" s) "\"")
      (concat "#" s))))

(defun karakeep--quote-folder (name)
  "Convert folder NAME to token: collection:NAME or collection:\"multi word\"."
  (let ((s (if (symbolp name) (symbol-name name) name)))
    (if (string-match-p "[\t\n\r ]" s)
        (concat "collection:\"" (replace-regexp-in-string "\"" "\\\"" s) "\"")
      (concat "collection:" s))))

(defun karakeep--build-search (tags folders match)
  "Build a combined search string for TAGS and FOLDERS honoring MATCH ('all|'any)."
  (let* ((ts (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                         (mapcar (lambda (tag) (if (symbolp tag) (symbol-name tag) tag)) (or tags '()))))
         (fs (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                         (mapcar (lambda (f) (if (symbolp f) (symbol-name f) f)) (or folders '()))))
         (terms (append (mapcar #'karakeep--quote-tag ts)
                        (mapcar #'karakeep--quote-folder fs))))
    (pcase match
      ('any (string-join (cons "match:OR" terms) " "))
      (_    (string-join terms " ")))))

(defun karakeep--build-tag-search (tags match)
  "Build a search string for TAGS honoring MATCH ('all|'any)."
  (karakeep--build-search tags nil match))

(defun karakeep--quote-tag-name (tag)
  "Quote TAG name for API search without # prefix."
  (let ((s (if (symbolp tag) (symbol-name tag) tag)))
    (if (string-match-p "[\t\n\r ]" s)
        (concat "\"" (replace-regexp-in-string "\"" "\\\"" s) "\"")
      s)))

(defun karakeep--build-excluded-tag-search (excluded-tags)
  "Build a search string for EXCLUDED-TAGS using NOT operators.
According to Karakeep API, you can exclude tags using '-tag'."
  (when (and excluded-tags (listp excluded-tags))
    (string-join
     (mapcar (lambda (tag) (format "-%s" (karakeep--quote-tag tag)))
             excluded-tags)
     " ")))

;;;; Normalization

(defun karakeep--extract-collection-id (collection)
  "Extract numeric collection ID from COLLECTION object."
  (when collection
    (or (and (numberp collection) collection)
        (karakeep--kv collection '$id)
        (karakeep--kv collection '_id)
        (karakeep--kv collection 'id))))

(defun karakeep--normalize-item (raw)
  "Normalize RAW Karakeep item alist into a simpler alist."
  (let* ((link (alist-get 'link raw))
         (title (or (alist-get 'title raw) link))
         (excerpt (or (alist-get 'excerpt raw) ""))
         (tags-raw (alist-get 'tags raw))
         (tags (if (vectorp tags-raw) (append tags-raw nil) tags-raw))
         (created (alist-get 'created raw))
         (id (alist-get '_id raw))
         (domain (alist-get 'domain raw))
         (collection-raw (alist-get 'collection raw))
         (collection-id (karakeep--extract-collection-id collection-raw)))
    (list (cons :id id)
          (cons :link link)
          (cons :title title)
          (cons :excerpt excerpt)
          (cons :tags tags)
          (cons :domain domain)
          (cons :created created)
          (cons :collection collection-raw)
          (cons :collectionId collection-id))))

(defun karakeep--normalize-items (items)
  "Normalize a list of raw ITEMS."
  (let ((item-list (if (and items (listp items)) items '())))
    (karakeep--debug "normalize-items input count=%S" (length item-list))
    (let ((result (mapcar #'karakeep--normalize-item item-list)))
      (karakeep--debug "normalize-items output count=%S" (length result))
      result)))

;;;; Query planning (shared between sync/async)

(defun karakeep--plan (plist)
  "Build a plan from PLIST and return a plist:
:tags :folders :search-present :endpoint :query :coll-id :alt-endpoint"
  (let* ((tags (plist-get plist :tags))
         (folders (or (plist-get plist :folders)
                      (let ((f (plist-get plist :folder))) (and f (list f)))))
         (collection (or (plist-get plist :collection) karakeep-default-collection))
         (limit (or (plist-get plist :limit) karakeep-default-limit))
         (match (or (plist-get plist :match) 'all))
         (tags* (and tags (if (listp tags) tags (list tags))))
         (folders* (and folders (if (listp folders) folders (list folders))))
         (resolved-coll (and folders* (= (length folders*) 1)
                             (let ((cid (karakeep--resolve-collection-id (car folders*))))
                               (and (integerp cid) (> cid 0) cid))))
         (effective-coll (or resolved-coll collection))
         (search (and (or tags* (and folders* (not resolved-coll)))
                      (karakeep--build-search tags* (and (not resolved-coll) folders*) match)))
         (endpoint (karakeep--endpoint-for effective-coll (and search t)))
         (query `((limit . ,(min limit 200))
                  ,@(when search `((q . ,search)))))
         (coll-id (cond
                   ((numberp effective-coll) effective-coll)
                   ((or (eq effective-coll 'all) (eq effective-coll :all) (null effective-coll)) karakeep-default-collection)
                   (t karakeep-default-collection))))
    (list :tags tags* :folders folders* :search-present (and search t)
          :endpoint endpoint :query query :coll-id coll-id :alt-endpoint nil)))

;;;; Public fetching API

(defun karakeep-fetch (&rest plist)
  "Fetch Karakeep bookmarks according to PLIST keys:
:tags (list or string), :excluded-tags (list or string), :folders (list/string) or :folder (string),
:search (string), :collection (number or 0 for all), :limit (int), :sort (symbol), :match ('all|'any).
Returns list of normalized items."
  (karakeep--debug "fetch %S" plist)
  (let* ((tags (plist-get plist :tags))
         (excluded-tags (plist-get plist :excluded-tags))
         (folders (or (plist-get plist :folders)
                      (let ((f (plist-get plist :folder))) (and f (list f)))))
         (search-text (plist-get plist :search))
         (match (or (plist-get plist :match) 'all))
         (limit (or (plist-get plist :limit) karakeep-default-limit))
         (input (string-join 
                 (delq nil
                       (append 
                        (when tags 
                          (list (karakeep--build-tag-search tags match)))
                        (when excluded-tags
                          (list (karakeep--build-excluded-tag-search excluded-tags)))
                        (when folders
                          (list (string-join (mapcar (lambda (folder) (concat "[" folder "]")) folders) " ")))
                        (when (and search-text (not (string-empty-p (string-trim search-text))))
                          (list (string-trim search-text)))))
                 " ")))
    (karakeep--debug "fetch input=%S limit=%S" input limit)
    (if (string-empty-p (string-trim input))
        '()  ; Return empty list if no search criteria
      (progn
        (karakeep--debug "calling karakeep-search-bookmarks with input=%S limit=%S" input limit)
        (karakeep-search-bookmarks input nil limit)))))

(defun karakeep-fetch-async (plist callback)
  "Asynchronously fetch Karakeep bookmarks according to PLIST and call CALLBACK with (items err)."
  (let* ((tags (plist-get plist :tags))
         (excluded-tags (plist-get plist :excluded-tags))
         (folders (or (plist-get plist :folders)
                      (let ((f (plist-get plist :folder))) (and f (list f)))))
         (search-text (plist-get plist :search))
         (match (or (plist-get plist :match) 'all))
         (limit (or (plist-get plist :limit) karakeep-default-limit))
         (input (string-join 
                 (delq nil
                       (append 
                        (when tags 
                          (list (karakeep--build-tag-search tags match)))
                        (when excluded-tags
                          (list (karakeep--build-excluded-tag-search excluded-tags)))
                        (when folders
                          (list (string-join (mapcar (lambda (folder) (concat "[" folder "]")) folders) " ")))
                        (when (and search-text (not (string-empty-p (string-trim search-text))))
                          (list (string-trim search-text)))))
                 " ")))
    (karakeep--debug "fetch-async input=%S limit=%S" input limit)
    (if (string-empty-p (string-trim input))
        (funcall callback '() nil)  ; Return empty list if no search criteria
      (karakeep-search-bookmarks input callback limit))))

;;;; Filters API

(defun karakeep--filters-endpoint (collection)
  "Return the Filters endpoint for COLLECTION."
  (let* ((id (cond
              ((numberp collection) collection)
              ((or (eq collection 'all) (eq collection :all) (null collection)) karakeep-default-collection)
              (t karakeep-default-collection)))
         (cid (if (<= id 0) 0 id)))
    (if (<= cid 0)
        "/dashboard/bookmarks/filters"
      (format "/dashboard/collections/%s/filters" cid))))

(defun karakeep-filters (&rest plist)
  "Fetch aggregated filters for a given collection.
PLIST keys:
  :collection — collection id (default `karakeep-default-collection` → “all”)
  :search — free-text search string
  :tags-sort — 'count (default) or '_id

Returns an alist per API: fields broken, duplicates, important, notag,
tags (list of {_id count}), types (list of {_id count})."
  (let* ((collection (or (plist-get plist :collection) karakeep-default-collection))
         (search (plist-get plist :search))
         (tags-sort (or (plist-get plist :tags-sort) 'count))
         (endpoint (karakeep--filters-endpoint collection))
         (query `((tagsSort . ,(pcase tags-sort
                                 ('_id "_id")
                                 ('count "count")
                                 (_ "count")))
                  ,@(when (and search (stringp search) (> (length search) 0))
                      `((q . ,search)))))
         (payload (karakeep-api-request endpoint 'GET query nil)))
    payload))

;;;###autoload
(defun karakeep-debug-filters (&optional collection search)
  "Make a raw request to the Filters API for COLLECTION with SEARCH."
  (let* ((coll (or collection karakeep-default-collection)))
    (karakeep-filters :collection coll :search search)))

;;;; Enhanced collection management

(defvar karakeep--collections-by-title nil
  "Hash table mapping collection titles (lowercase) to IDs.")

(defvar karakeep--collections-by-id nil
  "Hash table mapping collection IDs to titles.")

(defvar karakeep--collections-by-path nil
  "Hash table mapping full path (lowercase) to IDs (e.g. \"parent/child\").")

(defvar karakeep--collections-parent nil
  "Hash table mapping collection ID to parent ID.")

(defvar karakeep--collections-all nil
  "List of all collections (both root and nested).")

(defvar karakeep--collections-loading nil
  "Flag indicating if collections are currently being loaded.")

(defvar karakeep--collections-ready nil
  "Flag indicating if collections have been loaded and indexed.")

(defvar karakeep--collections-callbacks nil
  "List of callbacks waiting for collections to load.")

(defun karakeep--kv (item key)
  "Get value from ITEM (alist/plist) using KEY (keyword or symbol)."
  (let* ((k1 key)
         (k2 (if (keywordp key) (intern (substring (symbol-name key) 1))
               (intern (format ":%s" key)))))
    (or (plist-get item k1)
        (plist-get item k2)
        (and (consp item) (alist-get k1 item))
        (and (consp item) (alist-get k2 item)))))

(defun karakeep--extract-parent-id (collection)
  "Extract parent collection ID from COLLECTION."
  (let ((parent (karakeep--kv collection 'parent)))
    (when parent
      (or (karakeep--kv parent '$id)
          (karakeep--kv parent '_id)
          (karakeep--kv parent 'id)
          (and (numberp parent) parent)))))

(defun karakeep--build-collection-path (id by-id by-parent &optional seen)
  "Build full path for collection ID using BY-ID and BY-PARENT hash tables.
SEEN is used to prevent infinite loops."
  (let ((seen (or seen (make-hash-table :test 'eql))))
    (when (gethash id seen)
      (error "Circular reference in collections"))
    (puthash id t seen)
    (let ((title (gethash id by-id))
          (parent-id (gethash id by-parent)))
      (if (and parent-id (gethash parent-id by-id))
          (concat (karakeep--build-collection-path parent-id by-id by-parent seen)
                  "/" title)
        title))))

(defun karakeep--build-collections-index (items)
  "Build index hash tables from collections ITEMS."
  (let ((by-title (make-hash-table :test 'equal))
        (by-id (make-hash-table :test 'eql))
        (by-path (make-hash-table :test 'equal))
        (by-parent (make-hash-table :test 'eql))
        (seen-ids (make-hash-table :test 'eql)))
    (dolist (c items)
      (let* ((id (or (karakeep--kv c '_id)
                     (karakeep--kv c :_id)
                     (karakeep--kv c 'id)
                     (karakeep--kv c :id)))
             (title (or (karakeep--kv c 'title)
                        (karakeep--kv c :title)
                        (karakeep--kv c 'name)
                        (karakeep--kv c :name)))
             (parent-id (karakeep--extract-parent-id c)))
        (when (and id title (not (gethash id seen-ids)))
          (puthash id t seen-ids)
          (puthash (downcase title) id by-title)
          (puthash id title by-id)
          (when parent-id
            (puthash id parent-id by-parent)))))
    (maphash (lambda (id _title)
               (let ((path (karakeep--build-collection-path id by-id by-parent)))
                 (when path
                   (puthash (downcase path) id by-path))))
             by-id)
    (setq karakeep--collections-by-title by-title
          karakeep--collections-by-id by-id
          karakeep--collections-by-path by-path
          karakeep--collections-parent by-parent
          karakeep--collections-all items)))

(defun karakeep--run-collections-callbacks ()
  "Run all pending collection callbacks and clear the list."
  (let ((callbacks karakeep--collections-callbacks))
    (setq karakeep--collections-callbacks nil)
    (dolist (cb callbacks)
      (when cb (funcall cb)))))

(defun karakeep--ensure-collections-async (&optional callback)
  "Ensure collections are loaded (root + nested), calling CALLBACK when ready."
  (cond
   (karakeep--collections-ready
    (when callback (funcall callback)))
   (karakeep--collections-loading
    (when callback
      (push callback karakeep--collections-callbacks)))
   (t
    (when callback
      (push callback karakeep--collections-callbacks))
    (setq karakeep--collections-loading t)
    (karakeep-api-request-async
     "/dashboard/collections" 'GET nil nil
     (lambda (root-res root-err)
       (if root-err
           (progn
             (setq karakeep--collections-loading nil
                   karakeep--collections-ready t)
             (karakeep--run-collections-callbacks))
         (let ((root-items (alist-get 'items root-res)))
           (when (vectorp root-items)
             (setq root-items (append root-items nil)))
           (karakeep-api-request-async
            "/dashboard/collections/childrens" 'GET nil nil
            (lambda (child-res child-err)
              (setq karakeep--collections-loading nil)
              (let ((child-items (unless child-err
                                   (let ((items (alist-get 'items child-res)))
                                     (if (vectorp items) (append items nil) items))))
                    (all-items (or root-items '())))
                (when child-items
                  (setq all-items (append all-items child-items)))
                (karakeep--build-collections-index all-items))
              (setq karakeep--collections-ready t)
              (karakeep--run-collections-callbacks))))))))))

(defun karakeep--ensure-collections-sync ()
  "Ensure collections are loaded synchronously."
  (unless karakeep--collections-ready
    (let* ((root-res (karakeep-api-request "/dashboard/collections" 'GET nil nil))
           (root-items (let ((items (alist-get 'items root-res)))
                         (if (vectorp items) (append items nil) items)))
           (child-res (condition-case nil
                          (karakeep-api-request "/dashboard/collections/childrens" 'GET nil nil)
                        (error nil)))
           (child-items (when child-res
                          (let ((items (alist-get 'items child-res)))
                            (if (vectorp items) (append items nil) items))))
           (all-items (append (or root-items '()) (or child-items '()))))
      (karakeep--build-collections-index all-items)
      (setq karakeep--collections-ready t))))

(defun karakeep--collection-id-by-title (name)
  "Get collection ID by NAME (title or full path like \"Parent/Child\")."
  (when name
    (let* ((lname (downcase name))
           (by-path (and karakeep--collections-by-path
                         (gethash lname karakeep--collections-by-path)))
           (by-title (and karakeep--collections-by-title
                          (gethash lname karakeep--collections-by-title)))
           (by-path-suffix
            (and (not by-path)
                 (not by-title)
                 karakeep--collections-by-path
                 (let ((suffix (concat "/" lname))
                       found-id)
                   (maphash (lambda (path id)
                              (when (and (not found-id)
                                         (string-suffix-p suffix path))
                                (setq found-id id)))
                            karakeep--collections-by-path)
                   found-id)))
           (by-substring
            (and (not by-path)
                 (not by-title)
                 (not by-path-suffix)
                 karakeep--collections-by-path
                 (let (found-id)
                   (maphash (lambda (path id)
                              (when (and (not found-id)
                                         (string-match-p (regexp-quote lname) path))
                                (setq found-id id)))
                            karakeep--collections-by-path)
                   found-id))))
      (or by-path by-title by-path-suffix by-substring))))

(defun karakeep--collection-title-by-id (cid)
  "Get collection title by ID CID."
  (cond
   ((eq cid -1) "Unsorted")
   ((hash-table-p karakeep--collections-by-id)
    (gethash cid karakeep--collections-by-id))
   (t nil)))

(defun karakeep--collection-path-by-id (cid)
  "Get full collection path by ID CID (e.g. \"Parent/Child\")."
  (cond
   ((null cid) nil)
   ((eq cid -1) "Unsorted")
   ((and karakeep--collections-by-id
         karakeep--collections-parent
         (gethash cid karakeep--collections-by-id))
    (karakeep--build-collection-path cid
                                      karakeep--collections-by-id
                                      karakeep--collections-parent))
   (t (karakeep--collection-title-by-id cid))))

(defun karakeep--all-collection-paths ()
  "Return list of all collection paths for completion."
  (when karakeep--collections-by-path
    (let (paths)
      (maphash (lambda (_key id)
                 (let ((path (karakeep--collection-path-by-id id)))
                   (when path (push path paths))))
               karakeep--collections-by-id)
      (sort paths #'string<))))

;;;; High-level search API

(defun karakeep--do-search-bookmarks (input callback limit page)
  "Internal search implementation after collections are loaded."
  (let* ((parsed (karakeep--parse-search-input input))
         (tags (plist-get parsed :tags))
         (excluded-tags (plist-get parsed :excluded-tags))
         (folders (plist-get parsed :folders))
         (text (plist-get parsed :text))
         (folder-name (car (last folders)))
         (coll-id (or (and folder-name
                           (karakeep--collection-id-by-title folder-name))
                      karakeep-default-collection))
         (search (karakeep--compose-search-string text tags excluded-tags))
         (pair (karakeep--build-search-endpoint-and-query coll-id search page limit))
         (endpoint (car pair))
         (query (cdr pair)))
    (if callback
        (karakeep-api-request-async
         endpoint 'GET query nil
         (lambda (res err)
           (when (and err (string-match-p "HTTP 404\\|Network error" err))
             (setq res '((items))))
           (if err
               (funcall callback nil err)
             (let* ((raw (or (alist-get 'items res) '()))
                    (items (if (vectorp raw) (append raw nil) raw))
                    (norm (mapcar (lambda (x) (condition-case nil
                                                  (karakeep--normalize-item x)
                                                (error x)))
                                  items)))
               (funcall callback norm nil)))))
      (let* ((payload (karakeep-api-request endpoint 'GET query nil))
             (items-raw (alist-get 'items payload))
             (items (if (vectorp items-raw) (append items-raw nil) items-raw)))
        (karakeep--normalize-items items)))))

(defun karakeep-search-bookmarks (input &optional callback limit page)
  "Search Karakeep bookmarks with INPUT string.
If CALLBACK is provided, performs async search, otherwise sync.
Returns normalized items list (sync) or calls CALLBACK with (items err).
Ensures collections are loaded before searching."
  (if callback
      (if karakeep--collections-ready
          (karakeep--do-search-bookmarks input callback limit page)
        (karakeep--ensure-collections-async
         (lambda ()
           (karakeep--do-search-bookmarks input callback limit page))))
    (unless karakeep--collections-ready
      (karakeep--ensure-collections-sync))
    (karakeep--do-search-bookmarks input callback limit page)))

;;;; Tags management

(defun karakeep--fetch-tags (&optional callback)
  "Fetch all tags from Karakeep API.
If CALLBACK is provided, performs async request, otherwise sync.
Returns list of tag objects or calls CALLBACK with (tags err)."
  (let ((endpoint "/tags"))
    (if callback
        (karakeep-api-request-async
         endpoint 'GET nil nil
         (lambda (res err)
           (if err
               (funcall callback nil err)
             (let* ((raw (or (alist-get 'items res) '()))
                    (tags (if (vectorp raw) (append raw nil) raw)))
               (funcall callback tags nil)))))
      (let* ((payload (karakeep-api-request endpoint 'GET nil nil))
             (items-raw (alist-get 'items payload))
             (tags (if (vectorp items-raw) (append items-raw nil) items-raw)))
        tags))))

(defun karakeep-force-load-tags (&optional callback)
  "Force reload all tags from Karakeep API and update cache.
If CALLBACK is provided, performs async request, otherwise sync.
Returns list of tags or calls CALLBACK with (tags err)."
  (interactive)
  (setq karakeep--tags-ready nil
        karakeep--tags-cache nil
        karakeep--tags-loading nil)
  (karakeep--debug "force loading tags...")
  (if callback
      (progn
        (setq karakeep--tags-loading t)
        (karakeep--fetch-tags
         (lambda (tags err)
           (setq karakeep--tags-loading nil)
           (if err
               (karakeep--debug "tags load error: %s" err)
             (setq karakeep--tags-cache tags
                   karakeep--tags-ready t)
             (karakeep--debug "loaded %d tags" (length tags)))
           (funcall callback (or tags '()) err))))
    (let ((tags (karakeep--fetch-tags)))
      (setq karakeep--tags-cache tags
            karakeep--tags-ready t)
      (karakeep--debug "loaded %d tags" (length tags))
      tags)))

(defun karakeep-load-tags (&optional callback)
  "Load tags from cache or API if not already loaded.
If CALLBACK is provided, performs async request when needed, otherwise sync.
Returns list of tags or calls CALLBACK with (tags err)."
  (cond
   ((and karakeep--tags-ready karakeep--tags-cache)
    (karakeep--debug "using cached tags (%d items)" (length karakeep--tags-cache))
    (if callback
        (funcall callback karakeep--tags-cache nil)
      karakeep--tags-cache))
   (karakeep--tags-loading
    (karakeep--debug "tags are loading, returning empty for now")
    (if callback
        (funcall callback '() nil)
      '()))
   (t
    (karakeep--debug "loading tags from API...")
    (if callback
        (progn
          (setq karakeep--tags-loading t)
          (karakeep--fetch-tags
           (lambda (tags err)
             (setq karakeep--tags-loading nil)
             (unless err
               (setq karakeep--tags-cache tags
                     karakeep--tags-ready t))
             (funcall callback (or tags '()) err))))
      (let ((tags (karakeep--fetch-tags)))
        (setq karakeep--tags-cache tags
              karakeep--tags-ready t)
        tags)))))

(defun karakeep-copy-tags ()
  "Copy all tags to kill ring as a comma-separated string."
  (interactive)
  (let* ((tags (karakeep-load-tags))
         (tag-names (mapcar (lambda (tag) 
                              (or (alist-get '_id tag)
                                  (alist-get 'name tag)
                                  (format "%s" tag)))
                            tags))
         (tags-string (string-join tag-names ", ")))
    (if tag-names
        (progn
          (kill-new tags-string)
          (message "Copied %d tags to kill ring: %s" 
                   (length tag-names)
                   (if (> (length tags-string) 80)
                       (concat (substring tags-string 0 77) "...")
                     tags-string)))
      (message "No tags found"))))

;;;; Collections helpers

(defun karakeep-collections ()
  "Get the user's collection list (alist payload).
Useful for discovering valid collection IDs for /dashboard/collections/{id}/bookmarks requests."
  (karakeep-api-request "/dashboard/collections" 'GET nil nil))

;;;###autoload
(defun karakeep-debug-collections ()
  "Return the raw payload with the collection list."
  (karakeep-collections))

;;;###autoload
(defun karakeep-clear-cache ()
  "Clear internal caches (collections and tags)."
  (interactive)
  (setq karakeep--collections-cache nil
        karakeep--tags-cache nil
        karakeep--tags-ready nil
        karakeep--tags-loading nil)
  (when karakeep-debug-enable
    (karakeep--debug "collections and tags cache cleared")))

;;;; Minimal debug helpers

;;;###autoload
(defun karakeep-debug-search (tags &optional collection)
  "Make a raw API request filtered by TAGS and return the payload (alist).
TAGS is a string or list of tags. COLLECTION is a collection id (default 0 = all collections)."
  (interactive
   (list (read-string "Tags (space-separated, empty for none): ")
         (when current-prefix-arg
           (read-number "Collection id (default 0 = all): " 0))))
  (let* ((tags-list (karakeep-parse-tags tags))
         (search (and tags-list (karakeep--build-tag-search tags-list 'all)))
         (coll (if (numberp collection) collection 0))
         (endpoint (karakeep--endpoint-for coll (and search t)))
         (payload (karakeep-api-request endpoint 'GET `((limit . 5) ,@(when search `((q . ,search)))) nil)))
    (when (called-interactively-p 'interactive)
      (message "karakeep.el: debug-search %s → %s"
               endpoint
               (or (alist-get 'items payload) payload)))
    payload))

;;;###autoload
(defun karakeep-debug-fetch (tags &optional collection limit)
  "Get a normalized list of bookmarks filtered by TAGS.
COLLECTION is the collection id (default 0 = all). LIMIT is the maximum number of items (default 10)."
  (let* ((tags-list (karakeep-parse-tags tags))
         (coll (if (numberp collection) collection 0))
         (lim (or limit 10)))
    (apply #'karakeep-fetch (list :tags tags-list :collection coll :limit lim :match 'all))))

(provide 'karakeep)

;;; karakeep.el ends here
