;;; ob-karakeep.el --- Org-babel support for karakeep.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 artawower

;; Author: artawower <artawower33@gmail.com>
;; URL: https://github.com/julian-hoch/karakeep.el
;; Package-Requires: ((emacs "27.1") (org "9.4"))
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
;; Org-babel `karakeep` language: returns Org-formatted output suitable for
;; :results raw replace. Parameters mirror dynamic block params.

;;; Code:

(require 'org)
(require 'ob)
(require 'karakeep)
(require 'karakeep-org)

(defvar org-babel-default-header-args:karakeep
  '((:results . "raw replace") (:output . org-list))
  "Default header args for karakeep babel blocks.")

(defun karakeep-ob--param (key params default)
  "Return value for KEY from PARAMS or DEFAULT if not set or empty."
  (let ((v (alist-get key params nil nil #'eq)))
    (if (and v (not (equal v ""))) v default)))

(defun karakeep-ob--items (params)
  "Return fetched Karakeep items based on PARAMS."
  (let* ((tags-input (karakeep-ob--param :tags params nil))
         (parsed-tags (karakeep-parse-tags-with-exclusion tags-input))
         (tags (plist-get parsed-tags :tags))
         (excluded-tags (plist-get parsed-tags :excluded-tags))
         (search-text (karakeep-ob--param :search params nil))
         (match (or (karakeep-ob--param :match params 'all) 'all))
         (limit (or (karakeep-ob--param :limit params karakeep-default-limit)
                    karakeep-default-limit))
         (collection (or (karakeep-ob--param :collection params karakeep-default-collection)
                         karakeep-default-collection)))
    (if (or tags excluded-tags search-text)
        (apply #'karakeep-fetch
               (append (list :match match :limit limit :collection collection)
                       (when tags (list :tags tags))
                       (when excluded-tags (list :excluded-tags excluded-tags))
                       (when (and search-text (not (string-empty-p (string-trim search-text))))
                         (list :search search-text))))
      '())))

(defun karakeep-ob--render (items output)
  "Render ITEMS according to OUTPUT format."
  (pcase output
    ('org-list (if (null items) "- No results" (karakeep-render-org-list items)))
    (_ (user-error "ob-karakeep: Unsupported :output %S" output))))

;;;###autoload
(defun org-babel-execute:karakeep (_body params)
  "Execute a Karakeep org-babel block with PARAMS."
  (let* ((output (or (karakeep-ob--param :output params 'org-list) 'org-list))
         (items (karakeep-ob--items params)))
    (karakeep-ob--render items output)))

(provide 'ob-karakeep)

;;; ob-karakeep.el ends here
