;;; karakeep-search.el --- As-you-type Karakeep search completion  -*- lexical-binding: t; -*-

;; Author: artawower <artawower33@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, matching
;; URL: https://github.com/julian-hoch/karakeep.el

;;; Commentary:
;; Interactive search interface for Karakeep bookmarks with:
;; - As-you-type completion with animated spinner
;; - Smart collection/tag parsing (#tag, [folder])
;; - Embark integration for editing/deleting items
;; - Customizable spinner animation and appearance

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'karakeep)

(declare-function karakeep--kv "karakeep" (item key))
(declare-function karakeep--collection-id-by-title "karakeep" (title))
(declare-function karakeep--collection-title-by-id "karakeep" (id))
(declare-function karakeep--collection-path-by-id "karakeep" (id))
(declare-function karakeep--ensure-collections-async "karakeep" (callback))
(declare-function karakeep--parse-search-input "karakeep" (input))
(declare-function karakeep--meaningful-search-input-p "karakeep" (parsed))
(declare-function karakeep-search-bookmarks "karakeep" (input callback &optional page-size page))
(declare-function karakeep-api-request-async "karakeep" (endpoint method params payload callback))

(defgroup karakeep-search nil
  "Completion UI for Karakeep."
  :group 'convenience)

(defun karakeep-search--debug (fmt &rest args)
  "Log debug message if `karakeep-debug' is enabled."
  (when (bound-and-true-p karakeep-debug)
    (message "[karakeep-search] %s" (apply #'format fmt args))))

(defface karakeep-search-tag
  '((t :inherit font-lock-keyword-face :foreground "DodgerBlue3"))
  "Face for tags in completion results."
  :group 'karakeep-search)

(defface karakeep-search-collection
  '((t :inherit font-lock-function-name-face :foreground "orange"))
  "Face for collections in completion results."
  :group 'karakeep-search)

(defface karakeep-search-edit-key
  '((t :inherit font-lock-keyword-face))
  "Face used for read-only field labels in the edit buffer."
  :group 'karakeep-search)

(defface karakeep-search-edit-header
  '((t :inherit shadow :weight normal))
  "Face used for the header line in the edit buffer."
  :group 'karakeep-search)

(defcustom karakeep-search-title-max 20
  "Maximum characters for titles before truncation."
  :type 'integer
  :group 'karakeep-search)

(defcustom karakeep-search-excerpt-max 40
  "Maximum characters for excerpts before truncation."
  :type 'integer
  :group 'karakeep-search)

(defcustom karakeep-search-idle-delay 0.25
  "Idle time before firing a Karakeep API request."
  :type 'number
  :group 'karakeep-search)

(defcustom karakeep-search-page-size 50
  "Number of items to request per page from the Karakeep API (1..50)."
  :type 'integer
  :group 'karakeep-search)

(defcustom karakeep-search-text-collection 0
  "Default collection ID to search when no [collection] token is used.
0 means the root (\"All\"), while -1 refers to \"Unsorted\"."
  :type 'integer
  :group 'karakeep-search)

(defcustom karakeep-search-enter-action 'link
  "Enter opens either original link or Karakeep app."
  :type '(choice (const link) (const karakeep))
  :group 'karakeep-search)

(defcustom karakeep-search-karakeep-url-builder
  #'karakeep-search--default-karakeep-url-builder
  "Function used to build a Karakeep app URL for ITEM.
Called with a normalized Karakeep item plist/alist and must return a
URL string."
  :type 'function
  :group 'karakeep-search)

(defcustom karakeep-search-spinner-frames '("|" "/" "-" "\\")
  "Frames for the loading spinner animation."
  :type '(repeat string)
  :group 'karakeep-search)

(defcustom karakeep-search-spinner-delay 0.1
  "Delay in seconds between spinner frame updates."
  :type 'number
  :group 'karakeep-search)

(defvar karakeep-search--timer nil
  "Timer for delayed API requests.")

(defvar karakeep-search--last-gen 0
  "Generation counter for tracking request versions.")

(defvar karakeep-search--items nil
  "Current list of search result items.")

(defvar karakeep-search--last-input nil
  "Last input string to detect changes.")

(defvar karakeep-search--cand-map (make-hash-table :test 'equal)
  "Hash table mapping candidate strings to items.")

(defvar karakeep-search--loading nil
  "Whether a search is currently loading.")

(defvar karakeep-search--spinner-timer nil
  "Timer for spinner animation.")

(defvar karakeep-search--spinner-index 0
  "Current frame index in spinner animation.")

(defvar-local karakeep-search--edit-item-id nil
  "ID of the item being edited in the current buffer.")


(defun karakeep-search--start-spinner ()
  "Start the loading spinner animation."
  (setq karakeep-search--loading t
        karakeep-search--spinner-index 0)
  (when karakeep-search--spinner-timer
    (cancel-timer karakeep-search--spinner-timer))
  (setq karakeep-search--spinner-timer
        (run-with-timer 0 karakeep-search-spinner-delay #'karakeep-search--update-spinner)))

(defun karakeep-search--stop-spinner ()
  "Stop the loading spinner animation."
  (setq karakeep-search--loading nil)
  (when karakeep-search--spinner-timer
    (cancel-timer karakeep-search--spinner-timer)
    (setq karakeep-search--spinner-timer nil))
  (karakeep-search--safe-exhibit))

(defun karakeep-search--update-spinner ()
  "Update spinner frame and refresh display."
  (when karakeep-search--loading
    (setq karakeep-search--spinner-index
          (mod (1+ karakeep-search--spinner-index)
               (length karakeep-search-spinner-frames)))
    (karakeep-search--safe-exhibit)))

(defun karakeep-search--get-spinner-frame ()
  "Get current spinner frame."
  (if karakeep-search--loading
      (nth karakeep-search--spinner-index karakeep-search-spinner-frames)
    ""))

(defun karakeep-search--truncate (s n)
  "Truncate string S to N characters with ellipsis."
  (if (and (stringp s) (> (length s) n))
      (concat (substring s 0 n) "…")
    (or s "")))

(defalias 'karakeep-search--kv 'karakeep--kv)

(defun karakeep-search--safe-exhibit ()
  "Force completion UI update across different frameworks."
  (let ((win (active-minibuffer-window))
        (gen karakeep-search--last-gen))
    (when (and win (minibufferp (window-buffer win)))
      (with-selected-window win
        (when (= gen karakeep-search--last-gen)
          (cond
           ((and (boundp 'vertico-mode) vertico-mode)
            (with-current-buffer (window-buffer win)
              (when (boundp 'vertico--input)
                (setq vertico--input nil))
              (when (boundp 'vertico--history-hash)
                (setq vertico--history-hash nil))
              (when (boundp 'vertico--lock-candidate)
                (setq vertico--lock-candidate nil))
              (when (fboundp 'vertico--update)
                (vertico--update))
              (let ((content (minibuffer-contents-no-properties)))
                (delete-minibuffer-contents)
                (insert content))
              (when (fboundp 'vertico--exhibit)
                (vertico--exhibit))
              (redisplay t)))
           ((and (boundp 'ivy-mode) ivy-mode (fboundp 'ivy--exhibit))
            (ivy--exhibit))
           ((and (boundp 'icomplete-mode) icomplete-mode)
            (when (fboundp 'icomplete-exhibit)
              (icomplete-exhibit))
            (redisplay t))
           ((fboundp 'mct--exhibit)
            (mct--exhibit))
           ((and (boundp 'selectrum-mode) selectrum-mode (fboundp 'selectrum--update))
            (selectrum--update))
           (t (redisplay t))))))))

(defalias 'karakeep-search--collection-id-by-title 'karakeep--collection-id-by-title)
(defalias 'karakeep-search--collection-title-by-id 'karakeep--collection-title-by-id)
(defalias 'karakeep-search--collection-path-by-id 'karakeep--collection-path-by-id)

(defun karakeep-search--ensure-collections ()
  "Ensure collections are loaded, refreshing UI when ready."
  (karakeep--ensure-collections-async
   (lambda () (karakeep-search--safe-exhibit))))

(defalias 'karakeep-search--parse 'karakeep--parse-search-input)
(defalias 'karakeep-search--meaningful-input-p 'karakeep--meaningful-search-input-p)

(defun karakeep-search--domain-of (link item)
  "Extract domain from LINK or ITEM."
  (or (karakeep-search--kv item 'domain)
      (and (stringp link)
           (when (string-match "//\KATEX_INLINE_OPEN[^/]+\KATEX_INLINE_CLOSE" link)
             (match-string 1 link)))))

(defun karakeep-search--tags->strings (tags)
  "Convert TAGS to list of lowercase strings."
  (cond
   ((null tags) nil)
   ((vectorp tags) (mapcar (lambda (x) (downcase (format "%s" x))) (append tags nil)))
   ((listp tags)   (mapcar (lambda (x) (downcase (format "%s" x))) tags))
   (t nil)))

(defun karakeep-search--format-candidate (it)
  "Format IT as a completion candidate string. Title (if any) before excerpt."
  (let* ((raw-title   (or (karakeep-search--kv it :title)
                          (karakeep-search--kv it 'title) ""))
         (raw-excerpt (or (karakeep-search--kv it :excerpt)
                          (karakeep-search--kv it 'excerpt) ""))
         (t0 (string-trim (or raw-title "")))
         (e0 (string-trim (or raw-excerpt "")))
         (title* (and (not (string-empty-p t0))
                      (karakeep-search--truncate t0 karakeep-search-title-max)))
         (desc*  (and (not (string-empty-p e0))
                      (not (string= e0 t0))
                      (karakeep-search--truncate e0 karakeep-search-excerpt-max)))
         (link    (or (karakeep-search--kv it :link)
                      (karakeep-search--kv it 'link) ""))
         (domain* (and-let* ((d (karakeep-search--domain-of link it)))
                    (propertize d 'face 'shadow)))
         (tags    (karakeep-search--tags->strings
                   (or (karakeep-search--kv it :tags)
                       (karakeep-search--kv it 'tags))))
         (tagstr  (when (and tags (listp tags) tags)
                    (mapconcat
                     (lambda (tag) (propertize (format "#%s" tag) 'face 'karakeep-search-tag))
                     tags " ")))
         (coll-obj (or (karakeep-search--kv it :collection)
                       (karakeep-search--kv it 'collection)))
         (coll-id  (or (karakeep-search--kv it :collectionId)
                       (karakeep-search--kv it 'collectionId)))
         (coll-title
          (or (when coll-id
                (karakeep-search--collection-path-by-id coll-id))
              (when (consp coll-obj)
                (or (karakeep-search--kv coll-obj 'title)
                    (karakeep-search--kv coll-obj 'name)))))
         (collstr (when coll-title
                    (propertize (format "[%s]" coll-title)
                                'face 'karakeep-search-collection))))
    (string-join (delq nil (list collstr title* desc* tagstr domain*)) "  ")))

(defun karakeep-search--apply-results (gen items)
  "Apply ITEMS results if GEN matches current generation."
  (when (= gen karakeep-search--last-gen)
    (karakeep-search--stop-spinner)
    (setq karakeep-search--items items)
    (karakeep-search--safe-exhibit)))

(defun karakeep-search--fetch (gen page parsed)
  "Fetch results for GEN at PAGE using PARSED input."
  (let* ((tags (plist-get parsed :tags))
         (excluded-tags (plist-get parsed :excluded-tags))
         (folders (plist-get parsed :folders))
         (text (plist-get parsed :text))
         (input (string-join
                 (append
                  (mapcar (lambda (tag) (karakeep--quote-tag tag)) tags)
                  (mapcar (lambda (tag) (concat "-" (karakeep--quote-tag tag))) excluded-tags)
                  (mapcar (lambda (folder) (concat "[" folder "]")) folders)
                  (and text (not (string-empty-p text)) (list text)))
                 " ")))
    (karakeep-search--debug "fetch(gen=%s) input=%S" gen input)
    (karakeep-search-bookmarks
     input
     (lambda (items err)
       (when (= gen karakeep-search--last-gen)
         (if err
             (progn (message "Karakeep error: %s" err)
                    (karakeep-search--apply-results gen nil))
           (karakeep-search--apply-results gen items))))
     karakeep-search-page-size page)))

(defun karakeep-search--idle-fire ()
  "Fire API request after idle delay."
  (let* ((input (minibuffer-contents-no-properties))
         (parsed (karakeep-search--parse input)))
    (when (and (equal input karakeep-search--last-input)
               (karakeep-search--meaningful-input-p parsed))
      (karakeep-search--debug "idle fire input=%S" input)
      (karakeep-search--start-spinner)
      (setq karakeep-search--last-gen (1+ karakeep-search--last-gen))
      (karakeep-search--fetch karakeep-search--last-gen 0 parsed))))

(defun karakeep-search--schedule ()
  "Schedule API request only if input actually changed."
  (let ((input (minibuffer-contents-no-properties)))
    (cond
     ((equal input karakeep-search--last-input)
      nil)
     (t
      (setq karakeep-search--last-input input)
      (setq karakeep-search--items nil)
      (karakeep-search--safe-exhibit)
      (setq karakeep-search--last-gen (1+ karakeep-search--last-gen))
      (when karakeep-search--timer
        (cancel-timer karakeep-search--timer)
        (setq karakeep-search--timer nil))
      (let ((parsed (karakeep-search--parse input)))
        (when (karakeep-search--meaningful-input-p parsed)
          (setq karakeep-search--timer
                (run-with-idle-timer karakeep-search-idle-delay nil
                                     #'karakeep-search--idle-fire))))))))

(defun karakeep-search--ui-candidates ()
  "Generate completion candidates for current state."
  (setq karakeep-search--cand-map (make-hash-table :test 'equal))
  (cond
   (karakeep-search--loading
    (let ((spinner (karakeep-search--get-spinner-frame)))
      (list (propertize (format "%s Loading..." spinner)
                        'face 'shadow))))
   (karakeep-search--items
    (let ((i -1))
      (mapcar
       (lambda (it)
         (setq i (1+ i))
         (let ((s (karakeep-search--format-candidate it)))
           (puthash s it karakeep-search--cand-map)
           (propertize s 'karakeep-id (karakeep-search--item-id it)
                       'karakeep-index i)))
       karakeep-search--items)))
   (t '())))

(defun karakeep-search--item-id (it)
  "Extract ID from IT."
  (or (karakeep-search--kv it 'id)
      (karakeep-search--kv it :id)
      (karakeep-search--kv it '_id)
      (karakeep-search--kv it :_id)))

(defun karakeep-search--default-karakeep-url-builder (item)
  "Build default Karakeep app URL for ITEM."
  (let* ((cid (or (karakeep-search--kv item :collectionId)
                  (karakeep-search--kv item 'collectionId)))
         (id  (karakeep-search--item-id item)))
    (cond
     ((eq cid -1) (format "https://app.karakeep.io/#/unsorted/%d" id))
     ((and (integerp cid) (integerp id))
      (format "https://app.karakeep.io/#/collection/%d/%d" cid id))
     ((integerp cid)
      (format "https://app.karakeep.io/#/collection/%d" cid))
     (t "https://app.karakeep.io/"))))

(defun karakeep-search--open-item (item)
  "Open ITEM based on configured action."
  (pcase karakeep-search-enter-action
    ('karakeep (and-let* ((url (funcall karakeep-search-karakeep-url-builder item)))
                 (browse-url url)))
    (_ (and-let* ((url (or (karakeep-search--kv item :link)
                           (karakeep-search--kv item 'link))))
         (browse-url url)))))

(defun karakeep-search--exit (cand)
  "Exit with selected CAND."
  (let* ((idx (cl-position cand (karakeep-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx karakeep-search--items))
                 (and (stringp cand) (gethash cand karakeep-search--cand-map)))))
    (when it (karakeep-search--open-item it))))

;;;###autoload
(defun karakeep-search-toggle-enter-action ()
  "Toggle what pressing RET does in `karakeep-search'.
Switches between opening the original link and opening the Karakeep
app URL for the item."
  (interactive)
  (setq karakeep-search-enter-action (pcase karakeep-search-enter-action
                                       ('link 'karakeep) (_ 'link)))
  (karakeep-search--debug "Enter action → %s" karakeep-search-enter-action))

(defun karakeep-search--minibuffer-setup ()
  "Setup minibuffer for karakeep search."
  (when (minibufferp)
    (add-hook 'post-command-hook #'karakeep-search--schedule nil t)
    (karakeep-search--schedule)
    (karakeep-search--ensure-collections)))

(defun karakeep-search--minibuffer-cleanup ()
  "Cleanup minibuffer after karakeep search."
  (when (minibufferp)
    (remove-hook 'post-command-hook #'karakeep-search--schedule t))
  (when karakeep-search--timer
    (cancel-timer karakeep-search--timer)
    (setq karakeep-search--timer nil))
  (karakeep-search--stop-spinner))

;;;###autoload
(defun karakeep-search ()
  "Start interactive Karakeep search with as-you-type completion."
  (interactive)
  (setq karakeep-search--items nil
        karakeep-search--last-gen 0)
  (minibuffer-with-setup-hook #'karakeep-search--minibuffer-setup
    (unwind-protect
        (let ((table (lambda (_str _pred action)
                       (pcase action
                         ('metadata '(metadata (category . karakeep)))
                         (_ (karakeep-search--ui-candidates))))))
          (and-let* ((res (completing-read "Karakeep: " table nil t)))
            (unless (string-empty-p res)
              (karakeep-search--exit res))))
      (karakeep-search--minibuffer-cleanup))))

(defvar karakeep-search-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'karakeep-search-edit-save)
    (define-key m (kbd "C-c C-k") #'karakeep-search-edit-cancel)
    m)
  "Keymap for Karakeep edit mode.")

(define-derived-mode karakeep-search-edit-mode text-mode "Karakeep-Edit"
  "Major mode for editing a Karakeep item."
  (setq buffer-read-only nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults nil))

(defun karakeep-search--edit-buffer-name (id)
  "Generate buffer name for editing item with ID."
  (format "*Karakeep Edit %s*" id))

(defun karakeep-search--insert-ro-key (label)
  "Insert LABEL as read-only with face."
  (let ((start (point)))
    (insert label)
    (add-text-properties
     start (point)
     `(read-only t
                 front-sticky t
                 rear-nonsticky t
                 face karakeep-search-edit-key
                 font-lock-face karakeep-search-edit-key))))

(defun karakeep-search--open-edit-buffer (item)
  "Open edit buffer for ITEM."
  (let* ((id (karakeep-search--item-id item))
         (buf (get-buffer-create (karakeep-search--edit-buffer-name (or id "unknown"))))
         (title (substring-no-properties (or (karakeep-search--kv item :title)
                                             (karakeep-search--kv item 'title) "")))
         (link  (substring-no-properties (or (karakeep-search--kv item :link)
                                             (karakeep-search--kv item 'link) "")))
         (tags  (karakeep-search--tags->strings
                 (or (karakeep-search--kv item :tags)
                     (karakeep-search--kv item 'tags))))
         (excerpt (substring-no-properties (or (karakeep-search--kv item :excerpt)
                                               (karakeep-search--kv item 'excerpt) ""))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "# Edit Karakeep item. Press C-c C-c to save, C-c C-k to cancel."
                            'face 'karakeep-search-edit-header))
        (insert "\n\n")
        (karakeep-search--insert-ro-key "ID: ")
        (insert (format "%s\n" (or id "unknown")))
        (karakeep-search--insert-ro-key "Title: ")
        (insert (format "%s\n" title))
        (karakeep-search--insert-ro-key "Link: ")
        (insert (format "%s\n" link))
        (karakeep-search--insert-ro-key "Tags: ")
        (insert (mapconcat (lambda (tag)
                             (if (string-match-p "[ ,\"]" tag)
                                 (format "\"%s\"" tag) tag))
                           tags ", "))
        (insert "\n")
        (karakeep-search--insert-ro-key "Excerpt:")
        (insert "\n")
        (insert excerpt))
      (goto-char (point-min))
      (karakeep-search-edit-mode)
      (setq karakeep-search--edit-item-id id))
    (pop-to-buffer buf)
    (and-let* ((win (get-buffer-window buf)))
      (select-window win))
    (goto-char (point-min))
    (re-search-forward "^Title:\\s-*" nil t)
    (when (and (active-minibuffer-window)
               (minibufferp (window-buffer (active-minibuffer-window))))
      (run-at-time 0 nil (lambda ()
                           (when (and (minibufferp)
                                      (fboundp 'abort-recursive-edit))
                             (abort-recursive-edit)))))
    buf))

(defun karakeep-search--parse-edit-buffer ()
  "Parse current edit buffer into update payload."
  (save-excursion
    (goto-char (point-min))
    (let (title link tags excerpt)
      (when (re-search-forward "^Title:\\s-*\\(.*\\)$" nil t)
        (setq title (string-trim (substring-no-properties (match-string 1)))))
      (goto-char (point-min))
      (when (re-search-forward "^Link:\\s-*\\(.*\\)$" nil t)
        (setq link (string-trim (substring-no-properties (match-string 1)))))
      (goto-char (point-min))
      (when (re-search-forward "^Tags:\\s-*\\(.*\\)$" nil t)
        (let* ((raw (substring-no-properties (match-string 1)))
               (parts (split-string raw "\\s-*,\\s-*" t))
               (norm (mapcar (lambda (s)
                               (setq s (string-trim s))
                               (if (and (>= (length s) 2)
                                        (string-prefix-p "\"" s)
                                        (string-suffix-p "\"" s))
                                   (substring s 1 (1- (length s)))
                                 s))
                             parts)))
          (setq tags norm)))
      (goto-char (point-min))
      (when (re-search-forward "^Excerpt:\\s-*$" nil t)
        (setq excerpt (string-trim (buffer-substring-no-properties (point) (point-max)))))
      (seq-filter (lambda (kv)
                    (let ((val (cdr kv)))
                      (and val (if (listp val) (not (null val)) (not (string-empty-p val))))))
                  `((title . ,title)
                    (link . ,link)
                    (tags . ,tags)
                    (excerpt . ,excerpt))))))

;;;###autoload
(defun karakeep-search-embark-open-link (cand)
  "Open the original link for CAND."
  (let* ((idx (cl-position cand (karakeep-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx karakeep-search--items))
                 (and (stringp cand) (gethash cand karakeep-search--cand-map)))))
    (if it
        (and-let* ((url (or (karakeep-search--kv it :link)
                            (karakeep-search--kv it 'link))))
          (browse-url url))
      (message "No item for this candidate."))))

;;;###autoload
(defun karakeep-search-embark-open-karakeep (cand)
  "Open the Karakeep app URL for CAND."
  (let* ((idx (cl-position cand (karakeep-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx karakeep-search--items))
                 (and (stringp cand) (gethash cand karakeep-search--cand-map)))))
    (if it
        (and-let* ((url (funcall karakeep-search-karakeep-url-builder it)))
          (browse-url url))
      (message "No item for this candidate."))))

;;;###autoload
(defun karakeep-search-embark-edit (cand)
  "Open an editable buffer for the Karakeep item represented by CAND."
  (let* ((idx (cl-position cand (karakeep-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx karakeep-search--items))
                 (and (stringp cand) (gethash cand karakeep-search--cand-map)))))
    (if it (karakeep-search--open-edit-buffer it)
      (message "No item for this candidate."))))

;;;###autoload
(defun karakeep-search-embark-delete (cand)
  "Delete the Karakeep item represented by CAND after confirmation."
  (let* ((idx (cl-position cand (karakeep-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx karakeep-search--items))
                 (and (stringp cand) (gethash cand karakeep-search--cand-map)))))
    (if (not it)
        (message "No item for this candidate.")
      (when (y-or-n-p "Delete this Karakeep? ")
        (karakeep-api-request-async
         (format "/karakeep/%d" (karakeep-search--item-id it))
         'DELETE nil nil
         (lambda (_res err)
           (if err (message "Delete failed: %s" err)
             (message "Deleted"))))))))

;;;###autoload
(defun karakeep-search-edit-save ()
  "Save changes from the current Karakeep edit buffer back to Karakeep."
  (interactive)
  (let* ((id karakeep-search--edit-item-id)
         (payload (karakeep-search--parse-edit-buffer))
         (is-creation (null id)))
    (if is-creation
        ;; Create new bookmark
        (progn
          ;; Ensure link is present for creation
          (unless (alist-get 'link payload)
            (user-error "Link is required for creating a bookmark"))
          (karakeep-api-request-async
           "/karakeep" 'POST nil payload
           (lambda (res err)
             (if err 
                 (message "Create failed: %s" err)
               (message "Bookmark created successfully")
               (quit-window t (selected-window))))))
      ;; Update existing bookmark
      (if (not (integerp id))
          (user-error "No valid item ID available for saving")
        (karakeep-api-request-async
         (format "/karakeep/%d" id) 'PUT nil payload
         (lambda (_res err)
           (if err (message "Save failed: %s" err)
             (message "Saved")
             (quit-window t (selected-window)))))))))

;;;###autoload
(defun karakeep-search-edit-cancel ()
  "Abort editing the Karakeep item and close the edit buffer."
  (interactive)
  (quit-window t (selected-window)))

;;;###autoload
(defun karakeep-search-create-bookmark (url &optional title collection)
  "Create a new bookmark with URL.
Optionally specify TITLE and COLLECTION.
Opens edit buffer for further customization."
  (interactive 
   (let ((url (read-string "URL: "
                          (or (thing-at-point 'url)
                              (when (fboundp 'browse-url-url-at-point)
                                (browse-url-url-at-point))
                              ""))))
     (list url)))
  (unless (string-match-p "^https?://" url)
    (user-error "Invalid URL: %s" url))
  (let ((new-item `((title . ,(or title ""))
                    (link . ,url)
                    (tags . ())
                    (excerpt . "")
                    (collection . ,(when collection `((id . ,collection)))))))
    (karakeep-search--open-create-buffer new-item)))

(defun karakeep-search--open-create-buffer (item)
  "Open create buffer for new ITEM."
  (let* ((buf (get-buffer-create "*Karakeep Create*"))
         (title (or (karakeep-search--kv item :title)
                    (karakeep-search--kv item 'title) ""))
         (link  (or (karakeep-search--kv item :link)
                    (karakeep-search--kv item 'link) ""))
         (tags  (karakeep-search--tags->strings
                 (or (karakeep-search--kv item :tags)
                     (karakeep-search--kv item 'tags))))
         (excerpt (or (karakeep-search--kv item :excerpt)
                      (karakeep-search--kv item 'excerpt) "")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "# Create new Karakeep bookmark. Press C-c C-c to save, C-c C-k to cancel."
                            'face 'karakeep-search-edit-header))
        (insert "\n\n")
        (karakeep-search--insert-ro-key "Title: ")
        (insert (format "%s\n" title))
        (karakeep-search--insert-ro-key "Link: ")
        (insert (format "%s\n" link))
        (karakeep-search--insert-ro-key "Tags: ")
        (insert (mapconcat (lambda (tag)
                             (if (string-match-p "[ ,\"]" tag)
                                 (format "\"%s\"" tag) tag))
                           tags ", "))
        (insert "\n")
        (karakeep-search--insert-ro-key "Excerpt:")
        (insert "\n")
        (insert excerpt))
      (goto-char (point-min))
      (karakeep-search-edit-mode)
      (setq karakeep-search--edit-item-id nil)) ; nil indicates creation mode
    (pop-to-buffer buf)
    (and-let* ((win (get-buffer-window buf)))
      (select-window win))
    (goto-char (point-min))
    (re-search-forward "^Title:\\s-*" nil t)
    (when (and (active-minibuffer-window)
               (minibufferp (window-buffer (active-minibuffer-window))))
      (run-at-time 0 nil (lambda ()
                           (when (and (minibufferp)
                                      (fboundp 'abort-recursive-edit))
                             (abort-recursive-edit)))))
    buf))

;;;###autoload
(defun karakeep-search-create-from-browser ()
  "Create a bookmark from the current browser tab (requires browser extension or external tool)."
  (interactive)
  (let ((url (read-string "URL (from clipboard or browser): " 
                          (or (current-kill 0 t)
                              ""))))
    (karakeep-search-create-bookmark url)))

;;;###autoload  
(defun karakeep-search-create-from-kill-ring ()
  "Create a bookmark from URL in kill ring."
  (interactive)
  (let ((url (current-kill 0 t)))
    (when (and url (string-match-p "^https?://" url))
      (karakeep-search-create-bookmark url))))

(with-eval-after-load 'embark
  (defvar embark-keymap-alist)
  (defvar karakeep-search-embark-map
    (let ((m (make-sparse-keymap)))
      (define-key m (kbd "o") #'karakeep-search-embark-open-link)
      (define-key m (kbd "O") #'karakeep-search-embark-open-karakeep)
      (define-key m (kbd "e") #'karakeep-search-embark-edit)
      (define-key m (kbd "D") #'karakeep-search-embark-delete)
      (define-key m (kbd "c") #'karakeep-search-create-from-browser)
      (define-key m (kbd "C") #'karakeep-search-create-from-kill-ring)
      m)
    "Embark keymap for `karakeep' completion category.")
  (add-to-list 'embark-keymap-alist '(karakeep . karakeep-search-embark-map)))

(provide 'karakeep-search)
;;; karakeep-search.el ends here
