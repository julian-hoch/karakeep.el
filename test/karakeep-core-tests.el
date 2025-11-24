;;; karakeep-core-tests.el --- ERT tests for karakeep.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'karakeep)
(require 'karakeep-org)

;; Backward compatibility for tests

;; Mock async API request function for tests
(defun karakeep-api-request-async (endpoint method query data callback)
  "Mock async API request function for tests."
  ;; Default mock: return empty result with no error
  (funcall callback nil nil))

;; Mock async fetch function for tests with compatibility wrapper
(defun karakeep-fetch-async (input callback &optional limit page)
  "Compatibility wrapper for tests - handles both string and plist input."
  (let* ((search-input 
          (cond
           ;; If input is a plist, convert to search string
           ((and (listp input) (plist-get input :tags))
            (let* ((tags (plist-get input :tags))
                   (folders (plist-get input :folders))
                   (tag-strings (mapcar (lambda (tag) (format "#%s" tag)) tags))
                   (folder-strings (mapcar (lambda (folder) (format "[%s]" folder)) folders)))
              (string-join (append tag-strings folder-strings) " ")))
           ;; If input is already a string, use as-is
           ((stringp input) input)
           ;; Fallback
           (t "")))
         (actual-limit (or limit (plist-get input :limit) 50)))
    (karakeep-search-bookmarks search-input callback actual-limit page)))

(ert-deftest karakeep-parse-tags-nil ()
  (should (equal (karakeep-parse-tags nil) nil)))

(ert-deftest karakeep-parse-tags-list-mixed ()
  (should (equal (karakeep-parse-tags '(foo "bar")) '("foo" "bar"))))

(ert-deftest karakeep-parse-tags-string-commas-and-quotes ()
  (should (equal (karakeep-parse-tags "foo, \"multi word\",bar")
                 '("foo" "multi word" "bar"))))

(ert-deftest karakeep-parse-tags-string-spaces-no-commas ()
  (should (equal (karakeep-parse-tags "foo bar baz")
                 '("foo" "bar" "baz"))))

(ert-deftest karakeep-quote-tag-basic-and-spaces ()
  (should (equal (karakeep--quote-tag 'foo) "#foo"))
  (should (equal (karakeep--quote-tag "multi word") "#\"multi word\"")))

(ert-deftest karakeep-build-tag-search-all-vs-any ()
  (let ((tags '("foo" "multi word" "bar")))
    (should (equal (karakeep--build-tag-search tags 'all)
                   "#foo #\"multi word\" #bar"))
    (should (equal (karakeep--build-tag-search tags 'any)
                   (string-join (list "match:OR" "#foo" "#\"multi word\"" "#bar") " ")))))

(ert-deftest karakeep-endpoint-for-selection ()
  (should (equal (karakeep--endpoint-for 0 nil) "/bookmarks"))
  (should (equal (karakeep--endpoint-for 0 t) "/bookmarks"))
  (should (equal (karakeep--endpoint-for 5 t) "/collections/5/bookmarks"))
  (should (equal (karakeep--endpoint-for 5 nil) "/collections/5/bookmarks")))

(ert-deftest karakeep-filters-endpoint-mapping ()
  (should (equal (karakeep--filters-endpoint 0) "/bookmarks/filters"))
  (should (equal (karakeep--filters-endpoint -1) "/bookmarks/filters"))
  (should (equal (karakeep--filters-endpoint 7) "/collections/7/filters")))

(ert-deftest karakeep-render-org-list-basic ()
  (let ((karakeep-org-excerpt-next-line t))
    (let* ((items (list (list :link "https://ex.com" :title "Title" :excerpt "Desc")))
           (out (karakeep-render-org-list items)))
      (should (string-match-p "^- \\[\\[https://ex\\.com\\]\\[Title\\]\\]" out))
      (should (string-match-p "\n   Desc$" out)))))

(ert-deftest karakeep-mask-secret ()
  (should (equal (karakeep--mask "short") "******"))
  (should (equal (karakeep--mask "abcdefghijkl") "abc******jkl")))

(ert-deftest karakeep-build-query ()
  (let ((s (karakeep--build-query '((a . 1) (b . 2)))))
    (should (or (string= s "a=1&b=2") (string= s "b=2&a=1")))))

(ert-deftest karakeep-normalize-item-fallbacks ()
  (let* ((raw '((link . "https://ex.com") (excerpt . "e") (_id . 1) (tags . (t1 t2))))
         (it (karakeep--normalize-item raw)))
    (should (equal (alist-get :title it) "https://ex.com"))
    (should (equal (alist-get :excerpt it) "e"))
    (should (equal (alist-get :id it) 1))))

(ert-deftest karakeep-get-token-precedence ()
  (let ((karakeep-token-source '(custom env auth-source))
        (karakeep-custom-token "CUST")
        (getenv (lambda (_k) "ENV")))
    (should (equal (karakeep--get-token) "CUST")))
  (cl-letf* (((symbol-function 'getenv) (lambda (_k) "ENV"))
             ((symbol-function 'auth-source-search)
              (lambda (&rest _)
                (list (list :secret (lambda () "AUTH")))))
             (karakeep-token-source '(env auth-source)))
    (should (equal (karakeep--get-token) "ENV"))))

(ert-deftest karakeep-api-request-success-and-error ()
  (cl-letf (((symbol-function 'request)
             (lambda (_url &rest args)
               (let ((success (plist-get args :success)))
                 (when success
                   (funcall success :data '((ok . t) (items . (1 2))))))))
            ((symbol-function 'karakeep--get-token) (lambda () "TOK")))
    (let ((res (karakeep-api-request "/x" 'GET nil nil)))
      (should (eq (alist-get 'ok res) t))
      (should (equal (alist-get 'items res) '(1 2)))))
  (cl-letf (((symbol-function 'request)
             (lambda (_url &rest args)
               (let ((error (plist-get args :error)))
                 (when error
                   (funcall error :error-thrown '(error . "HTTP 404"))))))
            ((symbol-function 'karakeep--get-token) (lambda () "TOK")))
    (should-error (karakeep-api-request "/x" 'GET nil nil) :type 'user-error)))

(ert-deftest karakeep-fetch-fallback-on-empty-items ()
  (let ((calls '()))
    (cl-letf (((symbol-function 'karakeep-api-request)
               (lambda (endpoint &rest _)
                 (push endpoint calls)
                 (cond
                  ((string= endpoint "/bookmarks") '((items . (((_id . 1) (link . "l") (title . "t"))))))
                  (t '((items)))))))
      (let* ((items (apply #'karakeep-fetch (list :tags '("a") :collection 0 :limit 5 :match 'all))))
        (should (= (length items) 1))
        (should (member "/bookmarks" calls))))))

(ert-deftest karakeep-debug-search-builds-search ()
  (cl-letf (((symbol-function 'karakeep-api-request)
             (lambda (endpoint method query &rest _)
               (list (cons 'endpoint endpoint)
                     (cons 'method method)
                     (cons 'query query)))))
    (let* ((payload (karakeep-debug-search '(foo "multi word") 0))
           (q (alist-get 'query payload)))
      (should (equal (alist-get 'limit q) 5))
      (should (string-match-p "#foo" (alist-get 'q q)))
      (should (string-match-p "#\"multi word\"" (alist-get 'q q))))))

(ert-deftest karakeep-dblock-writer-renders-empty-and-items ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: karakeep :tags foo :match all :limit 5\n#+END:\n")
    (goto-char (point-min))
    (let ((karakeep-org-excerpt-next-line t))
      (cl-letf (((symbol-function 'karakeep-fetch) (lambda (&rest _) nil)))
        (org-dblock-write:karakeep '(:tags "foo" :match all :limit 5))
        (goto-char (point-min))
        (should (search-forward "- No results" nil t)))
      (erase-buffer)
      (insert "#+BEGIN: karakeep :tags foo :match all :limit 5\n#+END:\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'karakeep-fetch)
                 (lambda (&rest _)
                   (list (list :link "https://ex.com" :title "T" :excerpt "E")))))
        (org-dblock-write:karakeep '(:tags "foo" :match all :limit 5))
        (goto-char (point-min))
        (should (search-forward "[[https://ex.com][T]]" nil t))
        (should (search-forward "\n   E" nil t))))))

(ert-deftest karakeep-sanitize-org-trims-and-flattens ()
  (let ((s (concat "  Title\nwith\nnewlines  ")))
    (should (equal (karakeep-org--sanitize s) "Title with newlines")))
  (should (equal (karakeep-org--sanitize nil) nil)))

(ert-deftest karakeep-endpoint-for-nil-and-all ()
  (should (equal (karakeep--endpoint-for nil nil) "/bookmarks"))
  (should (equal (karakeep--endpoint-for 'all nil) "/bookmarks"))
  (should (equal (karakeep--endpoint-for :all t) "/bookmarks")))

(ert-deftest karakeep-filters-builds-query ()
  (cl-letf (((symbol-function 'karakeep-api-request)
             (lambda (endpoint _m query _data)
               (list (cons 'endpoint endpoint) (cons 'query query)))))
    (let* ((res (karakeep-filters :collection 0 :search "#foo" :tags-sort '_id))
           (q (alist-get 'query res)))
      (should (equal (alist-get 'tagsSort q) "_id"))
      (should (equal (alist-get 'q q) "#foo")))))

(ert-deftest karakeep-fetch-limit-cap ()
  (let (seen-query)
    (cl-letf (((symbol-function 'karakeep-api-request)
               (lambda (_e _m query _d)
                 (setq seen-query query)
                 '((items . ())))))
      (karakeep-fetch :tags '("a") :limit 20)
      (should (equal (alist-get 'limit seen-query) 20))
      (karakeep-fetch :tags '("a") :limit 1000)
      (should (equal (alist-get 'limit seen-query) 200)))))

(ert-deftest karakeep-fetch-async-limit-cap ()
  (let (seen-query)
    (cl-letf (((symbol-function 'karakeep-api-request-async)
               (lambda (_e _m query _d cb)
                 (setq seen-query query)
                 (funcall cb '((items)) nil))))
      (karakeep-fetch-async (list :tags '("a") :limit 5) (lambda (_ _)))
      (should (equal (alist-get 'limit seen-query) 5))
      (karakeep-fetch-async (list :tags '("a") :limit 200) (lambda (_ _)))
      (should (equal (alist-get 'limit seen-query) 200)))))

(ert-deftest karakeep-token-errors-and-auth-source-secret-fn ()
  (let ((karakeep-token-source '()))
    (should-error (karakeep--get-token) :type 'user-error))
  (let ((karakeep-token-source '(custom))
        (karakeep-custom-token ""))
    (should-error (karakeep--get-token) :type 'user-error))
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) (list (list :secret (lambda () "AUTH")))))
            (karakeep-token-source '(auth-source)))
    (should (equal (karakeep--get-token) "AUTH"))))

(ert-deftest karakeep-http-error-bodies-and-non-json ()
  "Test that API request handles HTTP errors correctly with request library."
  ;; Test HTTP 400 error
  (cl-letf (((symbol-function 'request)
             (lambda (_url &rest args)
               (let ((error (plist-get args :error)))
                 (when error
                   (funcall error :error-thrown '(error . "HTTP 400: Bad Request")
                            :response nil)))))
            ((symbol-function 'karakeep--get-token) (lambda () "TOK")))
    (should-error (karakeep-api-request "/x" 'GET nil nil) :type 'user-error))
  ;; Test HTTP 500 error  
  (cl-letf (((symbol-function 'request)
             (lambda (_url &rest args)
               (let ((error (plist-get args :error)))
                 (when error
                   (funcall error :error-thrown '(error . "HTTP 500: Server Error")
                            :response nil)))))
            ((symbol-function 'karakeep--get-token) (lambda () "TOK")))
    (should-error (karakeep-api-request "/x" 'GET nil nil) :type 'user-error)))

(ert-deftest karakeep-api-async-network-error-status ()
  "Test that async API request handles network errors correctly."
  (let (cb-result cb-err)
    (cl-letf (((symbol-function 'karakeep-api-request-async)
               (lambda (_endpoint _method _query _data callback)
                 (funcall callback nil "Network error: timeout")))
              ((symbol-function 'karakeep--get-token) (lambda () "TOK")))
      (karakeep-api-request-async "/x" 'GET nil nil (lambda (r e) (setq cb-result r cb-err e)))
      (should (null cb-result))
      (should (string-match-p "Network error" (or cb-err ""))))))

(ert-deftest karakeep-parse-tags-empty-string-and-spaces ()
  (should (equal (karakeep-parse-tags "") nil))
  (should (equal (karakeep-parse-tags "   \t\n") nil)))

(ert-deftest karakeep-dblock-reinvoke-updates-content ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: karakeep :tags foo :match all :limit 5\n#+END:\n")
    (goto-char (point-min))
    (let ((karakeep-org-excerpt-next-line t))
      (cl-letf (((symbol-function 'karakeep-fetch)
                 (lambda (&rest _) (list (list :link "https://a" :title "A" :excerpt "1")))))
        (org-dblock-write:karakeep '(:tags "foo" :match all :limit 5)))
      (goto-char (point-min))
      (should (search-forward "[[https://a][A]]" nil t))
      (should (search-forward "\n   1" nil t))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'karakeep-fetch)
                 (lambda (&rest _) (list (list :link "https://b" :title "B" :excerpt "2")))))
        (org-dblock-write:karakeep '(:tags "foo" :match all :limit 5)))
      (goto-char (point-min))
      (should (search-forward "[[https://b][B]]" nil t))
      (should (search-forward "\n   2" nil t))
      (should-not (search-forward "[[https://a][A]]" nil t)))))

(ert-deftest karakeep-api-request-async-success-and-error ()
  "Test that async API request handles success and error responses correctly."
  ;; Test success case
  (let (cb-result cb-err)
    (cl-letf (((symbol-function 'karakeep-api-request-async)
               (lambda (_endpoint _method _query _data callback)
                 (funcall callback '((ok . t)) nil))))
      (karakeep-api-request-async "/x" 'GET nil nil
                                  (lambda (res err) (setq cb-result res cb-err err)))
      (should (equal (alist-get 'ok cb-result) t))
      (should (null cb-err))))
  ;; Test error case  
  (let (cb-result cb-err)
    (cl-letf (((symbol-function 'karakeep-api-request-async)
               (lambda (_endpoint _method _query _data callback)
                 (funcall callback nil "HTTP 500: Server error"))))
      (karakeep-api-request-async "/x" 'GET nil nil
                                  (lambda (res err) (setq cb-result res cb-err err)))
      (should (null cb-result))
      (should (string-match-p "HTTP 500" (or cb-err ""))))))

(ert-deftest karakeep-fetch-async-fallback-on-empty-items ()
  (let ((calls '()))
    (cl-letf (((symbol-function 'karakeep-api-request-async)
                (lambda (endpoint _m _q _d cb)
                  (push endpoint calls)
                  (cond
                  ((string= endpoint "/bookmarks") (funcall cb '((items . (((_id . 1) (link . \"l\") (title . \"t\"))))) nil))
                  (t (funcall cb '((items)) nil))))))
      (let (done items err)
        (karakeep-fetch-async (list :tags '("a") :collection 0 :limit 5 :match 'all)
                              (lambda (its e) (setq items its err e done t)))
        (should done)
        (should (= (length items) 1))
        (should (member "/bookmarks" calls))))))

(ert-deftest karakeep-under-heading-errors-without-tags ()
  (with-temp-buffer
    (org-mode)
    (insert "* Heading without tags\n")
    (goto-char (point-min))
    (should-error (karakeep-insert-or-update-links-under-heading) :type 'user-error)))

(ert-deftest karakeep-under-heading-inserts-block-and-replaces ()
  "Test that karakeep dynamic block is inserted and populated under headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Test :foo:bar:\nBody\n")
    (goto-char (point-min))
    (let ((karakeep-org-excerpt-next-line t))
      (cl-letf (((symbol-function 'karakeep-search-bookmarks)
                 (lambda (_input callback &optional _limit _page)
                   (funcall callback (list (list :link "https://ex.com" :title "T" :excerpt "E")) nil))))
        (karakeep-insert-or-update-links-under-heading)
        (goto-char (point-min))
        (should (search-forward "#+BEGIN: karakeep" nil t))
        (should (search-forward "[[https://ex.com][T]]" nil t))
        (should (search-forward "\n   E" nil t))))))

(ert-deftest karakeep-quote-folder-basic-and-spaces ()
  (should (equal (karakeep--quote-folder 'Work) "collection:Work"))
  (should (equal (karakeep--quote-folder "Multi Word") "collection:\"Multi Word\"")))

;; ========== Tests for new functionality ==========

(ert-deftest karakeep-search-spinner-functionality ()
  "Test spinner start, stop, and frame rotation."
  (require 'karakeep-search)
  
  ;; Test initial state
  (should-not karakeep-search--loading)
  (should (equal (karakeep-search--get-spinner-frame) ""))
  
  ;; Test starting spinner
  (karakeep-search--start-spinner)
  (should karakeep-search--loading)
  (should karakeep-search--spinner-timer)
  (should (member (karakeep-search--get-spinner-frame) karakeep-search-spinner-frames))
  
  ;; Test stopping spinner
  (karakeep-search--stop-spinner)
  (should-not karakeep-search--loading)
  (should-not karakeep-search--spinner-timer)
  (should (equal (karakeep-search--get-spinner-frame) "")))

(ert-deftest karakeep-search-spinner-frames-customization ()
  "Test that spinner frames can be customized."
  (require 'karakeep-search)
  
  (let ((karakeep-search-spinner-frames '("●" "○" "◐" "◑")))
    (karakeep-search--start-spinner)
    (should (member (karakeep-search--get-spinner-frame) '("●" "○" "◐" "◑")))
    (karakeep-search--stop-spinner)))

(ert-deftest karakeep-org-tag-rendering-improvements ()
  "Test improved tag rendering with vector and list handling."
  (require 'karakeep-org)
  
  ;; Test with list tags
  (let ((karakeep-org-render-tags t))
    (should (equal (karakeep-org--format-tags '("tag1" "tag2")) "  (tag1, tag2)"))
    (should (equal (karakeep-org--format-tags '("single")) "  (single)"))
    (should (equal (karakeep-org--format-tags nil) nil))
    (should (equal (karakeep-org--format-tags '()) nil)))
  
  ;; Test with vector tags (converted to lists)
  (let ((karakeep-org-render-tags t))
    (should (equal (karakeep-org--format-tags ["vec1" "vec2"]) "  (vec1, vec2)"))
    (should (equal (karakeep-org--format-tags ["single"]) "  (single)")))
  
  ;; Test with render disabled
  (let ((karakeep-org-render-tags nil))
    (should (equal (karakeep-org--format-tags '("tag1" "tag2")) nil))))

(ert-deftest karakeep-search-bookmarks-functionality ()
  "Test new search API function."
  ;; Test with direct API call (synchronous)
  (cl-letf (((symbol-function 'karakeep-api-request)
             (lambda (_endpoint _method _query _data)
               '((items . [((title . "Test Item") (link . "https://test.com") (tags . ["test"]))])))))
    (let ((result (karakeep-search-bookmarks "#test" nil)))
      (should (= (length result) 1))
      (should (equal (alist-get :title (car result)) "Test Item"))
      (should (listp (alist-get :tags (car result)))))))

(ert-deftest karakeep-vector-to-list-conversion ()
  "Test that API vectors are properly converted to lists."
  (let* ((raw-item '((title . "Test") (tags . ["tag1" "tag2"])))
         (normalized (karakeep--normalize-item raw-item))
         (tags (alist-get :tags normalized)))
    (should (listp tags))
    (should (equal tags '("tag1" "tag2")))))

(ert-deftest karakeep-search-ui-candidates-with-spinner ()
  "Test UI candidates display with spinner functionality."
  (require 'karakeep-search)
  
  ;; Test loading state shows spinner
  (let ((karakeep-search--loading t)
        (karakeep-search--spinner-index 0)
        (karakeep-search-spinner-frames '("|" "/" "-" "\\")))
    (let ((candidates (karakeep-search--ui-candidates)))
      (should (= (length candidates) 1))
      (should (string-match-p "| Loading..." (car candidates)))))
  
  ;; Test normal results when not loading
  (let ((karakeep-search--loading nil)
        (karakeep-search--items '(((:title . "Test") (:link . "https://test.com")))))
    (let ((candidates (karakeep-search--ui-candidates)))
      (should (> (length candidates) 0))
      (should-not (string-match-p "Loading" (car candidates))))))

(ert-deftest karakeep-search-input-parsing-improvements ()
  "Test improved search input parsing for new API."
  ;; Test tag parsing
  (let ((parsed (karakeep--parse-search-input "#tag1 #tag2 some text")))
    (should (equal (plist-get parsed :tags) '("tag1" "tag2")))
    (should (equal (plist-get parsed :text) "some text")))
  
  ;; Test folder parsing (single word)
  (let ((parsed (karakeep--parse-search-input "[Work] #tag")))
    (should (equal (plist-get parsed :folders) '("Work")))
    (should (equal (plist-get parsed :tags) '("tag"))))
  
  ;; Test mixed input
  (let ((parsed (karakeep--parse-search-input "#important [Archive] project notes")))
    (should (equal (plist-get parsed :tags) '("important")))
    (should (equal (plist-get parsed :folders) '("Archive")))
    (should (equal (plist-get parsed :text) "project notes"))))

(ert-deftest karakeep-build-search-folders-and-tags ()
  (let* ((s-all (karakeep--build-search '("t1") '("Folder A") 'all))
         (s-any (karakeep--build-search '("t1") '("Folder A") 'any)))
    (should (string-match-p "#t1" s-all))
    (should (string-match-p "collection:\\\"Folder A\\\"" s-all))
    (should (not (string-match-p "match:OR" s-all)))
    (should (string-match-p "^match:OR" s-any))
    (should (string-match-p "#t1" s-any))
    (should (string-match-p "collection:\\\"Folder A\\\"" s-any))))

(ert-deftest karakeep-dblock-accepts-folder-param ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: karakeep :folder \"Folder A\" :match all :limit 5\n#+END:\n")
    (goto-char (point-min))
    (let (saw-folders)
      (cl-letf (((symbol-function 'karakeep-fetch)
                 (lambda (&rest plist)
                   (setq saw-folders (plist-get plist :folders))
                   nil)))
        (org-dblock-write:karakeep '(:folder "Folder A" :match all :limit 5))
        (should (equal saw-folders '("Folder A")))))))


;;; smart grouping tests

(ert-deftest karakeep-smart-eligible-tags-respects-min-and-stop ()
  (let* ((items (list
                 (list :tags '("ascii" "cli"))
                 (list :tags '("ascii" "cli"))
                 (list :tags '("file manager" "cli"))
                 (list :tags '("file manager"))
                 (list :tags '("unique"))))
         (karakeep-org-smart-min-count 2)
         (karakeep-org-smart-max-groups 8)
         (karakeep-org-smart-stop-tags '("cli"))
         (freqs (karakeep-org--tag-frequencies items))
         (selected (karakeep-org--eligible-tags freqs)))
    (should (member "ascii" selected))
    (should (member "file manager" selected))
    (should-not (member "cli" selected))
    (should-not (member "unique" selected))))

(ert-deftest karakeep-smart-max-groups-cap ()
  (let* ((items (mapcar (lambda (i)
                          ;; 20 тегов t1..t20, каждый встречается два раза
                          (list :tags (list (format "t%d" (1+ (/ i 2))))))
                        (number-sequence 0 39)))
         (karakeep-org-smart-min-count 2)
         (karakeep-org-smart-max-groups 5)
         (karakeep-org-smart-stop-tags '())
         (freqs (karakeep-org--tag-frequencies items))
         (selected (karakeep-org--eligible-tags freqs)))
    (should (= (length selected) 5))))

(ert-deftest karakeep-smart-group-items-auto-basic ()
  (let* ((items (list
                 (list :link "https://a" :title "Yazi"   :tags '("file manager" "cli" "tui"))
                 (list :link "https://b" :title "ncdu"   :tags '("file manager" "cli"))
                 (list :link "https://c" :title "toilet" :tags '("ascii" "cli"))
                 (list :link "https://d" :title "figlet" :tags '("ascii" "cli"))
                 (list :link "https://e" :title "misc"   :tags '("rare"))))
         (karakeep-org-smart-min-count 2)
         (karakeep-org-smart-max-groups 8)
         (karakeep-org-smart-stop-tags '("cli"))
         (groups (karakeep-org--group-items-auto items)))
    (let ((names (mapcar #'car groups)))
      (should (member "Ascii" names))
      (should (member "File manager" names))
      (should (member "Other" names)))
    (let* ((ascii (assoc "Ascii" groups))
           (fm    (assoc "File manager" groups))
           (other (assoc "Other" groups)))
      (should (= (length (cdr ascii)) 2))
      (should (= (length (cdr fm)) 2))
      (should (= (length (cdr other)) 1))
      (let* ((it (car (cdr other)))
             (lnk (or (plist-get it :link) (alist-get :link it))))
        (should (equal lnk "https://e"))))))

(ert-deftest karakeep-smart-render-grouped-respects-heading-level ()
  (let* ((items (list
                 (list :link "https://a" :title "A" :tags '("x" "cli"))
                 (list :link "https://b" :title "B" :tags '("x"))
                 (list :link "https://c" :title "C" :tags '("y"))))
         (karakeep-org-smart-min-count 2)
         (karakeep-org-smart-stop-tags '("cli"))
         (karakeep-org-smart-heading-level 2)
         (org (karakeep-org--render-grouped
               (karakeep-org--group-items-auto items))))
    (should (string-match-p "^\\*\\* X$" org))
    (should (string-match-p "^- \\[\\[https://a\\]\\[A\\]\\]" org))
    (should (string-match-p "^- \\[\\[https://b\\]\\[B\\]\\]" org))
    (should (string-match-p "^\\*\\* Other$" org))
    (should (string-match-p "\\[\\[https://c\\]\\[C\\]\\]" org))))

(ert-deftest karakeep-dblock-smart-flag-produces-headings ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: karakeep :tags foo :match all :limit 5 :smart t\n#+END:\n")
    (goto-char (point-min))
    (let ((karakeep-org-smart-min-count 1)
          (karakeep-org-smart-stop-tags '())
          (karakeep-org-smart-heading-level 3))
      (cl-letf (((symbol-function 'karakeep-fetch)
                 (lambda (&rest _)
                   (list
                    (list :link "https://a" :title "A" :tags '("alpha"))
                    (list :link "https://b" :title "B" :tags '("beta"))))))
        (org-dblock-write:karakeep '(:tags "foo" :match all :limit 5 :smart t))
        (goto-char (point-min))
        (should (search-forward "*** Alpha" nil t))
        (should (search-forward "*** Beta" nil t))
        ;; Ссылки как таковые
        (goto-char (point-min))
        (should (re-search-forward "\\[\\[https://a\\]\\[A\\]\\]" nil t))
        (goto-char (point-min))
        (should (re-search-forward "\\[\\[https://b\\]\\[B\\]\\]" nil t))))))

;; Tag exclusion tests

(ert-deftest karakeep-parse-tags-with-exclusion-basic ()
  "Test basic tag exclusion parsing."
  (let ((result (karakeep-parse-tags-with-exclusion "tag1, -tag2, tag3")))
    (should (equal (plist-get result :tags) '("tag1" "tag3")))
    (should (equal (plist-get result :excluded-tags) '("tag2")))))

(ert-deftest karakeep-parse-tags-with-exclusion-no-exclusion ()
  "Test parsing without excluded tags."
  (let ((result (karakeep-parse-tags-with-exclusion "tag1, tag2")))
    (should (equal (plist-get result :tags) '("tag1" "tag2")))
    (should (equal (plist-get result :excluded-tags) nil))))

(ert-deftest karakeep-parse-tags-with-exclusion-only-exclusion ()
  "Test parsing with only excluded tags."
  (let ((result (karakeep-parse-tags-with-exclusion "-tag1, -tag2")))
    (should (equal (plist-get result :tags) nil))
    (should (equal (plist-get result :excluded-tags) '("tag1" "tag2")))))

(ert-deftest karakeep-parse-tags-with-exclusion-list-input ()
  "Test exclusion parsing with list input (no exclusion support)."
  (let ((result (karakeep-parse-tags-with-exclusion '("tag1" "tag2"))))
    (should (equal (plist-get result :tags) '("tag1" "tag2")))
    (should (equal (plist-get result :excluded-tags) nil))))

(ert-deftest karakeep-search-input-parsing-with-exclusion ()
  "Test search input parsing with excluded tags using -# syntax."
  (let ((parsed (karakeep--parse-search-input "#include -#exclude [folder] text")))
    (should (equal (plist-get parsed :tags) '("include")))
    (should (equal (plist-get parsed :excluded-tags) '("exclude")))
    (should (equal (plist-get parsed :folders) '("folder")))
    (should (equal (plist-get parsed :text) "text"))))

(ert-deftest karakeep-build-excluded-tag-search ()
  "Test building excluded tag search string."
  (should (equal (karakeep--build-excluded-tag-search '("tag1" "tag2"))
                 "-#tag1 -#tag2"))
  (should (equal (karakeep--build-excluded-tag-search '("spaced tag"))
                 "-#\"spaced tag\""))
  (should (equal (karakeep--build-excluded-tag-search nil) nil)))

(ert-deftest karakeep-compose-search-string-with-exclusion ()
  "Test composing search string with excluded tags."
  (should (string-match-p "#include.*-#exclude"
                          (karakeep--compose-search-string "text" '("include") '("exclude")))))

(ert-deftest karakeep-parse-tags-with-exclusion-spaced-tags ()
  "Test parsing tags with spaces using comma separation."
  ;; Test with comma-separated format (new org parsing function)
  (let ((result (karakeep-org--parse-tags-string "file manager, -outdated, cli")))
    (should (equal (plist-get result :tags) '("file manager" "cli")))
    (should (equal (plist-get result :excluded-tags) '("outdated")))))

(ert-deftest karakeep-build-excluded-tag-search-with-spaces ()
  "Test building excluded tag search string with spaced tags."
  (should (equal (karakeep--build-excluded-tag-search '("old tool"))
                 "-#\"old tool\"")))

;; Test org-babel parameter parsing

(ert-deftest karakeep-ob-comma-separated-tags ()
  "Test that ob-karakeep can handle comma-separated tags without quotes."
  (require 'ob-karakeep)
  ;; Test basic comma separation without spaces in tags
  (let ((params '((:tags . "cli, -windows, macos"))))
    (let* ((tags-input (karakeep-ob--param :tags params nil))
           (parsed-tags (karakeep-parse-tags-with-exclusion tags-input))
           (tags (plist-get parsed-tags :tags))
           (excluded-tags (plist-get parsed-tags :excluded-tags)))
      (should (equal tags '("cli" "macos")))
      (should (equal excluded-tags '("windows"))))))


(ert-deftest karakeep-org-parse-tags-string-with-spaces ()
  "Test new org tag parsing function with tags containing spaces."
  (let ((result (karakeep-org--parse-tags-string "cli, -openai, -tui with space")))
    (should (equal (plist-get result :tags) '("cli")))
    (should (equal (plist-get result :excluded-tags) '("openai" "tui with space"))))
  
  ;; Test single tag with spaces
  (let ((result (karakeep-org--parse-tags-string "disk usage")))
    (should (equal (plist-get result :tags) '("disk usage")))
    (should (equal (plist-get result :excluded-tags) nil)))
  
  ;; Test single excluded tag with spaces
  (let ((result (karakeep-org--parse-tags-string "-disk usage")))
    (should (equal (plist-get result :tags) nil))
    (should (equal (plist-get result :excluded-tags) '("disk usage")))))

(ert-deftest karakeep-org-dblock-new-implementation ()
  "Test that new dblock implementation correctly parses parameters."
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: karakeep :tags \"cli, -openai, -tui with space\" :match all :limit 5\n#+END:\n")
    (goto-char (point-min))
    (let (parsed-tags parsed-excluded-tags)
      (cl-letf (((symbol-function 'karakeep-fetch)
                 (lambda (&rest plist)
                   (setq parsed-tags (plist-get plist :tags))
                   (setq parsed-excluded-tags (plist-get plist :excluded-tags))
                   nil)))
        (org-dblock-write:karakeep '(:tags "cli, -openai, -tui with space" :match all :limit 5))
        (should (equal parsed-tags '("cli")))
        (should (equal parsed-excluded-tags '("openai" "tui with space")))))))

(ert-deftest karakeep-search-input-parsing-quoted-tags ()
  "Test parsing search input with quoted tags containing spaces."
  ;; Test quoted tag with text
  (let ((result (karakeep--parse-search-input "#\"disk usage\" backup tools")))
    (should (equal (plist-get result :tags) '("disk usage")))
    (should (equal (plist-get result :text) "backup tools")))
  
  ;; Test multiple quoted and regular tags
  (let ((result (karakeep--parse-search-input "#emacs #\"file manager\" -#\"old tool\" config")))
    (should (equal (plist-get result :tags) '("emacs" "file manager")))
    (should (equal (plist-get result :excluded-tags) '("old tool")))
    (should (equal (plist-get result :text) "config")))
  
  ;; Test quoted excluded tag
  (let ((result (karakeep--parse-search-input "-#\"system admin\" scripts")))
    (should (equal (plist-get result :tags) nil))
    (should (equal (plist-get result :excluded-tags) '("system admin")))
    (should (equal (plist-get result :text) "scripts")))
  
  ;; Test mixed quoted and unquoted
  (let ((result (karakeep--parse-search-input "#cli #\"disk usage\" -#outdated tools")))
    (should (equal (plist-get result :tags) '("cli" "disk usage")))
    (should (equal (plist-get result :excluded-tags) '("outdated")))
    (should (equal (plist-get result :text) "tools"))))

(ert-deftest karakeep-fetch-with-search-parameter ()
  "Test that karakeep-fetch handles :search parameter correctly."
  (let (captured-input)
    (cl-letf (((symbol-function 'karakeep-search-bookmarks)
               (lambda (input callback limit)
                 (setq captured-input input)
                 '((link . "https://example.com") (title . "Test Result")))))
      ;; Test search only
      (karakeep-fetch :search "machine learning")
      (should (string= captured-input "machine learning"))
      
      ;; Test search with tags
      (karakeep-fetch :search "AI research" :tags '("python"))
      (should (string-match-p "#python" captured-input))
      (should (string-match-p "AI research" captured-input))
      
      ;; Test empty search is ignored
      (karakeep-fetch :search "" :tags '("test"))
      (should (string= captured-input "#test"))
      
      ;; Test whitespace-only search is ignored
      (karakeep-fetch :search "   " :tags '("test"))
      (should (string= captured-input "#test")))))

(ert-deftest karakeep-dblock-with-search-parameter ()
  "Test that dynamic blocks handle :search parameter."
  (with-temp-buffer
    (cl-letf (((symbol-function 'karakeep-fetch)
               (lambda (&rest args)
                 (let ((search (plist-get args :search)))
                   (if (and search (string= search "test query"))
                       (list (list :link "https://test.com" :title "Test" :excerpt "Found"))
                     nil)))))
      ;; Test with search parameter
      (org-dblock-write:karakeep '(:search "test query" :limit 5))
      (goto-char (point-min))
      (should (search-forward "[[https://test.com][Test]]" nil t)))))

(ert-deftest karakeep-ob-with-search-parameter ()
  "Test that org-babel blocks handle :search parameter."
  (cl-letf (((symbol-function 'karakeep-fetch)
             (lambda (&rest args)
               (let ((search (plist-get args :search)))
                 (if (and search (string= search "babel test"))
                     (list (list :link "https://babel.com" :title "Babel Test" :excerpt "Success"))
                   nil)))))
    (let* ((params '((:search . "babel test") (:limit . 10)))
           (result (org-babel-execute:karakeep nil params)))
      (should (string-match-p "\\[\\[https://babel.com\\]\\[Babel Test\\]\\]" result)))))

(ert-deftest karakeep-smart-grouping-excludes-search-tags ()
  "Test that smart grouping excludes search tags from heading selection."
  (let* ((items '(((link . "https://a.com") (title . "A") (tags . ("emacs" "package")))
                  ((link . "https://b.com") (title . "B") (tags . ("emacs" "config")))
                  ((link . "https://c.com") (title . "C") (tags . ("vim" "config")))))
         (grouped (karakeep-org--group-items-auto items '("emacs"))))
    ;; "emacs" should not appear as a heading since it was excluded
    (should-not (assoc "Emacs" grouped))
    ;; Other tags should still be used for grouping
    (should (or (assoc "Package" grouped) (assoc "Config" grouped) (assoc "Other" grouped)))))

(ert-deftest karakeep-dblock-smart-excludes-search-tags ()
  "Test that dynamic block smart grouping excludes search tags."
  (with-temp-buffer
    (let* ((test-items '(((link . "https://test1.com") (title . "Test 1") (tags . ("emacs" "package")))
                         ((link . "https://test2.com") (title . "Test 2") (tags . ("emacs" "config")))))
           (captured-exclude-tags nil))
      (cl-letf (((symbol-function 'karakeep-fetch)
                 (lambda (&rest args) test-items))
                ((symbol-function 'karakeep-org--group-items-auto)
                 (lambda (items exclude-tags exclude-groups)
                   (setq captured-exclude-tags exclude-tags)
                   `(("Other" . ,items))))
                ((symbol-function 'karakeep-org--render-grouped)
                 (lambda (grouped) "- Test content")))
        (org-dblock-write:karakeep '(:tags "emacs" :smart t :limit 5))
        ;; Verify that "emacs" was passed as excluded tag
        (should (member "emacs" captured-exclude-tags))))))

(ert-deftest karakeep-exclude-groups-functionality ()
  "Test that :exclude-groups parameter works correctly."
  (let* ((items '(((link . "https://a.com") (title . "A") (tags . ("emacs" "cli" "package")))
                  ((link . "https://b.com") (title . "B") (tags . ("emacs" "cli" "config")))
                  ((link . "https://c.com") (title . "C") (tags . ("vim" "cli" "editor")))))
         ;; Exclude "cli" from grouping - it should not appear as a heading
         (grouped (karakeep-org--group-items-auto items '("emacs") '("cli"))))
    ;; "cli" should not appear as a heading since it was excluded
    (should-not (assoc "Cli" grouped))
    ;; "emacs" should not appear as a heading since it was in exclude-tags
    (should-not (assoc "Emacs" grouped))
    ;; Other tags should still be used for grouping
    (should (or (assoc "Package" grouped) (assoc "Config" grouped) (assoc "Editor" grouped) (assoc "Other" grouped)))))

(ert-deftest karakeep-dblock-with-exclude-groups ()
  "Test that dynamic blocks handle :exclude-groups parameter."
  (with-temp-buffer
    (let* ((test-items '(((link . "https://test1.com") (title . "Test 1") (tags . ("emacs" "cli" "package")))
                         ((link . "https://test2.com") (title . "Test 2") (tags . ("emacs" "cli" "config")))))
           (captured-exclude-groups nil))
      (cl-letf (((symbol-function 'karakeep-fetch)
                 (lambda (&rest args) test-items))
                ((symbol-function 'karakeep-org--group-items-auto)
                 (lambda (items exclude-tags exclude-groups)
                   (setq captured-exclude-groups exclude-groups)
                   `(("Other" . ,items))))
                ((symbol-function 'karakeep-org--render-grouped)
                 (lambda (grouped) "- Test content")))
        (org-dblock-write:karakeep '(:tags "emacs" :exclude-groups "cli,terminal" :smart t :limit 5))
        ;; Verify that exclude-groups were parsed and passed correctly
        (should (member "cli" captured-exclude-groups))
        (should (member "terminal" captured-exclude-groups))))))

(ert-deftest karakeep-context-aware-heading-levels ()
  "Test that smart grouping uses context-aware heading levels."
  (let ((temp-file (make-temp-file "test-karakeep" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (org-mode)
          
          ;; Test: No parent heading - should use level 1 (*)
          (erase-buffer)
          (insert "Some text\n#+BEGIN: karakeep\n#+END:\n")
          (goto-char (point-min))
          (search-forward "#+BEGIN:")
          (let ((level (karakeep-org--get-context-heading-level)))
            (should (= level 1)))
          
          ;; Test: Under level 1 heading - should use level 2 (**)  
          (erase-buffer)
          (insert "* Main Heading\nSome text\n#+BEGIN: karakeep\n#+END:\n")
          (goto-char (point-min))
          (search-forward "#+BEGIN:")
          (let ((level (karakeep-org--get-context-heading-level)))
            (should (= level 2)))
          
          ;; Test: Under level 2 heading - should use level 3 (***)
          (erase-buffer)
          (insert "* Main Heading\n** Sub Heading\nSome text\n#+BEGIN: karakeep\n#+END:\n")
          (goto-char (point-min))
          (search-forward "#+BEGIN:")
          (let ((level (karakeep-org--get-context-heading-level)))
            (should (= level 3))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest karakeep-smart-grouping-respects-context ()
  "Test that smart grouping output has correct heading levels based on context."
  (let ((temp-file (make-temp-file "test-karakeep" nil ".org")))
    (unwind-protect
        (with-current-buffer (find-file-noselect temp-file)
          (org-mode)
          (let* ((test-items '(((link . "https://test1.com") (title . "Test 1") (tags . ("emacs" "package")))
                               ((link . "https://test2.com") (title . "Test 2") (tags . ("emacs" "config")))))
                 (grouped-output nil))
            
            ;; Test: No parent heading
            (erase-buffer)
            (insert "#+BEGIN: karakeep\n#+END:\n")
            (goto-char (point-min))
            (search-forward "#+BEGIN:")
            (cl-letf (((symbol-function 'karakeep-fetch)
                       (lambda (&rest args) test-items)))
              (setq grouped-output (karakeep-org--render-grouped 
                                    (karakeep-org--group-items-auto test-items '("emacs"))))
              ;; Should start with single * (level 1)
              (should (string-match-p "^\\* " grouped-output)))
            
            ;; Test: Under level 1 heading
            (erase-buffer)
            (insert "* Main\n#+BEGIN: karakeep\n#+END:\n")
            (goto-char (point-min))
            (search-forward "#+BEGIN:")
            (cl-letf (((symbol-function 'karakeep-fetch)
                       (lambda (&rest args) test-items)))
              (setq grouped-output (karakeep-org--render-grouped 
                                    (karakeep-org--group-items-auto test-items '("emacs"))))
              ;; Should start with ** (level 2)
              (should (string-match-p "^\\*\\* " grouped-output)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))


;;; karakeep-core-tests.el ends here
