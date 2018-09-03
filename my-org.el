;;;; Org directories

(org-starter-def "~/org/"
  :add-to-path t
  :custom-vars (org-directory)
  :config
  (setq hledger-jfile "~/org/journal.ledger")
  (org-starter-def "~/org/journal"
    :custom-vars org-journal-dir
    :config
    (require 'init-org-journal)))

(org-starter-def "~/learning"
  :add-to-path t
  :config
  (setq
   bookmark-default-file "~/learning/emacs-bookmarks"
   org-download-screenshot-file "~/learning/static/screenshots/scrot.png"))

(org-starter-def "~/learning/natural-languages"
  :id language-learning
  :add-to-path t
  :agenda nil
  :refile (:maxlevel . 1)
  :files
  ("english.org")
  ("chinese.org")
  ("japanese.org"))

(org-starter-def "~/private/ledger"
  :add-to-path t)

;;;; Org files

(org-starter-def "scratch.org"
  :key "i"
  :required nil
  :custom-vars org-default-notes-file
  :agenda t
  :refile (:maxlevel . 2))

;; Write-only Org file on my phone
;; You can add entries on the phone but not on any other devices
(org-starter-def "phone.org"
  :agenda t)

;; Write-only Org file on my tablet
(org-starter-def "tablet.org"
  :agenda t)

(org-starter-def "tasks.org"
  :required nil
  :agenda t
  :refile (:maxlevel . 9))

(org-starter-define-file "planner.org"
  :key "p"
  :agenda t
  :required nil
  :refile '(:maxlevel . 3)
  :custom-vars '(org-focus-file)
  :capture `(("gr" "Item to read" entry
              (lambda ()
                (goto-char (org-find-property "CUSTOM_ID" "main-reading-list")))
              ,(akirak/org-capture-entry-template-1
                   "^{Title}"
                 "%i"
                 :todo "TODO"))))

(org-starter-define-file "subjects.org"
  :key "s"
  :agenda t
  :refile '(:maxlevel . 9))

(org-starter-define-file "brainstorming.org"
  :key "b"
  :agenda t
  :refile '(:maxlevel . 9)
  :capture
  '(("R" "Reflection" plain
     (file+function
      (lambda ()
        (goto-char (org-find-property "CUSTOM_ID" "reflection"))
        (end-of-line)
        (re-search-forward "^\*" nil t)
        (end-of-line 0)
        (insert "\n")))
     "%i %U"
     :empty-lines 1)))

(org-starter-def "workflow.org"
  :key "w"
  :agenda nil
  :refile (:maxlevel . 4))

(org-starter-def "icebox.org"
  :key "m"
  :refile (:maxlevel . 9))

(defun akirak/buffer-mode-name (filename)
  (with-current-buffer (find-buffer-visiting filename)
    (string-remove-suffix "-mode" (symbol-name major-mode))))

(org-starter-define-file "code.org"
  :key "c"
  :required nil
  :agenda t
  :refile nil
  ;; Add templates for specific languages.
  :capture `(("c" "Code (org-babel)")
             ("cc" "Capture code" entry
              (file+function akirak/org-reverse-date-tree)
              ,(akirak/org-capture-entry-template-1 "%?%(which-function)"
                 "%a\n\n#+BEGIN_SRC %(akirak/buffer-mode-name \"%F\")\n%i\n#+END_SRC\n"
                 :todo "TODO")
              :clock-in t :clock-resume t)
             ("ce" "Babel code, Emacs Lisp" entry
              (file+function akirak/org-reverse-date-tree)
              ,(akirak/babel-capture-template "emacs-lisp")
              :clock-in t :clock-resume t)
             ("ct" "Topic in code.org" entry
              (file+function akirak/org-reverse-date-tree)
              ,(akirak/org-capture-entry-template-1 "%?" "%K"
                                                    :todo "TOPIC"
                                                    :tags '("topic"))
              :clock-in t :clock-resume t)))

(org-starter-def "posts.org"
  :key "P"
  :required nil
  :agenda t
  :refile (:level . 1))

(org-starter-def "ledger.org"
  :directory "~/private/ledger"
  :custom-vars org-hledger-file
  :config
  (setq org-hledger-tangle-file
        (concat (file-name-sans-extension org-hledger-file) ".ledger")))

(org-starter-def "accounting.org"
  :key "M"
  :directory "~/private/ledger"
  :agenda t
  :refile (:maxlevel . 3))

(org-starter-def "yankpad.org"
  :set-default yankpad-file
  :refile (:level . 1))

(org-starter-def "offtime.org"
  :agenda nil
  :custom-vars org-offtime-file)

;; Set org-default-notes-file
(unless (bound-and-true-p org-default-notes-file)
  (setq org-default-notes-file "~/notes.org")
  (message "Set org-default-notes-file to %s" org-default-notes-file)
  (unless (file-exists-p org-default-notes-file)
    (with-temp-buffer
      (write-file org-default-notes-file))))

;;;; org-agenda custom commands (currently unused)

(akirak/org-add-agenda-custom-command "b" "Main block agenda"
  '((agenda ""
            ((org-agenda-span 1)))
    (tags "CATEGORY=\{scratch\\|phone\\|tablet\}"
          ((org-agenda-overriding-header "Top-level headings in scratch")
           (org-tags-match-list-sublevels nil)))))

;;;; Custom rifle commands

(defun akirak/helm-org-rifle-knowledge-base ()
  (interactive)
  (helm-org-rifle-files (delq nil
                              (mapcar (lambda (fname)
                                        (org-starter-locate-file fname nil t))
                                      '("scratch.org"
                                        "emacs.org"
                                        "workflow.org"
                                        "posts.org"
                                        "brainstorming.org"
                                        "code.org"
                                        "subjects.org"
                                        "planner.org")))))

;;;; Other org options

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate))

(provide 'my-org)
