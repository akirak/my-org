;;;; Org directories

;;;;; ~/org
;; This is my main repository for Org files. It is synchronized with
;; Android devices using Syncthing and periodically backed up by Git.
;; On Android, I use Orgzly to access Org files in this directory.
(org-starter-def "~/org/"
  :add-to-path t
  :custom-vars (org-directory)
  :config
  (setq akirak/init-time-log-file
        (format "~/org/log/emacs-init-%s.log" (system-name)))
  (org-starter-def "~/org/journal"
    :custom-vars org-journal-dir
    :config
    (require 'init-org-journal)))

;;;;; ~/learning
;; This is a secondary repository. It is a Git repository which is
;; pushed to a remote server (currently GitHub).

(org-starter-def "~/learning"
  :add-to-path t
  :config
  (setq
   bookmark-default-file "~/learning/emacs-bookmarks"
   org-download-screenshot-file "~/learning/static/screenshots/scrot.png"))

;; This is a subdirectory of the repository, and it contains Org
;; files for linguistic study (I am neither a linguist or a student,
;; but I have to learn foreign languages.
(org-starter-def "~/learning/natural-languages"
  :id language-learning
  :add-to-path t
  :agenda nil
  :refile (:maxlevel . 1)
  :files
  ("english.org")
  ("chinese.org")
  ("japanese.org"))

;;;;; ~/private
;; This is a repository for files that should be kept private.
(org-starter-def "~/private"
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
  :refile '(:maxlevel . 3))

(org-starter-define-file "subjects.org"
  :key "s"
  :agenda t
  :refile '(:maxlevel . 9))

(org-starter-define-file "brainstorming.org"
  :agenda t
  :refile '(:maxlevel . 9)
  :deprecated t)

(org-starter-def "workflow.org"
  :key "w"
  :agenda nil
  :refile (:maxlevel . 4))

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
              (file+function org-reverse-datetree-1)
              ,(akirak/org-capture-entry-template-1 "%?%(which-function)"
                 "%a\n\n#+BEGIN_SRC %(akirak/buffer-mode-name \"%F\")\n%i\n#+END_SRC\n"
                 :todo "TODO")
              :clock-in t :clock-resume t)
             ("ce" "Babel code, Emacs Lisp" entry
              (file+function org-reverse-datetree-1)
              ,(akirak/babel-capture-template "emacs-lisp")
              :clock-in t :clock-resume t)
             ("ct" "Topic in code.org" entry
              (file+function org-reverse-datetree-1)
              ,(akirak/org-capture-entry-template-1 "%?" "%K"
                                                    :todo "TOPIC"
                                                    :tags '("@topic"))
              :clock-in t :clock-resume t)))

(org-starter-def "posts.org"
  :key "P"
  :required nil
  :agenda t
  :refile (:level . 1))

(org-starter-define-file "immersion.org"
  :agenda t
  :refile '(:maxlevel . 3)
  :capture
  `(("gr" "Item to read" entry (file+olp "Reading")
     ,(akirak/org-capture-entry-template-1
          "^{Title}"
        "%i"
        :todo "TODO"))))

(org-starter-def "yankpad.org"
  :set-default yankpad-file
  :refile (:level . 1))

(org-starter-def "timetables.org")

(org-starter-def "offtime.org"
  :agenda nil
  :custom-vars org-offtime-file)

(org-starter-def "facts.org"
  :refile (:maxlevel . 2))

(org-starter-def "advices.org"
  :refile (:maxlevel . 3))

;; To open this file in org-mode, it should contain the following header:
;; -*- mode: org; mode: beancount -*-
(org-starter-def "bookkeeping.bean"
  :capture
  (("l" "Log/ledger")
   ("le" "Expense" plain (file+olp "Daybook") "%i"))
  :config
  (akirak/define-frame-workflow "ledger"
    :layout
    '(progn
       (find-file (org-starter-locate-file "bookkeeping.bean")))))

;; Set org-default-notes-file
(unless (bound-and-true-p org-default-notes-file)
  (setq org-default-notes-file "~/notes.org")
  (message "Set org-default-notes-file to %s" org-default-notes-file)
  (unless (file-exists-p org-default-notes-file)
    (with-temp-buffer
      (write-file org-default-notes-file))))

(org-starter-def "journal.org"
  :capture
  (("lt" "Time" entry (file+function
                       (lambda () (org-reverse-datetree-1 nil :week-tree t)))
    "* %U %i%? %^g")))

(defun akirak/org-check-in-journal ()
  (interactive)
  (let* ((file (org-starter-locate-file "journal.org" nil t))
         (buf (or (find-buffer-visiting file)
                  (find-file-noselect file))))
    (switch-to-buffer buf)
    (widen)
    (goto-char (point-min))
    (if (org-reverse-datetree-1 nil :week-tree t)
        (progn
          (insert "\nCHECKIN: "
                  (org-timestamp-format
                   (org-timestamp-from-time (current-time) t)
                   (org-time-stamp-format t t))
                  "\n")
          (org-narrow-to-subtree))
      (org-narrow-to-subtree)
      (message "Already checked in")
      (org-end-of-subtree)
      (insert "\n"))))

;;;; org-agenda custom commands (currently unused)

(org-starter-add-agenda-custom-command "b" "Main block agenda"
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
                                        "planner.org"
                                        "timetables.org")))))

;;;; Other org options

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate))

(provide 'my-org)
