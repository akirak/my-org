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

;;;;; ~/private
;; This is a repository for files that should be kept private.
(org-starter-def "~/private"
  :add-to-path t)

;;;; Org files

;;;;; Inbox files

(org-starter-def "scratch.org"
  :key "i"
  :required nil
  :custom-vars org-default-notes-file
  :agenda t
  :refile (:maxlevel . 2))

;; Inbox file on the phone
;; To avoid synchronization conflicts, I add new entries to this file
;; and usually not to other files when I am on my phone.
;; When I am on my computer, I refile entries in this file to other
;; permanent files.
(org-starter-def "phone.org"
  :agenda t)

;; Inbox file on the tablet
;; The same as above
(org-starter-def "tablet.org"
  :agenda t)

;;;;; Tasks and notes

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
  :deprecated t
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
  :agenda t
  :refile (:maxlevel . 2))

(org-starter-def "advices.org"
  :agenda t
  :refile (:maxlevel . 3))

;; Configure files for linguistic study.
;; All of these files have basically the same structure, but they can
;; also have some additional language-specific structures.
(dolist (filename '("english.org"
                    "chinese.org"
                    "japanese.org"
                    "korean.org"))
  (org-starter-define-file filename
    :agenda t
    :refile '(:maxlevel . 1)))

(org-starter-define-file "groceries.org"
  :capture
  `(("gg" "Add an item to grocery list" entry
     ,(akirak/org-capture-entry-template-1 "%i%?"
        "" :todo "TODO"))))

(org-starter-def "trash.org"
  :refile (:level . 0))

;; To open this file in org-mode, it should contain the following header:
;; -*- mode: org; mode: beancount -*-
(org-starter-def "bookkeeping.bean"
  :capture
  (("l" "Log to the cashbook" plain (file+olp "Daybook") "%i"))
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
  :agenda t
  :capture
  (("j" "Log event" entry (file+function
                      (lambda () (org-reverse-datetree-1 nil :week-tree t)))
    "* %^U %i%? %^g")))

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

;;;; org-agenda custom commands

(org-starter-add-agenda-custom-command "b" "Main block agenda"
  '((agenda ""
            ((org-agenda-span 1)
             (org-agenda-prefix-format "  %4e ")
             (org-super-agenda-groups
              '((:tag "@errand")
                (:tag "@focus")
                (:tag "@buffer")
                (:habit t)
                (:auto-category t)))))
    (tags "CATEGORY=\{scratch\\|phone\\|tablet\}"
          ((org-agenda-overriding-header "Top-level headings in scratch")
           (org-tags-match-list-sublevels nil)
           (org-agenda-sorting-strategy '(priority-down))
           (org-super-agenda-groups
            '((:todo ("DONE" "ARCHIVED"))
              (:todo "REVIEW")
              (:todo "STARTED")
              (:todo "NEXT")))))
    (tags-todo "CATEGORY=\"journal\""
               ((org-tags-match-list-sublevels nil)))))

(org-starter-add-agenda-custom-command "f" "Focus"
  ;; @focus tag is usually set as a file tag
  '((tags-todo "@focus"
               ((org-agenda-overriding-header "Tasks in specific categories")
                (org-agenda-sorting-strategy '(scheduled-down priority-up))
                (org-agenda-prefix-format "  %5e %s %-7c %b")
                (org-super-agenda-groups
                 '((:todo "REVIEW")
                   (:tag "@fix")
                   (:todo "STARTED")
                   (:todo "NEXT")
                   (:auto-category t)))))))

(akirak/define-frame-workflow "focus"
  :key "f"
  :layout '(progn
             (org-agenda nil "f")
             (delete-other-windows)))

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
