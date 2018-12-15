;;;; Org options

;;;;; Org tags 

(setq-default org-group-tags t
              ;; org-use-fast-tag-selection t
              ;; org-fast-tag-selection-single-key nil
              org-agenda-use-tag-inheritance t
              org-tags-exclude-from-inheritance
              '(
                ;; "objective"
                ;; "sprint"
                ;; "agenda-group"
                ))

(require 'my-org-todo)

;;;;; Other org options

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate))

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
;; This is my secondary repository for miscellaneous records.
;; It is a Git repository which is pushed to a remote server
;; (currently GitHub).

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

(defun akirak/org-refile-to-code (arg)
  "Refile the current entry into the date tree in code.org.

When the prefix ARG is set, jump to the date tree."
  (interactive "P")
  (when-let
      ((date (org-read-date nil nil nil "Refile to code.org on date: "))
       (file (org-starter-locate-file "code.org" nil t))
       (rfloc (with-current-buffer (or (find-buffer-visiting file)
                                       (find-file-no-select file))
                (org-reverse-datetree-1 (org-time-string-to-time date)
                                        :return 'rfloc))))
    (org-refile nil nil rfloc)
    (when arg
      (org-refile '(16)))))

(org-starter-def "posts.org"
  :key "P"
  :required nil
  :agenda t
  :refile (:maxlevel . 3))

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

(org-starter-def "random.org"
  ;; TODO: Add capture template
  )

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
  (message "Set org-default-notes-file to %s" org-default-notes-file))

(org-starter-def "journal.org"
  :agenda t
  :capture
  (("j" "Log event" entry
    (file+function (lambda () (org-reverse-datetree-1 nil :week-tree t)))
    "* %^U %i%? %^g"
    :clock-in t :clock-resume t)))

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

(defun akirak/org-schedule-event-in-journal (arg)
  "Refile an entry under the cursor into the journal file.

This command refiles the entry on a particular date in the date tree
in the journal file. With prefix ARG, move the point to the
destination."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (when-let
      ((date (org-read-date nil nil nil "Scheduled date: "))
       (file (org-starter-locate-file "journal.org" nil t))
       (rfloc (with-current-buffer (or (find-buffer-visiting file)
                                       (find-file-no-select file))
                (org-reverse-datetree-1 (org-time-string-to-time date)
                                        :week-tree t)
                (list nil file nil (point)))))
    (org-schedule nil date)
    (org-refile nil nil rfloc)
    (when arg
      (org-refile '(16)))))

;;;; org-agenda custom commands

(org-starter-add-agenda-custom-command "b" "Main block agenda"
  '((agenda ""
            ((org-agenda-span 1)
             (org-agenda-prefix-format "  %4e %t %-7:c %s ")
             (org-super-agenda-groups
              '((:tag "@focus")
                (:tag "@errand")
                (:tag "housekeeping")
                (:tag "@buffer")
                (:habit t)
                (:auto-category t)))))
    (tags-todo "CATEGORY=\"journal\"&SCHEDULED<\"<+0d>\""
               ((org-agenda-overriding-header "Finishing journal entries")
                (org-tags-match-list-sublevels nil)
                (org-agenda-todo-ignore-scheduled 'future)))
    (tags "CATEGORY=\{scratch\\|phone\\|tablet\}"
          ((org-agenda-overriding-header "Top-level headings in scratch")
           (org-tags-match-list-sublevels nil)
           (org-agenda-sorting-strategy '(priority-down))
           (org-super-agenda-groups
            '((:todo ("DONE" "ARCHIVED"))
              (:tag "@urgent")
              (:todo "REVIEW")
              (:todo "STARTED")
              (:todo "NEXT")
              (:tag "troubleshooting")
              (:tag "@explore")
              (:tag "@coding")))))
    (tags-todo "CATEGORY=\"tasks\""
               ((org-agenda-overriding-header "Unscheduled tasks in tasks category")
                (org-agenda-prefix-format "  %b ")
                (org-agenda-sorting-strategy '(priority-down))
                (org-super-agenda-groups
                 '((:discard (:scheduled t))
                   (:todo "STARTED")
                   (:todo "NEXT")
                   (:priority "A")
                   ;; Group by tags as you like
                   (:tag "maintenance")))))))

(org-starter-add-agenda-custom-command "r" "Review"
  '((tags-todo "CATEGORY=\"facts\""
               ((org-agenda-overriding-header "Reflection")
                (org-agenda-prefix-format "  ")
                (org-agenda-sorting-strategy '(priority-down))
                (org-super-agenda-groups
                 '((:auto-group t)))))
    (tags-todo "CATEGORY=\"advices\"/STARTED"
               ((org-agenda-overriding-header "Working on")
                (org-agenda-prefix-format
                 "  %(org-entry-get nil \"DEADLINE\") %(or (org-entry-get nil \"PROGRESS\") \"0%\") %b")
                (org-tags-match-list-sublevels t)))
    (tags-todo "CATEGORY=\"advices\"/NEXT"
               ((org-agenda-overriding-header "Up next")
                (org-tags-match-list-sublevels t)))
    ;; (tags-todo "CATEGORY=\"advices\""
    ;;            ((org-agenda-overriding-header "Status")
    ;;             (org-agenda-prefix-format "  %b ")
    ;;             (org-agenda-dim-blocked-tasks 'invisible)
    ;;             (org-super-agenda-groups
    ;;              '((:todo "REVIEW")
    ;;                (:todo "STARTED")
    ;;                (:todo "NEXT")
    ;;                (:discard (:anything t))))))
    (tags-todo "CATEGORY=\"advices\"-@invalid"
               ((org-agenda-overriding-header "Overview")
                (org-agenda-sorting-strategy '(user-defined-up))
                (org-agenda-prefix-format "  %s ")
                (org-tags-match-list-sublevels nil)
                (org-agenda-cmp-user-defined 'akirak/org-agenda-cmp-sanity-level)))
    (tags-todo "CATEGORY=\"advices\"+@invalid"
               ((org-agenda-overriding-header "Invalid")
                (org-tags-match-list-sublevels nil)
                (org-agenda-prefix-format "  %b ")))))

(defun akirak/org-agenda-cmp-sanity-level (a b)
  (let* ((ma (or (get-text-property 1 'org-marker a)
                 (get-text-property 1 'org-hd-marker a)))
         (mb (or (get-text-property 1 'org-marker b)
                 (get-text-property 1 'org-hd-marker b)))
         (fa (and ma (marker-buffer ma)))
         (fb (and mb (marker-buffer mb)))
         (la (and fa (with-current-buffer fa (org-entry-get ma "SANITY_LEVEL"))))
         (lb (and fb (with-current-buffer fb (org-entry-get mb "SANITY_LEVEL")))))
    (or (and la lb (< (math-read-number la) (math-read-number lb)))
        0)))

;;;; Startup

(defconst akirak/org-start-page-buffer "*start*")

(defun akirak/org-start-page-buffer ()
  "Open my \"start page\" or return its buffer."
  (when-let ((buf (get-buffer akirak/org-start-page-buffer)))
    (kill-buffer buf))
  (when-let ((file (org-starter-locate-file "advices.org" nil t)))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (org-with-wide-buffer
       (if-let ((pos (org-find-property "CUSTOM_ID" "entry-points")))
           (let ((org-indirect-buffer-display 'current-window))
             (goto-char pos)
             (org-tree-to-indirect-buffer)
             (rename-buffer akirak/org-start-page-buffer)
             (setq buffer-read-only t)
             (current-buffer))
         (user-error "Cannot find an entry with CUSTOM_ID property set to emacs-start"))))))

(defun akirak/org-start-page ()
  "Load my startup page in Org."
  (interactive)
  (let ((buf (akirak/org-start-page-buffer)))
    ;; In interactive calls, display the buffer in the current window.
    (pop-to-buffer-same-window buf)))

;; Override the existing function to set up startup windows.
(defun akirak/setup-startup-windows ()
  (switch-to-buffer "*Messages*")
  (if (< (frame-width) 160)
      (split-window-below)
    (split-window-right))
  (switch-to-buffer (akirak/org-start-page-buffer)))

(add-hook 'emacs-startup-hook 'akirak/setup-startup-windows)

(defun akirak/refresh-session ()
  (interactive)
  (save-some-buffers t)
  (desktop-clear)
  (akirak/org-start-page))

(provide 'my-org)
