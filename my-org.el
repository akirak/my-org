;;;; Configure org-starter package

(use-package org-starter
  :functions (org-starter-define-directory org-starter-define-file)
  :config
  (defun helm-org-rifle-known-files ()
    (interactive)
    (helm-org-rifle-files org-starter-known-files))
  ;; Based on http://www.howardism.org/Technical/Emacs/capturing-content.html
  (general-setq
   org-starter-initial-capture-templates
   `(("a" "Append plain text to the clocked task" plain
      (clock)
      "%i"
      :empty-lines 1 :immediate-finish t)
     ("i" "Add an item to the clocked task" item
      (clock)
      "%i%?" :empty-lines 1)
     ("t" "Sub-task of the clocked task" entry
      (clock)
      ,(akirak/org-capture-entry-template-1 "%i%?" ""
                                            :todo "TODO")
      :clock-in t :clock-resume t)
     ("p" "Protocol quote" entry (clock)
      ,akirak/org-protocol-note-template)
     ("L" "Protocol link (as item)" item (clock)
      "[[%:link][%:description]] %?")
     ("d" "To the default notes file")
     ("dt" "Task in the default notes file" entry
      (file "")
      ,(akirak/org-capture-entry-template-1 "%i%?" ""
                                            :todo "TODO")
      :clock-in t)
     ("dp" "Protocol quote" entry (file "") ,akirak/org-protocol-note-template
      :clock-in t)
     ("dL" "Protocol link (as entry)" entry (file "") ,akirak/org-protocol-link-template
      :clock-in t)
     ("u" "Urgent task" entry
      (file "")
      "* NEXT %?\nDEADLINE: %t\n%i"
      :clock-in t :clock-resume t)))
  (setq org-capture-templates-contexts
        `(
          ;; Capture into org-default-notes-file when not clocking in
          ,@(cl-loop for key in '("t" "p" "L")
                     collect `(,key ,(concat "d" key)
                                    ((lambda () (not (org-clocking-p))))))
          ;; Disable templates with the clock target when not clocking in
          ("@" (org-clocking-p))
          ,@(cl-loop for (key _ _ target) in org-starter-initial-capture-templates
                     when (equal target '(clock))
                     collect `(,key (org-clocking-p)))))
  :custom
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-alternative-find-function #'helm-org-rifle-files)
  (org-starter-extra-find-file-map
   '(("j" (lambda () (interactive) (counsel-ag nil "~/personal/org-journal")) "search journal")))
  (org-starter-extra-refile-map
   '(("j" org-refile-to-journal "journal")
     ("'" avy-org-refile-as-child "avy")
     ("?" org-refile-same-buffer "in-buffer")
     ("o" org-refile-other-window-files "other window buffers")
     ("@" (lambda () (interactive) (org-refile 2)) "clock")))
  (org-starter-extra-alternative-find-file-map
   '(("SPC" helm-org-rifle-known-files "all"))))

;;;; Org directories

(org-starter-def "~/org/"
  :add-to-path t
  :custom-vars (org-directory)
  (org-starter-def "~/org/journal"
    :custom-vars org-journal-dir
    :config
    (require 'init-org-journal)))

(org-starter-def "~/learning"
  :add-to-path t)

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

(org-starter-define-file "scratch.org"
  :key "i"
  :required nil
  :custom-vars 'org-default-notes-file
  :agenda t
  :refile '(:maxlevel . 2))

(org-starter-define-file "tasks.org"
  :required nil
  :agenda t
  :refile '(:maxlevel . 9))

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

(org-starter-define-file "workflow.org"
  :key "w"
  :agenda nil
  :refile '(:maxlevel . 4))

(org-starter-define-file "icebox.org"
  :key "m"
  :refile '(:maxlevel . 9))

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
                                                    :tags '("topic")))))

(org-starter-define-file "posts.org"
  :key "P"
  :required nil
  :agenda t
  :refile '(:level . 1))

(org-starter-define-file "accounting.org"
  :key "M"
  :agenda t
  :refile '(:maxlevel . 3))

(org-starter-define-file "yankpad.org"
  :set-default 'yankpad-file
  :refile '(:level . 1))

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
    (tags "CATEGORY=\"scratch\""
          ((org-agenda-overriding-header "Top-level headings in scratch")
           (org-tags-match-list-sublevels nil)))))

;;;; Custom rifle commands

(defun akirak/helm-org-rifle-knowledge-base ()
  (interactive)
  (helm-org-rifle-files (delq nil
                              (mapcar (lambda (fname)
                                        (org-starter-locate-file fname nil t))
                                      '("scratch.org"
                                        "workflow.org"
                                        "emacs.org"
                                        "code.org"
                                        "posts.org"
                                        "brainstorming.org"
                                        "subjects.org"
                                        "planner.org")))))

;;;; Other org options

(with-eval-after-load 'org-clock
  (org-clock-persistence-insinuate))

(provide 'my-org)
