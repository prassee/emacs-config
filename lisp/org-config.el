(add-hook 'org-mode-hook 'visual-line-mode)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


(setq org-capture-templates '(
                              ("o" "Capture a Office Tasks" entry (file+headline "/data/org/office.org" "Office")
                               "* TODO %^{what's task in Office ?} %^G  \n %?\n %T\n  %i\n")
                              
                              ("v" "Capture a Vamana Todo" entry (file+headline "/data/org/vamana.org" "Vamana")
                               "* TODO %^{what's task in Vamana ?} %^G  \n %?\n %T\n  %i\n")

                              ("l" "Capture what to learn, explore" entry (file+headline "/data/org/lrde.org" "LRDE")
                               "* TODO %^{what's to Learn / Research / Explore ?} %^G  \n %?\n %T\n  %i\n")

                              ("n" "Notes" entry (file "/data/org/notes.org")
                               "* %^{Capture a note about ? } %?" :empty-lines 2)

                              ("b" "BUJO" entry (file "/data/org/bujo.org")
                               "* %^{Capture a BUJO } %?" :empty-lines 2)
                              
                              ("g" "Capture anything in General" entry (file+headline "/data/org/general.org" "Notes")
                               "* TODO %^{Whatsup prassee...} %^G  \n %?\n %T\n  %i\n")
                              )
      org-cycle-separator-lines 1
      org-log-done 'time org-todo-keywords'(
                                            (sequence "TODO(t)" "|" "DONE(d)")
                                            (sequence  "FEATURE/IDEA(f)" "BUG(b)" "KNOWNCAUSE(k)" "|" "IMPLEMENTED(m)"))
      org-agenda-files (list "/data/org/general.org" "/data/org/lrde.org" "/data/org/office.org" "/data/org/vamana.org" "/data/org/notes.org" "/data/org/bujo.org")
;;     org-priority-faces ';; ((65 :foreground "red" :background "yellow" :weight 'bold )
                          ;;  (66 :foreground "black" :background "orange" :weight 'bold)
                          ;;  (67 :foreground "black" :background "green" :weight 'bold)
                          ;;  (68 :foreground "black" :background "green")
                          ;;  )
      org-hide-emphasis-markers t

      
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm

      org-auto-align-tags nil
      org-startup-folded t
      org-checkbox-hierarchical-statistics nil
      
      org-outline-path-complete-in-steps nil
      org-display-inline-images t
      org-startup-with-inline-images "inlineimages"
      org-redisplay-inline-images t
      org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar")
      org-pretty-entities t
      
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window

      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t

      org-highlight-latex-and-related '(latex)

      org-enforce-todo-dependencies t

      org-agenda-window-setup 'current-window
      org-agenda-use-time-grid nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-sorting-strategy
      (quote
       ((agenda priority-down alpha-up)
        (todo priority-down alpha-up)
        (tags priority-down alpha-up)))

      org-agenda-prefix-format (quote
       ((agenda . "%s %?-12t %e ")
        (timeline . "  %s")
        (todo . " %i %e ")
        (tags . " %i %e ")
        (search . " %i %e ")))
      )

(provide 'org-config)
