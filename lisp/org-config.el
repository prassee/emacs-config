(add-hook 'org-mode-hook 'visual-line-mode)

(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0
                            (prog1
                                ()
                              (compose-region
                               (match-beginning 1)
                               (match-end 1)
                               "•"))))))

(setq org-capture-templates
      '(("o" "Capture a Office Tasks" entry
         (file+headline "/data/org/office.org" "Office")
         "* TODO %^{what's task in Office ?} DEADLINE: %^T  %^G \n %?\n %T\n  %i \n")

        ("v" "What todo on Vamana?" entry
         (file+headline "/data/org/vamana.org" "Vamana")
         "* TODO %^{what's task in Vamana ?} DEADLINE: %^T  %^G \n %?\n %T\n  %i \n")

        ("l" "What are you learn/explor'ing" entry
         (file+headline "/data/org/lrde.org" "LRDE")
         "* TODO %^{what's to Learn / Research / Explore ?}  %^G \n %?\n %T\n %i \n")


        ("n" "Quick Notes" entry
         (file "/data/org/notes.org")
         "* TODO %^{Capture a note about ? }  %^G \n %?" :empty-lines 2)

         ("d" "Quick Document" entry
         (file "/data/org/documentation.org")
         "* TODO %^{Quickly 'Document' steps about ? } %?" :empty-lines 2)
        
        ("g" "Capture a ad-hoc thought" entry
         (file+headline "/data/org/general.org" "Notes")
         "* TODO %^{what going on ?} %^G  \n %?\n %T\n  %i\n"))
      org-cycle-separator-lines 1
      org-log-done 'time
      org-todo-keywords'((sequence
                          "TODO(t)"
                          "DOING(d)" "|"
                          "DONE(o)")
                         (sequence
                          "FEATURE/IDEA(f)"
                          "DEFECT(d)"
                          "FOLLOWUP(f)"
                          "DELAYED(l)"
                          "CANCELLED(c)" "|"
                          "IMPLEMENTED(m)"))
      org-agenda-files
      (list "/data/org/general.org" "/data/org/lrde.org" "/data/org/office.org" "/data/org/vamana.org" "/data/org/notes.org" "/data/org/documentation.org" "/data/org/notes.org" )
      ;;     org-priority-faces ';; ((65 :foreground "red" :background "yellow" :weight 'bold )
      ;;  (66 :foreground "black" :background "orange" :weight 'bold)
      ;;  (67 :foreground "black" :background "green" :weight 'bold)
      ;;  (68 :foreground "black" :background "green")
      ;;  )
      org-hide-emphasis-markers t

      org-refile-targets
      '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm

      org-auto-align-tags nil
      org-startup-folded t
      org-checkbox-hierarchical-statistics nil

      org-outline-path-complete-in-steps nil
      org-display-inline-images t
      org-startup-with-inline-images "inlineimages"
      org-redisplay-inline-images t
      org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/plantuml.jar")
      org-pretty-entities t

      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window

      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t

      org-highlight-latex-and-related
      '(latex)

      org-enforce-todo-dependencies t

      org-agenda-window-setup 'current-window
      org-deadline-warning-days 7
      ;;show me tasks scheduled or due in next fortnight
      org-agenda-span ' fortnight
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-use-time-grid nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-sorting-strategy
      (quote
       ((agenda priority-down alpha-up)
        (todo priority-down alpha-up)
        (tags priority-down alpha-up)))
      ;; (quote
      ;;  ((agenda deadline-up priority-down)
      ;;   (todo priority-down category-keep)
      ;;   (tags priority-down category-keep)
      ;;   (search category-keep)))

      org-agenda-prefix-format
      (quote
       ((agenda . "%s %?-12t %e ")
        (timeline . "  %s")
        (todo . " %i %e ")
        (tags . " %i %e ")
        (search . " %i %e "))))

(setq org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t
          ;; org-image-actual-width '(300)
          )

(provide 'org-config)
