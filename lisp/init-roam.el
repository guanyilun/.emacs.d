;; org-roam related functionaities -- de-cluttered from the init-org
;; This needs to be loaded after init-org, init-helm
;; everything under this will be bundled under

;; dependencies
;; (require 'dash)
;; (require 'helm)
;; (require 'helm-org-rifle)
;; (require 'helm-rg)

;; the variable org-roam-directory needs to be customized
(use-package org-roam
  :ensure t
  :bind (
         (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n r" . org-roam-random-note)
          ("C-c n c" . org-roam-capture)
          ("C-c n o" . my/org-roam-knowledge-search))
         :map org-mode-map
         (("C-c n i" . org-roam-node-insert)))
  :config
  (org-roam-db-autosync-mode)
  (org-add-link-type "ebib" 'ebib)

  ;; (use-package org-roam-bibtex
  ;;   :ensure t)
  ;; ;; (use-package org-roam-bibtex
  ;; ;;   :ensure t
  ;; ;;   :hook (org-roam-mode . org-roam-bibtex-mode))
  (use-package org-roam-bibtex
    :after org-roam)
  
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+setupfile: ../config.setup\n#+title: ${title}\n#+date: %<%Y-%m-%d>\n\nbibliography:../bib/citation.bib\n\n"
           :unnarrowed t)))

  (defun my/org-roam-knowledge-search ()
    "Perform full-text search on matching files in `org-z-knowledge-dirs'."
    (interactive)
    (let* ((rg-opts (list "-t" "org"))
           (helm-rg-default-extra-args rg-opts)
           (helm-rg-default-directory org-roam-directory))
      (helm-rg nil nil org-roam-directory)))

  ;; deft for org-roam
  (use-package deft
    :bind (("C-c n d" . deft))
    :commands (deft)
    :config
    (setq deft-extensions '("org" "bib" "txt")
          deft-directory org-roam-directory
          deft-recursive t
          deft-use-filename-as-title t))

  (use-package helm-rg)
  )

(use-package org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'init-roam)
