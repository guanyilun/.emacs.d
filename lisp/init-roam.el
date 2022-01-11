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
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-ui-mode)
         ("C-c n r" . org-roam-node-random)
         ("C-c n R" . org-roam-refile)
         ("C-c n c" . org-roam-capture)
         ("C-c n o" . my/org-roam-knowledge-search)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-id-get-create)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n t" . org-roam-tag-add))
  :config
  (org-roam-db-autosync-mode)
  (org-add-link-type "ebib" 'ebib)

  (use-package org-roam-bibtex
    :after org-roam)
  
  (setq org-roam-capture-templates
        '(("d" "default" plain ;; (function org-roam--capture-get-point)
           "%?"
           :target (file+head "${slug}.org" "#+setupfile: ../config.setup\n#+title: ${title}\n#+date: %<%Y-%m-%d>\n\nbibliography:../bib/citation.bib\n\n")
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
          deft-use-filename-as-title t)))

(use-package org-roam-ui
  :bind (:map org-mode-map
              ("C-c n z" . org-roam-ui-node-zoom))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t))
;;        org-roam-ui-open-on-start nil))

(use-package helm-org-rifle
  :ensure t
  :bind (("C-c n r" . my/helm-org-rifle-roam-rifle))
  :config
  (defun my/helm-org-rifle-roam-rifle ()
    "Use roam directory rifle"
    (interactive)
    (helm-org-rifle-directories org-roam-directory))

  (defun my/helm-org-rifle--store-link (candidate)
    "Store link into CANDIDATE."
    (-let (((buffer . pos) candidate))
      (with-current-buffer buffer
        (goto-char pos)
        (call-interactively 'org-store-link))))

  (defun my/helm-org-rifle--insert-link (candidate)
    "Insert link to CANDIDATE in current location."
    (interactive)
    (my/helm-org-rifle--store-link candidate)
    (call-interactively 'org-insert-link))

  ;; add new actions to the default rifle action list
  (setq helm-org-rifle-actions
        (append helm-org-rifle-actions
                (helm-make-actions
                 "Store link" 'my/helm-org-rifle--store-link
                 "Insert link" 'my/helm-org-rifle--insert-link))))

(provide 'init-roam)
