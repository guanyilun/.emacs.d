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
  :init (add-hook 'after-init-hook 'org-roam-mode)
  :bind (:map org-roam-mode-map
         (("C-c n l" . org-roam)
          ("C-c n f" . org-roam-find-file)
          ("C-c n g" . org-roam-graph)
          ("C-c n o" . my/org-roam-knowledge-search))
         :map org-mode-map
         (("C-c n i" . org-roam-insert))
         (("C-c n I" . org-roam-insert-immediate)))
  :config
  (use-package org-roam-bibtex
    :hook (org-roam-mode . org-roam-bibtex-mode))

  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+setupfile: ../config.setup\n#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n* ${title}\n"
           :unnarrowed t)))

  (defun my/org-roam-knowledge-search ()
    "Perform full-text search on matching files in `org-z-knowledge-dirs'."
    (interactive)
    (let* ((rg-opts (list "-t" "org"))
           (helm-rg-default-extra-args rg-opts)
           (helm-rg-default-directory org-roam-directory))
      (helm-rg nil nil org-roam-directory)))
  )

;; helm-org-rifle will mostly be used with roam so it makes sense that i put it here
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
