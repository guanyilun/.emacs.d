(use-package auctex
  :config
  ;; this depends on auctex
  (use-package cdlatex
    :hook (org-mode . turn-on-org-cdlatex))
  ;; set preview scale
  (set-default 'preview-scale-function 1.2))

(provide 'init-tex)
