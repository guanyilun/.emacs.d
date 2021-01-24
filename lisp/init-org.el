;; initialize org related settings

(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture))
  :init (setq 
         ;; org babel
         org-confirm-babel-evaluate nil
         ;; attachment
         org-attach-dir-relative   t
         org-src-fontify-natively  t
         org-src-tab-acts-natively t
         ;; inline image size default
         ;; for image specific setting use
         ;; #+ATTR_ORG: :width 400
         org-image-actual-width '(400)
         org-attach-auto-tag nil)
  :config
  ;; Prettify UI
  (use-package org-bullets
    :if (char-displayable-p ?⚫)
    :hook (org-mode . org-bullets-mode)
    :init (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫")))

  ;; org babel language list
  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (js . t)
                               (css . t)
                               (C . t)
                               (shell . t)))
  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; org-ref to manage references
  ;; see https://github.com/jkitchin/org-ref/blob/master/org-ref.org
  ;; for more information
  (use-package org-ref
    :init (require 'org-ref))
  
  ;; org download
  (use-package org-download
    :bind (("C-S-y" . org-download-clipboard))
    :config
    (setq org-download-method 'directory
          org-download-image-org-width 400)
    (defun org-download--dir-2 ()
      "Return the current filename instead of heading name"
      (file-name-base (buffer-file-name)))
    (add-hook 'dired-mode-hook 'org-download-enable))

  ;; always use evince to open pdf
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s"))

(provide 'init-org)

;;; end of init-org
