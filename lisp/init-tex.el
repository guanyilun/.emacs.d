(use-package auctex
  :config
  ;; this depends on auctex
  ;; set preview scale
  (set-default 'preview-scale-function 1.2))

;; org latex related
(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(require 'ox-latex)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
      org-latex-prefer-user-labels t)

;; toggle fragment when leaving the src block
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; org equation live preview
(use-package org-elp
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/"
  :init
  (require 'org-elp))

(setq org-highlight-latex-and-related '(entities)
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -f %f"
        "bibtex %b"
        "pdflatex -shell-escape -interaction nonstopmode -f %f"
        "pdflatex -shell-escape -interaction nonstopmode -f %f"))

;; Enlarge tex fragments at the same time as text scale
(defun update-org-latex-fragment-scale ()
  (let ((text-scale-factor (expt text-scale-mode-step text-scale-mode-amount)))
    (plist-put org-format-latex-options :scale (* 1.5 text-scale-factor)))
  )
(add-hook 'text-scale-mode-hook 'update-org-latex-fragment-scale)
;; revtex template
(add-to-list 'org-latex-classes
             '("revtex4-1"
               "\\documentclass{revtex4-1}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(provide 'init-tex)
