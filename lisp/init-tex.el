(use-package auctex
  :init
  (add-hook 'bibtex-mode-hook 'turn-on-auto-revert-mode)
  ;; Activate nice interface between RefTeX and AUCTeX
  (setq reftex-plug-into-AUCTeX t)
  :hook
  ((TeX-mode-hook . turn-on-reftex)
   (TeX-mode-hook . flyspell-mode)
   (TeX-mode-hook . TeX-source-correlate-mode)
   ;; for synctex integration
   (LaTeX-mode-hook . TeX-PDF-mode)
   (LaTeX-mode-hook . TeX-source-correlate-mode)
   )
  :config
  ;; default preview-scale
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t)
  (set-default 'preview-scale-function 1.2))

;; org latex related
(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(require 'ox-latex)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2)
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
    (plist-put org-format-latex-options :scale (* 1.2 text-scale-factor)))
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

;; for pitt etd
;; revtex template
(add-to-list 'org-latex-classes
             '("pittetd"
               "\\documentclass[pdftex]{pittetd}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package ebib
  :init
  (global-set-key (kbd "C-c e") 'ebib)
  :bind (:map ebib-index-mode-map
              ("A" . my/ebib-browse-adsurl))
  :config
  (defun my/ebib-file-name (key)
    (substring (ebib-get-field-value "eprint" key ebib--cur-db) 1 -1))

  (setq ebib-preload-bib-files '("~/Documents/knowledge-base/bib/citation.bib")
        ebib-notes-default-file "~/Documents/knowledge-base/org/papers.org"
        ;; ebib-file-associations '(("pdf" . "okular")) ;;
        ebib-file-associations nil
        ebib-reading-list-file "~/Dropbox/org/reading.org"
        ebib-file-search-dirs '("~/Documents/knowledge-base/papers/")
        ebib-name-transform-function 'my/ebib-file-name
        )

  (defun my/ebib-browse-adsurl (&optional arg)
    "Browse the URL in the \"adsurl\" field.
If the \"adsurl\" field contains more than one URL, ask the user
which one to open.  Alternatively, the user can provide a numeric
prefix argument ARG."
    (interactive "P")
    (ebib--execute-when
      (entries
       (let ((urls (ebib-get-field-value "adsurl" (ebib--get-key-at-point) ebib--cur-db 'noerror 'unbraced 'xref)))
         (if urls
             (ebib--call-browser (ebib--select-url urls (if (numberp arg) arg nil)))
           (error "[Ebib] No URL found in adsurl field"))))
      (default
        (beep)))))

(provide 'init-tex)
