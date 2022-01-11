;; initialize org related settings

(use-package org
  :ensure nil
  ;; :custom-face (org-ellipsis ((t (:foreground nil))))  
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1)))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org" :face 'all-the-icons-green :height 1.1 :v-adjust 0.0)
           :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("e" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("S" (hot-expand "<s" "sh") "sh"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))  
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
         ;; (org-mode . (lambda ()
         ;;               "Beautify org symbols."
         ;;               (push '("[ ]" . "☐" ) prettify-symbols-alist)
         ;;               (push '("[X]" . "☑" ) prettify-symbols-alist)
         ;;               (push '("[-]" . "❍" ) prettify-symbols-alist)                       
         ;;               (prettify-symbols-mode 1)))
         )
  :config
  (setq 
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
   org-attach-auto-tag nil
   ;; org-ellipsis (if (and (display-graphic-p) (char-displayable-p ?⏷)) "⏷" nil)
   ;; org-link-file-path-type "adaptive"
   )
  ;; For hydra
  (defun hot-expand (str &optional mod)
    "Expand org template.
STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))
  ;; Prettify UI
  (use-package org-bullets
    :if (char-displayable-p ?⚫)
    :hook (org-mode . org-bullets-mode)
    :init (setq org-bullets-bullet-list '("⚫" "⚫" "⚫" "⚫")))
  
  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :init (setq org-fancy-priorities-list
                (if (and (display-graphic-p) (char-displayable-p ?⯀))
                    '("⯀" "⯀" "⯀" "⯀")
                  '("HIGH" "MEDIUM" "LOW" "OPTIONAL"))))
  
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
    :init
    (require 'org-ref)
    (require 'org-ref-arxiv)
    (require 'org-ref-bibtex))
  
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
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")

  (use-package ox-reveal
    :config
    (require 'ox-reveal))
  
  (use-package org-transclusion
    :after org
    :bind
    (;; key choice to be consistent with roam
     ("C-c n t" . org-transclusion-mode)
     ("C-c n a" . org-transclusion-add))))

(provide 'init-org)

;;; end of init-org
