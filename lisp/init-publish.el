;;; web publishing tools
(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :init
  (add-to-list 'org-src-lang-modes '("html" . web))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-to-list 'org-src-lang-modes '("inline-js" . javascript)) ;; js2 if you're fancy
(defvar org-babel-default-header-args:inline-js
  '((:results . "html")
    (:exports . "results")))

(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))

(provide 'init-publish)
