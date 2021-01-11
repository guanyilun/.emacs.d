;; lisp editing configurations
;; by Yilun Guan

(use-package paredit
  :ensure t
  :commands (enable-paredit-mode)
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (hy-mode . enable-paredit-mode))
  :bind (:map paredit-mode
         ("C-t" . transpose-sexps)
         ("C-c t" . reverse-transpose-sexps)
         ("C-o" . avy-goto-sexp-begin)
         ("C-c o" . avy-goto-sexp-end)
         ("M-c" . paredit-convolute-sexp)
         ("C-c ." . my-paredit-convolute-sexp))
  :config
  (defun reverse-transpose-sexps (arg)
    (interactive "*p")
    (transpose-sexps (- arg))
    (backward-sexp  arg)
    (forward-sexp 1))

  (defun avy-goto-sexp-begin ()
    (interactive "*")
    (let ((avy-all-windows nil))
      (avy-with avy-goto-char
        (avy--process
         (avy--regex-candidates
          (regexp-quote "("))
         (avy--style-fn avy-style)))))

  (defun avy-goto-sexp-end ()
    (interactive "*")
    (let ((avy-all-windows nil))
      (avy-with avy-goto-char
        (avy--process
         (avy--regex-candidates
          (regexp-quote ")"))
         (avy--style-fn avy-style)))
      (forward-char)))

  (defun my-paredit-convolute-sexp ()
    (interactive "*")
    (paredit-convolute-sexp)
    (paredit-forward)
    (backward-char 2)
    (delete-char 1)
    (backward-char -1)
    (insert " "))
  )

(provide 'init-lisp)
;; init-lisp ends here
