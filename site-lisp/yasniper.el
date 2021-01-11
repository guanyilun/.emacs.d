;; a better snippet tool
(require 'helm-org-rifle)

(defcustom yasniper-directory "~/.emacs.d/yasniper/"
  "yasnipper directory"
  :type 'string)

(defvar yasniper--last-insert nil)

(defun yasniper ()
  (interactive)
  (helm-org-rifle-directories yasniper-directory))

(defun yasniper-insert-snippet (candidate)
  (-let (((buffer . pos) candidate))
    (with-current-buffer buffer
      (goto-char pos)
      (org-babel-next-src-block)
      (org-babel-mark-block)
      (kill-ring-save (region-beginning) (region-end)))
    (yank)))

;; add new actions to the default rifle action list
(setq helm-org-rifle-actions
      (helm-make-actions
        "insert" 'yasniper-insert-snippet))

;; show full content
;; (setq helm-org-rifle-show-full-contents t)
(setq helm-org-rifle-show-full-contents nil)

(provide 'yasniper)
