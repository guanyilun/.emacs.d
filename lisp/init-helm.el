;;; init-helm.el --- -*- lexical-binding: t no-byte-compile: t; -*-
;;
;; Copyright (C) 2019 Yilun Guan
;;
;; Author: Yilun Guan <zoom.aaron@gmail.com>
;; URL: https://github.com/guanyilun/.emacs.d-light/
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;
;;; Commentary:
;;
;; Initialize helm setting
;;
;;
;;; Code:


(use-package helm
  :ensure t
  :init
  (global-unset-key (kbd "C-x c"))
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-o" . helm-occur)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :config
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line        t
        helm-M-x-fuzzy-match                  t
        helm-autoresize-max-height            0
        helm-autoresize-min-height            30)
  (defun spacemacs//helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))
  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package helm-rg
  :after helm)

(use-package helm-recoll
  :commands helm-recoll
  :bind
  (("C-c n O" . my/helm-recoll))
  :init
  (helm-recoll-create-source "main" "~/.recoll")  ;; only need to run once
  (setq helm-debug t)
  :config
  ;; (helm :sources 'helm-source-recoll-main)
  (defun my/helm-recoll ()
    (interactive)
    (helm :sources 'helm-source-recoll-main)))

(use-package helm-google
  :bind (("C-h g" . helm-google))
  :config
  ;;(setq browse-url-browser-function 'eww-browse-url)
  )

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
