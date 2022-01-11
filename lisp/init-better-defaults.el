;; init-helm.el --- -*- lexical-binding: t no-byte-compile: t; -*-
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

;;; Commentary:
;;
;; Better default configurations
;;
;;
;;; Code:

;; Fill column
(setq-default fill-column 70)

;; Indentation no tab
(setq-default indent-tabs-mode nil)

;; show column number
(setq column-number-mode t)

;; Remove menu bar and toolbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Always follow when split window
(global-set-key "\C-x2" (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive) (split-window-horizontally) (other-window 1)))

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))


;; Quickly switch windows
(use-package ace-window
  :preface
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Window Management" 'faicon "windows")
           :foreign-keys warn :quit-key "q")
   ("Actions"
    (("TAB" other-window "switch")
     ("x" ace-delete-window "delete")
     ("m" ace-delete-other-windows "maximize")
     ("s" ace-swap-window "swap")
     ("a" ace-select-window "select")
     ("f" toggle-frame-fullscreen "fullscreen"))
    "Resize"
    (("h" shrink-window-horizontally "←")
     ("j" enlarge-window "↓")
     ("k" shrink-window "↑")
     ("l" enlarge-window-horizontally "→")
     ("n" balance-windows "balance"))
    "Split"
    (("b" split-window-right "horizontally")
     ("B" split-window-horizontally-instead "horizontally instead")
     ("v" split-window-below "vertically")
     ("V" split-window-vertically-instead "vertically instead")
     ("t" toggle-window-split "toggle"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase "in")
     ("-" text-scale-decrease "out")
     ("0" (text-scale-increase 0) "reset"))
    "Appearance"
    (("F" set-frame-font "font")
     ("T" centaur-load-theme "theme"))))
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :bind (;; ([remap other-window] . ace-window)
         ("C-c w" . ace-window-hydra/body))
  :hook (emacs-startup . ace-window-display-mode)
  :config (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t))

;; type with a selected region removes it
(delete-selection-mode t)

;; display line number
;; (global-linum-mode)
;; (setq linum-format "%4d ")
;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))

;; scrollbars
;; highlight current line
(global-hl-line-mode nil)

;; better defaults for buffer management
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save

;; dired settings (always recursive)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(put 'dired-find-alternate-file 'disabled nil)

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))

(bind-key "C-;" 'comment-line)

;; pyim
(use-package pyim
  :init
  (require 'pyim)
  (require 'pyim-basedict)
  (pyim-basedict-enable))

;; auto save
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)   ; quietly save

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(setq visible-bell 1)
(setq-default show-trailing-whitespace t)

(use-package hydra)

;; pdf

(use-package pdf-view
  :ensure pdf-tools
  :diminish (pdf-view-themed-minor-mode
             pdf-view-midnight-minor-mode
             pdf-view-printer-minor-mode)
  :defines pdf-annot-activate-created-annotations
  :functions (my-pdf-view-set-midnight-colors my-pdf-view-set-dark-theme)
  :hook ((pdf-tools-enabled . pdf-view-themed-minor-mode)
         (pdf-tools-enabled . pdf-view-auto-slice-minor-mode)
         (pdf-tools-enabled . pdf-isearch-minor-mode))
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :init
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil
        pdf-annot-activate-created-annotations t)
  :config
  ;; Activate the package
  (pdf-tools-install t nil t nil)

  (with-no-warnings
    ;; Highlight matches
    (defun my-pdf-isearch-hl-matches (current matches &optional occur-hack-p)
      "Highlighting edges CURRENT and MATCHES."
      (cl-destructuring-bind (fg1 bg1 fg2 bg2)
          (pdf-isearch-current-colors)
        (let* ((width (car (pdf-view-image-size)))
               (page (pdf-view-current-page))
               (window (selected-window))
               (buffer (current-buffer))
               (tick (cl-incf pdf-isearch--hl-matches-tick))
               (pdf-info-asynchronous
                (lambda (status data)
                  (when (and (null status)
                             (eq tick pdf-isearch--hl-matches-tick)
                             (buffer-live-p buffer)
                             (window-live-p window)
                             (eq (window-buffer window)
                                 buffer))
                    (with-selected-window window
                      (when (and (derived-mode-p 'pdf-view-mode)
                                 (or isearch-mode
                                     occur-hack-p)
                                 (eq page (pdf-view-current-page)))
                        (pdf-view-display-image
                         (pdf-view-create-image data :width width))))))))
          (pdf-info-renderpage-text-regions
           page width t nil
           `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                          current))
           `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                          (apply 'append
                                 (remove current matches))))))))
    (advice-add #'pdf-isearch-hl-matches :override #'my-pdf-isearch-hl-matches)

    ;; Show annotation
    (defun my-pdf-annot-show-annotation (a &optional highlight-p window)
      "Make annotation A visible."
      (save-selected-window
        (when window (select-window window 'norecord))
        (pdf-util-assert-pdf-window)
        (let ((page (pdf-annot-get a 'page))
              (size (pdf-view-image-size)))
          (unless (= page (pdf-view-current-page))
            (pdf-view-goto-page page))
          (let ((edges (pdf-annot-get-display-edges a)))
            (when highlight-p
              (pdf-view-display-image
               (pdf-view-create-image
                (pdf-cache-renderpage-highlight
                 page (car size)
                 `("white" "steel blue" 0.35 ,@edges))
                :map (pdf-view-apply-hotspot-functions
                      window page size)
                :width (car size))))
            (pdf-util-scroll-to-edges
             (pdf-util-scale-relative-to-pixel (car edges)))))))
    (advice-add #'pdf-annot-show-annotation :override #'my-pdf-annot-show-annotation))

  ;; Recover last viewed position
  (use-package saveplace-pdf-view
    :commands (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
    :init
    (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
    (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice)))

(use-package tramp-mode
  :ensure nil
  :config
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-verbose 1)
  (setq tramp-inline-compress-start-size 1000)
  (setq tramp-copy-size-limit 10000)
  (setq vc-handled-backends '(Git))
  (setq tramp-verbose 1)
  (setq tramp-default-method "scp")
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq projectile--mode-line "Projectile"))

(use-package tramp-term)

(use-package writeroom-mode)
(use-package olivetti)

(provide 'init-better-defaults)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-better-defaults.el ends here
