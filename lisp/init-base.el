(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Environment
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Start server
(use-package server
  :ensure nil
  :hook (after-init . my-server-start)
  :config
  (defun my-server-start ()
    "Only start server when server is not running"
    (unless (or (server-running-p) (daemonp))
      (server-start))))

(use-package pretty-hydra
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))
  (require 'pretty-hydra))

(use-package elfeed
  :pretty-hydra
  ((:title (pretty-hydra-title "Elfeed" 'faicon "rss-square")
           :color amaranth :quit-key "q")
   ("Search"
    (("g" elfeed-search-update--force "refresh")
     ("G" elfeed-search-fetch "update")
     ("y" elfeed-search-yank "copy URL")
     ("+" elfeed-search-tag-all "tag all")
     ("-" elfeed-search-untag-all "untag all"))
    "Filter"
    (("s" elfeed-search-live-filter "live filter")
     ("S" elfeed-search-set-filter "set filter")
     ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
     ("A" (elfeed-search-set-filter "@6-months-ago" "all"))
     ("T" (elfeed-search-set-filter "@1-day-ago" "today")))
    "Article"
    (("b" elfeed-search-browse-url "browse")
     ("n" next-line "next")
     ("p" previous-line "previous")
     ("u" elfeed-search-tag-all-unread "mark unread")
     ("r" elfeed-search-untag-all-unread "mark read")
     ("RET" elfeed-search-show-entry "show"))))
  :bind (("C-x w" . elfeed)
         :map elfeed-search-mode-map
         ("?" . elfeed-hydra/body)
         :map elfeed-show-mode-map
         ("o" . ace-link)
         ("q" . delete-window))
  :config
  (setq elfeed-db-directory (locate-user-emacs-file ".elfeed")
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'delete-window)
  ;; a trick to adjust elfeed fill column
  (setq shr-width 80))

(provide 'init-base)
