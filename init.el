(load "~/.emacs.d/sanemacs.el")

;;;;;;;;;;;;;;;;;;;;;;
;; General Config   ;;
;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Path
(setenv "PATH" (concat
                "/usr/bin:"
                "/usr/local/bin:"
                "/home/doug/.local/bin:"
                "/home/doug/.npm/bin:"
                (getenv "PATH")))

;;; Indent a selection of text using Control+> and Control+<
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)

;;; Open corresponding header or source file
(defun toggle-header-source ()
  (interactive)
  (ff-find-other-file nil t))
(global-set-key (kbd "<f4>") 'toggle-header-source)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacsclient / Emacs server
(add-hook 'server-switch-hook #'raise-frame)

;; Window
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-j") 'windmove-down)

;; Remove annoying minimize shortcu
(global-set-key (kbd "C-z") nil)

;; Highlight spaces and tabs differently
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere

;;;;;;;;;;;;;;;;;;;;;;
;; Package Config   ;;
;;;;;;;;;;;;;;;;;;;;;;

;;;
;; Themes
;;;

(use-package atom-one-dark-theme)
(use-package dracula-theme)
(use-package monokai-theme)
(use-package doom-themes)

;;;
;; Syntax Modes
;;;

(use-package pug-mode)
(use-package yaml-mode)
(use-package sass-mode)
(use-package vue-mode :init (setq-default mmm-submode-decoration-level 0))
(use-package cmake-mode)
(use-package all-the-icons)

;;;
;; Basic Enhancements
;;;

(use-package diff-hl
  :init
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package autopair :init (autopair-global-mode))

(use-package undo-tree :init (global-undo-tree-mode))

(use-package emmet-mode
  :commands emmet-mode
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package yafolding
  :commands yafolding-mode
  :config
  (define-key yafolding-mode-map (kbd "M-<return>") 'yafolding-toggle-element))

(use-package highlight-indent-guides
  :commands highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'character)
  (defun dont-highlight-first-level (level responsive display)
    (if (> 1 level) ; replace `1' with the number of guides you want to hide
        nil
      (highlight-indent-guides--highlighter-default level responsive display)))

  (setq highlight-indent-guides-highlighter-function 'dont-highlight-first-level))

(use-package doom-modeline
  :init
  (doom-modeline-init))

;;;
;; Language-related tools
;;;

(use-package slime
  :commands slime
  :config
  (setq slime-contribs '(slime-fancy))
  (add-hook 'slime-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c l") 'slime-repl-clear-buffer))))

;;;
;; Packages that turn Emacs into a powerhouse
;;;

(use-package helm
  :bind ("M-x" . helm-M-x)
  :bind ("C-x r b" . helm-filtered-bookmarks)
  :bind ("C-x C-f" . helm-find-files)
  :init
  (require 'helm-config)
  (helm-mode 1))

(use-package projectile
  :bind ("M-m" . projectile-command-map)
  :config
  (use-package helm-projectile
    :init
    (helm-projectile-on))
  :init
  (projectile-mode +1)
  (require 'helm-projectile)
  (helm-projectile-on))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package yasnippet)

(use-package company
  :commands company-complete
  :bind ("M-/" . company-complete)
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :commands flycheck-mode
  :init
  (add-hook 'c-mode-hook #'flycheck-mode)
  (add-hook 'c++-mode-hook #'flycheck-mode))

(use-package neotree
  :bind ("M-0" . neotree-show)
  :bind ("C-x t t" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-fixed-size nil))

(use-package lsp-mode
  :config
  (require 'lsp-clients) ;; Multiple language configurations out of the box
  ;;; Enable lsp in all programming modes
  (add-hook 'prog-mode-hook 'lsp)

  (use-package cquery
    :config
    (setq cquery-executable "/usr/local/bin/cquery"))

  ;;; Additional lsp-related packages
  (use-package lsp-ui
    :init
    (setq lsp-ui-sideline-ignore-duplicate t)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  (use-package company-lsp
    :requires company
    :init
    (push 'company-lsp company-backends)))

;;;
;; Non-Coding related packages
;;;

(use-package org
  :mode "\\.org$"
  :bind ("C-c c" . org-capture)
  :bind ("C-c o" . (lambda () (interactive) (find-file "~/.notes"))))
(use-package eww
  :commands eww
  :config
  (setq-default eww-search-prefix "https://duckduckgo.com/lite/?q="))
