;; Place font before sanemacs so that I can change the
;; font size in custom.el on a computer-by-computer basis.
(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :height 100
                    :weight 'normal
                    :width 'normal)

(load "~/.emacs.d/sanemacs.el")

;;;;;;;;;;;;;;;;;;;;;;
;; General Config   ;;
;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Compile shortcut key
(global-set-key (kbd "C-c C-,") #'recompile)

;; Path
(setenv "PATH" (concat
                "/usr/bin:"
                "/usr/local/bin:"
                "/home/doug/.gem/bin:"
                "/home/doug/.local/bin:"
                "/home/doug/.npm/bin:"
                (getenv "PATH")))

(add-to-list 'exec-path "/home/doug/.npm/bin")

;;; Highlight current line
;;(global-hl-line-mode 1)

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
(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package shell
  :config
  (setq comint-prompt-read-only t)
  (setq comint-process-echoes t))

;;;;;;;;;;;;;;;;;;;;;;
;; Package Config   ;;
;;;;;;;;;;;;;;;;;;;;;;

;;;
;; Themes
;;;

;;(use-package atom-one-dark-theme)
;;(use-package dracula-theme)
;;(use-package monokai-theme)
(use-package doom-themes)

;;;
;; Syntax Modes
;;;

(use-package pug-mode)
(use-package yaml-mode)
(use-package sass-mode)
(use-package cmake-mode)
(use-package all-the-icons)
(use-package markdown-mode)

;;;
;; Basic Enhancements
;;;

(use-package diff-hl
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package autopair
  :config
  (autopair-global-mode))

;;; Expand region
(use-package expand-region
  :bind ("C-;" . er/expand-region))

;;(use-package autopair :init (autopair-global-mode))

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

;;; Avy, an alternative to ace
(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-\"") 'avy-goto-char-2)
  (global-set-key (kbd "M-g g") 'avy-goto-line))

(use-package multiple-cursors)

;;;
;; Language-related tools
;;;

(defun django-shell ()
  (interactive)
  (let ((python-shell-interpreter (read-file-name "Locate manage.py "))
        (python-shell-interpreter-args "shell"))
    (run-python (python-shell-calculate-command) nil t)))

(use-package slime
  :init
  (use-package slime-company)
  (slime-setup '(slime-fancy slime-company))
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package geiser ;;; A scheme-related package
  :config
  ;; (use-package ac-geiser
  ;;   :config
  ;;   (add-hook 'geiser-mode-hook 'ac-geiser-setup)
  ;;   (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
  ;;   (eval-after-load "auto-complete"
  ;;     '(add-to-list 'ac-modes 'geiser-repl-mode)))
  )

(use-package srefactor
  :config
  (add-hook 'c-mode-hook 'semantic-mode)
  (add-hook 'c++-mode-hook 'semantic-mode)
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))

(use-package pyvenv)

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
  ;;; Setting ignored projectile directories
  (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
  (add-to-list 'projectile-globally-ignored-directories "qmake")
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  :init
  (projectile-mode +1)
  (require 'helm-projectile)
  (helm-projectile-on))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package yasnippet)

;; (use-package auto-complete
;;   :commands aut-complete
;;   :bind ("M-/" . auto-complete)
;;   :config
;;   (ac-config-default))

(use-package company
  :commands company-complete
  :bind ("M-/" . company-complete)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(use-package neotree
  :bind ("M-0" . neotree-show)
  :bind ("C-x t t" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-fixed-size nil))

;; (use-package treemacs
;;   :bind ("C-x t t" . treemacs)
;;   :bind ("M-0" . treemacs-select-window))

;; (use-package irony
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;   (use-package flycheck
;;     :config
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

(use-package lsp-mode
  :config
  (require 'lsp-clients) ;; Multiple language configurations out of the box
  (setq lsp-prefer-flymake nil) ;; Don't use flymake; we'll use flycheck.
  (setq lsp-ui-sideline-enable nil) ;; Disable sideline
  (remove-hook 'lsp-eldoc-hook #'lsp-document-highlight)

  ;;; Enable lsp in certain programming modes
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'javsacript-mode-hook 'lsp)
  (add-hook 'css-mode-hook 'lsp)
  (add-hook 'vue-mode-hook 'lsp)


  (use-package flycheck)
  (use-package lsp-ui)
  (use-package vue-mode)

  ;;; C / C++
  ;;Using cquery
  (use-package cquery
    :config
    (setq cquery-executable "/usr/bin/cquery"))

  (use-package company-lsp
    :requires company
    :init
    (push 'company-lsp company-backends))

  ;;; Ruby
  (setq exec-path (append exec-path '("/home/doug/.gem/bin"))))

;;; Eyebrowse - workspaces in Emacs
(use-package eyebrowse
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

;;;
;; Non-Coding related packages
;;;

(use-package org
  :mode ("\\.org$" . org-mode)
  :mode ("~/Drive/Notes/index.org" . org-mode)
  :bind ("C-c c" . org-capture)
  :bind ("C-c o" . (lambda () (interactive) (find-file "~/Drive/Notes/index.org")))
  :config
  ;; (use-package org-bullets
  ;;   :init (add-hook 'org-mode-hook 'org-bullets-mode))
  )

(use-package olivetti
  :hook (markdown-mode . olivetti-mode)
  :hook (org-mode .  olivetti-mode))

(use-package eww
  :commands eww
  :config
  (setq-default eww-search-prefix "https://duckduckgo.com/lite/?q="))
(put 'erase-buffer 'disabled nil)

(use-package emojify
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))
