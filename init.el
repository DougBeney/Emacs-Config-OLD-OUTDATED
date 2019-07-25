;; Fix bullshit bug
(setq-default gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
(add-to-list 'exec-path "/home/doug/.local/bin")

;;; Highlight current line
;;;(global-hl-line-mode 1)

;;; Open corresponding header or source file
(defun toggle-header-source ()
  (interactive)
  (ff-find-other-file nil t))
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "<f4>") 'toggle-header-source)))

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
  (setq comint-process-echoes nil))

;;;;;;;;;;;;;;;;;;;;;;
;; Package Config   ;;
;;;;;;;;;;;;;;;;;;;;;;

;;;
;; Themes
;;;

(use-package atom-one-dark-theme :defer t)
(use-package dracula-theme :defer t)
(use-package doom-themes :defer t)

;;;
;; Syntax Modes
;;;

(use-package pug-mode)
(use-package vue-mode)
(use-package php-mode)
(use-package qml-mode)
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
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(use-package smartparens
  :init
  (add-hook 'prog-mode-hook #'smartparens-mode))

(use-package expand-region
  :bind ("C-;" . er/expand-region))

(use-package emmet-mode
  :commands emmet-mode
  :hook (sgml-mode php-mode))

(use-package yafolding
  :commands yafolding-mode
  :hook (prog-mode . yafolding-mode)
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

;;; Open terminal in working directory using C-c t
(defun open-terminal-in-workdir ()
  (interactive)
  (let ((workdir (if (projectile-project-root)
                     (projectile-project-root)
                   default-directory)))
    (call-process-shell-command
     (concat "urxvt -cd " workdir) nil 0)))

(global-set-key (kbd "C-c t") 'open-terminal-in-workdir)

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
  ;;   :config0
  ;;   (add-hook 'geiser-mode-hook 'ac-geiser-setup)
  ;;   (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
  ;;   (eval-after-load "auto-complete"
  ;;     '(add-to-list 'ac-modfdfdfdes 'geiser-repl-mode)))
  )

(use-package srefactor
  :config
  (add-hook 'c-mode-hook 'semantic-mode)
  (add-hook 'c++-mode-hook 'semantic-mode)
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))

(use-package elpy
  :init
  (elpy-enable))

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
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  :init
  (projectile-mode +1)
  (require 'helm-projectile)
  (helm-projectile-on))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package yasnippet
  :init
  (use-package yasnippet-snippets))

(use-package company
  :commands company-complete
  :bind ("M-/" . company-complete)
  :hook (after-init-hook . global-company-mode)
  :init
  (use-package company-lsp)
  :config
  (setq company-idle-delay 0)
  (push 'company-yasnippet company-backends)
  (push 'company-lsp company-backends))

(use-package lsp-mode
  :config
  (use-package flycheck)
  (use-package lsp-ui
    :config
    (setq lsp-prefer-flymake nil) ;; Don't use flymake; we'll use flycheck.
    (setq lsp-ui-sideline-enable nil) ;; Disable sideline)
  (use-package ccls) ;; C/C++ language server

  (require 'lsp-clients) ;; Multiple language configurations out of the box

  ;; (remove-hook 'lsp-eldoc-hook #'lsp-document-highlight)

  ;;; Enable lsp in certain programming modes
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'javsacript-mode-hook 'lsp)
  (add-hook 'css-mode-hook 'lsp)
  (add-hook 'vue-mode-hook 'lsp)))

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
  ;; :hook (markdown-mode . olivetti-mode)
  ;; :hook (org-mode .  olivetti-mode)
)

(use-package eww
  :commands eww
  :config)
(put 'erase-buffer 'disabled nil)
