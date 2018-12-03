(setq package-list
 '(
	 atom-one-dark-theme
	 dracula-theme
	 monokai-theme
	 ;;	elpy
	 anaconda-mode
	 helm
	 undo-tree
	 slime
	 org
	 magit
	 diff-hl
	 autopair
	 emmet-mode
	 pug-mode
	 vue-mode
	 yafolding
	 yasnippet
	 irony
	 company
	 company-irony
	 flycheck
	 flycheck-irony
	 cmake-mode
	 neotree
	 bash-completion
	 yafolding
	 projectile
	 helm-projectile
	 all-the-icons
))
(load "~/.emacs.d/sanemacs.el")

;;;;;;;;;;;;;;;;;;;;;;
;; General Config   ;;
;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-width 2)

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Path
(setenv "PATH" (concat
								"/usr/local/bin:"
								"/home/doug/.local/bin:"
								"/home/doug/.npm/bin:"
								(getenv "PATH")))

;;; Confirm Emacs quit
;;(setq confirm-kill-emacs 'y-or-n-p)

;;; Line number mode
(global-set-key (kbd "C-c l") 'linum-mode)

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

;;;;;;;;;;;;;;;;;;;;;;
;; Package Config   ;;
;;;;;;;;;;;;;;;;;;;;;;

;;; Autopair
(autopair-global-mode)

;;; diff-hl-mode
(setq-default diff-hl-mode t)

;;; vue-mode / mmm-mode
(setq-default mmm-submode-decoration-level 0)

;;; Neotree
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-fixed-size nil) ;; Make resizable!
(global-set-key (kbd "M-0") 'neotree-show)
(global-set-key (kbd "C-x t t") 'neotree-toggle)

;;; Yafolding
(define-key yafolding-mode-map (kbd "M-<return>") 'yafolding-toggle-element)

;;; Helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;; eww
(setq-default eww-search-prefix "https://duckduckgo.com/lite/?q=")

;;; Slime / Lisp
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(add-hook 'slime-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c l") 'slime-repl-clear-buffer)))

;; Undo Tree
(global-undo-tree-mode)

;; Company Mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Flycheck
(add-hook 'c-mode-hook #'flycheck-mode)
(add-hook 'c++-mode-hook #'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Emmet
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; Python
;;(setq elpy-rpc-python-command "python3")
;;(elpy-enable)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(setq python-shell-interpreter "python3")


;; EShell
(bash-completion-setup) ;; Bash completion in eshell
(global-set-key (kbd "C-S-T") 'eshell)
(defun eshell/pjects ()
  (cd "~/Code"))
(defun eshell/mkcd (dir)
  (mkdir dir)
  (cd dir))

(defun eshell/myclear ()
  "Clears the eshell the proper way"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook
 'eshell-mode-hook
 '(lambda()
    (local-set-key (kbd "C-l") 'eshell/myclear)))
(put 'downcase-region 'disabled nil)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "M-m") 'projectile-command-map)
(require 'helm-projectile)
(helm-projectile-on)
