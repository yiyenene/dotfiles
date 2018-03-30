;; add load-path function
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
;; add directory of args recursive
(add-to-load-path "elisp" "conf" "public_repos")

;; set language
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(column-number-mode t)

(setq frame-title-format "%f")

(setq-default tab-width 2)

(setq-default indent-tabs-mode nil)

(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)

(define-key global-map (kbd "M-n") 'forward-paragraph)
(define-key global-map (kbd "M-p") 'backward-paragraph)

(define-key global-map (kbd "C-x C-b") 'ibuffer)

;; paren-mode
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; auto-revert
(global-auto-revert-mode t)

;; package.el
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(load-theme 'wombat t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (popwin direx dockerfile-mode rspec-mode slim-mode robe git-gutter magit projectile-rails helm-projectile projectile flycheck inf-ruby ruby-electric yaml-mode js2-mode sass-mode web-mode undo-tree undohist helm-c-moccur auto-complete helm init-loader)))
 '(ruby-insert-encoding-magic-comment nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'cl)
(let ((not-installed
       (loop for x in package-selected-packages
             when (not (package-installed-p x)) collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist
        (pkg not-installed)
        (package-install pkg))))

(require 'helm-config)

;; auto-complete
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))

;; color-mocur
(when (require 'color-mocur nil t)
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  (setq moccur-split-word t)
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$"))

;; undohist
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ; disable key bind

;; web-mode
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

;; ruby-electric
(add-hook  'ruby-mode-hook #'ruby-electric-mode)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; projectile
(when (require 'projectile nil t)
  (projectile-mode)
  (add-to-list
   'projectile-globally-ignored-directories
   "node-modules")
  (add-to-list
   'projectile-globally-ignored-directories
   "vendor/bundle")
  (setq projectile-enable-caching t))

;; helm-projectile
(when (require 'helm-projectile nil t)
  (setq projectile-completion-system 'helm))

;; projectile-rails
(when (require 'projectile-rails nil t)
  (projectile-rails-global-mode))

;; git-gutter
(when (require 'git-gutter nil t)
  (global-git-gutter-mode t))

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode 'ac-robe-setup)

(require 'slim-mode)
(setq slim-backspace-backdents-nesting nil)
(add-hook 'slim-mode-hook
          '(lambda ()
             (setq tab-width slim-indent-offset)))

(require 'rspec-mode)

(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'popwin)
(popwin-mode 1)

(defun direx-project-if ()
  (interactive)
  (let ((result (ignore-errors
                  (direx-project:jump-to-project-root-other-window)
                  t)))
    (unless result
      (direx:jump-to-directory-other-window))))

(global-set-key (kbd "C-x C-j") 'direx-project-if)
(push '(direx:direx-mode :position left :width 40 :dedicated t)
      popwin:special-display-config)

(provide 'init)
;;; init.el ends here
