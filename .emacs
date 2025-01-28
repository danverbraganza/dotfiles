;;; .emacs --- Danver Braganza's Emacs file
;;; Just my local emacs file

(setq package-archives '(("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(require 'package)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(package-install 'use-package)

(use-package go-mode
  :ensure t
  :hook
  ('before-save . 'gofmt-before-save)
)

(use-package toml-mode
  :ensure t)

(use-package python-mode
  :ensure t
  :hook
      (python-mode . eglot-ensure)
  :custom
    (python-tab-width 4)
    (python-indent 4)
    (python-shell-interpreter "python-3")
)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

(use-package magit :ensure t)

(use-package company-jedi :ensure t)

(use-package company :ensure t)

(use-package blacken :ensure t)

(use-package helm-swoop :ensure t)

(use-package prettier-js :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

(use-package flycheck-pyflakes
  :ensure t
  :after flycheck
  )

; TODO: mypy checks are not working
(use-package flycheck-mypy
  :ensure t
  :after flycheck
  )


(use-package typescript-mode
  :ensure t
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))
  :requires tree-sitter
)


;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :hook (
  (typescript-mode . (lambda () (tsi-typescript-mode 1)))
  (json-mode . (lambda () (tsi-json-mode 1)))
  (css-mode . (lambda () (tsi-css-mode 1)))
  (scss-mode . (lambda () (tsi-scss-mode 1)))))


(use-package add-node-modules-path :ensure t)

(add-hook 'java-mode-hook (lambda ()
                           (setq c-basic-offset 4)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package yaml-mode :ensure t
  :mode ("\\.yml$" . yaml-mode)
  :init
  (lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent))
)

(use-package eglot :ensure t)

(setq python-shell-interpreter "python3")

(use-package protobuf-mode :ensure t
  :mode ("\\.proto$" . protobuf-mode))

(add-hook 'after-load-hook (lambda ()
                           (setq py-indent-offset 2)
                           (set-fill-column 80)
                           (highlight-beyond-fill-column)))

(put 'set-goal-column 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 1)
 '(blacken-line-length 150)
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(custom-enabled-themes '(tango-dark))
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(flymake-start-on-flymake-mode t)
 '(global-auto-revert-mode 1)
 '(highlight-beyond-fill-column t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(copilot ivy zzz-to-char editorconfig toml-mode lsp-pyright eglot-tempel typescript-mode flycheck-mypy flycheck flycheck-go go-flycheck flycheck-pyflakes go-mode prettier-js elpy isortify company-jedi yaml-mode transient python-mode python py-autopep8 protobuf-mode magit json-mode flymake-yaml flymake-python-pyflakes flymake-go flymake-cursor coffee-mode blacken))
 '(py--delete-temp-file-delay 0.1)
 '(py-paragraph-re "*")
 '(sentence-end-double-space nil)
 '(tab-width 4)
 '(transient-mark-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:background "dark magenta"))))
 '(flymake-infoline ((((class color)) (:background "grey15"))))
 '(flymake-warning ((((class color)) (:background "grey30"))))
 '(font-lock-comment-face ((t (:foreground "color-40")))))

(setq skeleton-pair nil)

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))


;; From Martin Fowler's Blog
(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

(use-package ivy
  :demand t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-ignore-buffers '())
  (setq ivy-re-builders-alist '(
                                (t . ivy--regex-ignore-order)
                                ))
  (setq ivy-height 10)
  (setq counsel-find-file-at-point nil)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure t
  :bind (
         ("C-x C-b" . ivy-switch-buffer)
         ("M-r" . counsel-rg)
         ("C-x C-d" . counsel-dired)
	 ("C-x d" . dired)
	 ("C-x b" . ivy-switch-buffer)
         )
  :diminish
  :config
  (global-set-key [remap org-set-tags-command] #'counsel-org-tag))

(use-package swiper
  :ensure t
  :bind(("M-C-s" . swiper)))

(use-package ivy-hydra
  :ensure t)

(use-package copilot
  :ensure t
  :bind(
		("C-c M-f" . copilot-accept-completion-by-word)
		("C-c C-n" . copilot-accept-completion-by-line)
		("C-c <TAB>" . copilot-accept-completion-by-paragraph)
		; The next line will bind copilot-mode to C-c C-c
		("C-c C-c" . copilot-mode)
  ))

;; From https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; by Mickey Petersen
(when nil
      (setq treesit-language-source-alist
	    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	     (cmake "https://github.com/uyha/tree-sitter-cmake")
	     (css "https://github.com/tree-sitter/tree-sitter-css")
	     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	     (go "https://github.com/tree-sitter/tree-sitter-go")
	     (html "https://github.com/tree-sitter/tree-sitter-html")
	     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	     (json "https://github.com/tree-sitter/tree-sitter-json")
	     (make "https://github.com/alemuller/tree-sitter-make")
	     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	     (python "https://github.com/tree-sitter/tree-sitter-python")
	     (toml "https://github.cm/tree-sitter/tree-sitter-toml")
	     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

      (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
)
