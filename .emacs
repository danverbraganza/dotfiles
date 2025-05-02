;;; .emacs --- Danver Braganza's Emacs file
;;; Just my local emacs file

(setq package-archives '(
						 ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
))

(require 'package)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-c") nil)
  (define-key python-mode-map (kbd "C-c C-<TAB>") nil)
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
  :init (global-flycheck-mode) ;; Enable Flycheck globally
  :hook ((typescript-ts-mode . flycheck-mode)  ;; Use tree-sitter TypeScript mode
         (tsx-ts-mode . flycheck-mode))        ;; Use tree-sitter TSX mode
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

(add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter")

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
   '(lsp-ui lsp-mode xclip tree-sitter-indent tree-sitter-langs tree-sitter copilot
			ivy zzz-to-char editorconfig toml-mode lsp-pyright eglot-tempel
			typescript-mode flycheck-mypy flycheck flycheck-go go-flycheck
			flycheck-pyflakes go-mode prettier-js elpy isortify company-jedi
			yaml-mode transient python-mode python py-autopep8 protobuf-mode
			magit json-mode flymake-yaml flymake-python-pyflakes flymake-go
			flymake-cursor coffee-mode blacken))
 '(py--delete-temp-file-delay 0.1)
 '(py-paragraph-re "*")
 '(sentence-end-double-space nil)
 '(tab-width 4)
 '(transient-mark-mode nil))

(package-refresh-contents)
(package-install-selected-packages)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:background "dark magenta"))))
 '(flymake-infoline ((((class color)) (:background "grey15"))))
 '(flymake-warning ((((class color)) (:background "grey30"))))
 '(font-lock-comment-face ((t (:foreground "green"))))
 '(lsp-face-semhl-comment ((t nil))))

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

  ;; ────────────────────────────────────────────────────────────
  ;; 1.  Create an “AI” prefix map on C-c .
  ;;     (punctuation after C-c is almost never taken)
  ;; ────────────────────────────────────────────────────────────
  :init
  (define-prefix-command 'my/copilot-prefix)      ; new sparse keymap
  (global-set-key (kbd "C-c .") 'my/copilot-prefix)

  ;; ────────────────────────────────────────────────────────────
  ;; 2.  Bind Copilot commands under that prefix
  ;;     You can hit C-c . ? to see them if you use which-key
  ;; ────────────────────────────────────────────────────────────
  :config
  (define-key my/copilot-prefix (kbd "a") #'copilot-accept-completion)           ; C-c . a
  (define-key my/copilot-prefix (kbd "w") #'copilot-accept-completion-by-word)   ; C-c . w
  (define-key my/copilot-prefix (kbd "l") #'copilot-accept-completion-by-line)   ; C-c . l
  (define-key my/copilot-prefix (kbd "p") #'copilot-accept-completion-by-paragraph) ; C-c . p
  (define-key my/copilot-prefix (kbd "n") #'copilot-next-completion)             ; C-c . n
  (define-key my/copilot-prefix (kbd "b") #'copilot-previous-completion)         ; C-c . b (back)
  (define-key my/copilot-prefix (kbd "c") #'copilot-clear-overlay)               ; C-c . c
  (define-key my/copilot-prefix (kbd "t") #'copilot-mode))                       ; C-c . t (toggle)


(setq gptel-api-key (getenv "OPENAI_API_KEY"))

; Sourced from Modern Emacs Web Development
; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config#orgf6d33f7
(use-package treesit
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python"))
                   (go "https://github.com/tree-sitter/tree-sitter-go")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '(
				 (python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 ;(bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))


; Again, sourced from Modern Emacs Web Development
; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config#orgf6d33f7
(use-package lsp-mode
      :diminish "LSP"
      :ensure t
      :hook ((lsp-mode . lsp-diagnostics-mode)
             (lsp-mode . lsp-enable-which-key-integration)
             ((tsx-ts-mode
               typescript-ts-mode
               js-ts-mode) . lsp))
      :custom
      (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
      (lsp-completion-provider :none)       ; Using Corfu as the provider
      (lsp-diagnostics-provider :flycheck)
      (lsp-sesion-file (locate-user-emacs-file ".lsp-session"))
      (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
      (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
      (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
      ;; core
      (lsp-enable-xref t)                   ; Use xref to find references
      (lsp-auto-configure t)                ; Used to decide between current active servers
      (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
      (lsp-enable-dap-auto-configure t)     ; Debug support
      (lsp-enable-file-watchers nil)
      (lsp-enable-folding t)
      (lsp-enable-imenu t)
      (lsp-enable-indentation nil)          ; I use prettier
      (lsp-enable-links t)
      (lsp-enable-on-type-formatting nil)   ; Prettier handles this
      (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
      (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
      (lsp-enable-text-document-color nil)   ; This is Treesitter's job

      (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
      (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
      ;; completion
      (lsp-completion-enable t)
      (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
      (lsp-enable-snippet t)                         ; Important to provide full JSX completion
      (lsp-completion-show-kind t)                   ; Optional
      ;; headerline
      (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
      (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
      (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
      (lsp-headerline-breadcrumb-icons-enable nil)
      ;; modeline
      (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
      (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
      (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
	  (lsp-pyright-auto-import-completions t)    ; ChatGPT o4 said this will enable import completions for me
      (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
      (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
      (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
      ;; lens
      (lsp-lens-enable nil)                 ; Optional, I don't need it
      ;; semantic
      (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

	  (setq lsp-clients-typescript-server "typescript-language-server"
			lsp-clients-typescript-server-args '("--stdio"))

      :init
      (setq lsp-use-plists nil
			lsp-clients-typescript-tsserver-executable (executable-find "typescript-language-server")))

    (use-package lsp-completion
      :no-require
      :hook ((lsp-mode . lsp-completion-mode)))

    (use-package lsp-ui
      :ensure t
      :commands
      (lsp-ui-doc-show
       lsp-ui-doc-glance)
      :bind (:map lsp-mode-map
                  ("C-c C-d" . 'lsp-ui-doc-glance))
	  :hook (lsp-mode . lsp-ui-mode)
      :after (lsp-mode evil)
      :config (setq lsp-ui-doc-enable t
                    lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                    lsp-ui-doc-include-signature t       ; Show signature
                    lsp-ui-doc-position 'at-point))


(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'tsx-mode-hook #'lsp-deferred)  ;; For React (TSX) files
(add-hook 'python-ts-mode-hook #'lsp-deferred)

; Enable xclip mode globally
(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))
