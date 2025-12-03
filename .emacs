;;; .emacs --- Danver Braganza's Emacs file
;;; Just my local emacs file
(require 'cl-lib)

(setq package-archives '(
						 ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
))

(setq read-process-output-max (* 4 1024 1024)) ; 4 MB, good for rust-analyzer
(global-display-line-numbers-mode 0) ; Disabled because Goto line is so fast

(setq lsp-enable-file-watchers t      ; Used by lsp, set this up early.
      lsp-file-watch-threshold 5000)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default skeleton-pair nil)
(setq next-screen-context-lines 10)


(require 'package)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Save place in files
(save-place-mode 1)
;; Remember recent files for consult-recent-file
(recentf-mode 1)

;; Auto-revert but be quiet
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)


;;; ─── Navigation: whitespace jumps on C-↑/↓ (with macOS TTY fallback) ─────────
(when (and (eq system-type 'darwin) (not (display-graphic-p)))
  ;; If you previously mapped M-Arrows to line moves, unbind them *only here*
  ;; so the fallback can take effect in macOS terminal without touching Linux.
  (keymap-global-unset "M-<up>")
  (keymap-global-unset "M-<down>")
  ; Jump forward to next blank line
  (global-set-key (kbd "M-<down>") (lambda () (interactive) (forward-paragraph)))
  (global-set-key (kbd "M-<up>")   (lambda () (interactive) (backward-paragraph))))


;; ----------------- Vertico stack -----------------
;; 1. Vertico: completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

;; 2. Save minibuffer history across sessions
(use-package savehist
  :init
  (savehist-mode 1))

;; 3. Rich annotations next to candidates
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;; 4. Smarter matching style
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil))

;; 5. Consult: high-level commands
(use-package consult
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
		 ("M-r"     . consult-ripgrep)
		 ("M-a"     . consult-fd)
		 ("C-x C-d" . consult-dir)
		 ("C-s"     . consult-line)
		 ("C-x C-y" . consult-yank-from-kill-ring))
  :custom
  (consult-narrow-key "<")              ;; like Ivy’s M-j
  ;; Project root  ➜  prefer the first‑level "sculptor" directory if it exists;
  ;; otherwise fall back to the real project root.
  (consult-project-function
   (lambda (_dir)                       ; must accept one arg
     (when-let ((root (when (fboundp 'project-root)
                        (project-root (project-current)))))
       (let ((sub (expand-file-name "sculptor" root)))
         (if (file-directory-p sub) sub root)))))
  )


;; 7. Embark for context actions
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)            ;; like Ivy hydra
         ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))
;; -------------------------------------------------

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package go-mode
  :ensure t
  :hook
  ('before-save . 'gofmt-before-save)
)

(use-package toml-mode
  :ensure t)

(use-package ruff-format
  :ensure t
  :hook ((python-mode python-ts-mode python-ts-base-mode)
         . (lambda ()
             (ruff-format-on-save-mode 1))))

(use-package python
  :ensure t
  :after ruff-format
  :preface
  (defun danver/ruff-fix-after-save ()
    "Run `ruff check --fix` for the just-saved file from project root."
    (when buffer-file-name
      (let* ((proj (when-let ((p (project-current)))
                     (car (project-roots p))))
             (default-directory (or proj default-directory)))
        (start-process "ruff-fix" nil "ruff" "check" "--fix" "--exit-zero" buffer-file-name))))

  (defun danver/python-ruff-setup ()
    (ruff-format-on-save-mode 1)
    (add-hook 'after-save-hook #'danver/ruff-fix-after-save nil t))
  (defun danver/python-use-ty ()
    "Prefer the ty LSP for Python in this buffer."
    (setq-local lsp-enabled-clients '(ty)))
  :hook ((python-ts-base-mode . danver/python-use-ty)
         (python-ts-base-mode . lsp-deferred)
         (python-ts-base-mode . flycheck-mode)
         (python-ts-base-mode . danver/python-ruff-setup))
  :init
  ;; If you sometimes open legacy python-mode buffers (non-tree-sitter),
  ;; keep the same behavior there too.
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local lsp-enabled-clients '(ty))))

  :config
  (setq python-shell-interpreter "python3"))


(defun check-rust-analyzer ()
  "Check if rust-analyzer is installed and in PATH."
  (interactive)
  (if (executable-find "rust-analyzer")
      (message "✅ rust-analyzer is installed and in your PATH.")
    (user-error "❌ rust-analyzer is NOT installed or not in your PATH. Run: rustup component add rust-analyzer")))


(use-package rust-mode
  :ensure t
)

(use-package rust-ts-mode
  :ensure nil ;; rust-ts-mode is built-in with Emacs 29+
  :hook ((rust-ts-mode . cargo-minor-mode)
         (rust-ts-mode . check-rust-analyzer)))


(use-package cargo  :ensure t)


(defun my/rust-organise-before-save ()
  (when (derived-mode-p 'rust-ts-mode)
    (lsp-format-buffer)
    (lsp-organize-imports)))
(add-hook 'before-save-hook #'my/rust-organise-before-save)


(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c C-c") #'cargo-process-run)
  (define-key rust-ts-mode-map (kbd "C-c C-t") #'cargo-process-test)
  (define-key rust-ts-mode-map (kbd "C-c C-b") #'cargo-process-bench))


(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-c") nil)
  (define-key python-mode-map (kbd "C-c C-<TAB>") nil)
)


(use-package magit :ensure t)


; Keep Copilot from interfering with company
(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common)))


(use-package company
  :ensure t
  :diminish          ;; hide “Company” from mode‑line
  :hook (after-init . global-company-mode)  ;; turn it on everywhere
  :custom
  (company-minimum-prefix-length 1)   ;; start completing after 1 char
  (company-idle-delay 0.1)            ;; 0.1 s after you stop typing
  (company-tooltip-align-annotations t)
  :config
  ;; Set the keybinding once Company is fully loaded
  (define-key company-active-map (kbd "<tab>") #'my/copilot-tab)
  (define-key company-active-map (kbd "TAB") #'my/copilot-tab)
  )


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode) ;; Enable Flycheck globally
  :hook ((typescript-ts-mode . flycheck-mode)  ;; Use tree-sitter TypeScript mode
         (tsx-ts-mode . flycheck-mode))        ;; Use tree-sitter TSX mode
)

(use-package tree-sitter-indent :ensure t)
(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter :ensure t)
(use-package zzz-to-char :ensure t)
(use-package editorconfig :ensure t)
(use-package transient :ensure t)
(use-package json-mode :ensure t)
(use-package coffee-mode :ensure t)
(use-package consult-dir :ensure t)


(use-package flycheck-pyflakes
  :ensure t
  :after flycheck
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


(use-package protobuf-mode :ensure t
  :mode ("\\.proto$" . protobuf-mode))

(add-hook 'after-init-hook (lambda ()
                           (setq python-indent-offset 4)
                           (setq-default fill-column 120)))

(put 'set-goal-column 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 1)
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(custom-enabled-themes '(tango-dark))
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(flymake-start-on-flymake-mode t)
 '(global-auto-revert-mode 1)
 '(highlight-beyond-fill-column t)
 '(inhibit-startup-screen t)
 '(package-selected-packages nil)
 '(py--delete-temp-file-delay 0.1)
 '(py-paragraph-re "*")
 '(sentence-end-double-space nil)
 '(tab-width 4)
 '(transient-mark-mode t))

(unless package-archive-contents
  (package-refresh-contents))

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

(set-face-attribute 'region nil
                    :background nil
                    :foreground nil)


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


(use-package copilot
  :ensure t

  ;; ────────────────────────────────────────────────────────────
  ;; 1.  Create an “AI” prefix map on C-c .
  ;;     (punctuation after C-c is almost never taken)
  ;; ────────────────────────────────────────────────────────────
  :init
  (define-prefix-command 'my/copilot-prefix)      ; new sparse keymap
  (global-set-key (kbd "C-c '") 'my/copilot-prefix)

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
                                   (rust "https://github.com/tree-sitter/tree-sitter-rust" "v0.20.4")
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
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                                 (rust-mode . rust-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))




(defun danver/xref-dedup-by-line (xrefs)
  "Return XREFS with duplicates removed by file and line."
  (cl-delete-duplicates
   xrefs
   :test (lambda (a b)
           (let* ((la (xref-item-location a))
                  (lb (xref-item-location b)))
             (and (equal (xref-location-group la)
                         (xref-location-group lb))
                  (equal (xref-location-line la)
                         (xref-location-line lb)))))))

(defun danver/xref-show-one-or-prompt (fetcher alist)
  "If FETCHER returns one xref (after dedup), jump; if many, prompt."
  (let* ((xrefs (funcall fetcher))
         (uniq  (danver/xref-dedup-by-line xrefs)))
    (cond
     ((null uniq)
      (message "No locations found"))
     ((null (cdr uniq))
      (xref-pop-to-location (car uniq)
                            (assoc-default 'display-action alist)))
     (t
      (xref-show-definitions-completing-read
       (lambda () uniq)
       alist)))))

(use-package xref
  :ensure nil ; built-in
  :custom
  (xref-auto-jump-to-first-definition nil)
  (xref-auto-jump-to-first-xref nil)
  (xref-show-definitions-function #'danver/xref-show-one-or-prompt)
  (xref-show-xrefs-function       #'danver/xref-show-one-or-prompt))


; Again, sourced from Modern Emacs Web Development
; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config#orgf6d33f7
(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode        . lsp-deferred)
         (python-ts-mode     . lsp-deferred)
         (rust-ts-mode       . lsp-deferred))
 :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :capf)       ; Use standard completion-at-point
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
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
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
  (lsp-rust-analyzer-server-command '("rust-analyzer")) ; rustup component add rust-analyzer
  (lsp-rust-analyzer-cargo-watch-command "clippy")      ; show Clippy warnings live
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial") ; inlay hints
  (lsp-use-plists nil)
  (lsp-clients-typescript-server "typescript-language-server")
  (lsp-clients-typescript-server-args '("--stdio"))
  :init
  (setq lsp-use-plists nil
        lsp-clients-typescript-tsserver-executable (executable-find "typescript-language-server"))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ty" "server"))
    :activation-fn (lsp-activate-on "python")
    :server-id 'ty
    :priority 10
    ;; If ty uses init options, put them here:
    :initialization-options
    (lambda ()
      (ht
       ("diagnosticMode" "workspace") ; or "openFilesOnly"
       ("inlayHints" (ht
                      ("variableTypes" t)
                      ("callArgumentNames" t))))))))


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
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))


(use-package prettier
  :ensure t
  :hook ((typescript-ts-mode tsx-ts-mode json-ts-mode) . prettier-mode))


(use-package which-key
  :ensure t
  :init (which-key-mode))

; Enable xclip mode globally
(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))


(global-set-key (kbd "M-<up>")   #'previous-line)
(global-set-key (kbd "M-<down>") #'next-line)


;; Doom Modeline

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)               ;; adjust height
  (doom-modeline-bar-width 3)
  (doom-modeline-icon nil)                  ;; enable icons
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-minor-modes nil)         ;; keep it clean
  (doom-modeline-enable-word-count t)
  (doom-modeline-lsp t)                   ;; show LSP status
  (doom-modeline-vcs-max-length 20))

(use-package just-mode
  :ensure t
  :hook
  ;; ensure just-mode uses spaces, not tabs
  (just-mode . (lambda ()
                 (setq indent-tabs-mode nil
                       tab-width 4)))
  ;; add a buffer-local before-save hook to untabify
  (just-mode . (lambda ()
                 (add-hook 'before-save-hook #'untabify nil t))))


(provide '.emacs)
;;; .emacs ends here
