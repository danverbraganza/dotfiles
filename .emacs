(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(require 'package)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar my-packages
  '(flymake flymake-go flymake-python-pyflakes flymake-yaml flymake-easy go-mode magit git-commit dash protobuf-mode python-mode transient with-editor async yaml-mode company-jedi company blacken helm-swoop elpy web-mode flymake-eslint prettier-js add-node-modules-path counsel swiper ivy-hydra)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (let ((value t))
    (dolist (p my-packages value)
      (if (not (package-installed-p p))
	  (setq value nil)))
       value))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  ;(add-to-list 'flymake-allowed-file-name-masks
  ;             '("\\.py\\'" flymake-pyflakes-init))
  )

(add-hook 'find-file-hook 'flymake-find-file-hook)
(setq-default indent-tabs-mode t)

(setq python-indent 4)

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
(add-hook 'web-mode-hook ; or whatever the mode-hook is for your mode of choice
  (lambda ()
    (flymake-eslint-enable)))

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)


(setq js-indent-level 4)


(elpy-enable)
(eval-after-load "elpy"
  '(cl-dolist (key '("C-<down>"  "C-<up>"));
     (define-key elpy-mode-map (kbd key) nil)))

(setq elpy-rpc-python-command 'python3)

(add-hook 'java-mode-hook (lambda ()
                           (setq c-basic-offset 4)))

(require 'flymake)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'before-save-hook 'gofmt-before-save)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

'(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(require 'python-mode)
(setq python-shell-interpreter "python3")

(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\.proto$" . protobuf-mode))

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
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-revert-interval 1)
 '(blacken-line-length 150)
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(custom-enabled-themes '(deeper-blue))
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(flymake-start-on-flymake-mode t)
 '(global-auto-revert-mode 1)
 '(highlight-beyond-fill-column t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(go-mode prettier-js elpy isortify company-jedi yaml-mode transient python-mode python py-autopep8 protobuf-mode magit json-mode flymake-yaml flymake-python-pyflakes flymake-go flymake-cursor coffee-mode blacken))
 '(py--delete-temp-file-delay 0.1)
 '(py-paragraph-re "*")
 '(sentence-end-double-space nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:background "dark magenta"))))
 '(flymake-infoline ((((class color)) (:background "grey15"))))
 '(flymake-warning ((((class color)) (:background "grey30"))))
 '(font-lock-comment-face ((t (:foreground "color-40")))))


;(require 'py-autopep8)
;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;(setq py-autopep8-options '("--ignore=E712"))
(setq py-autopep8-options '("--max-line-length=120" "--ignore=E402"))

(setq skeleton-pair nil)

(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)

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
  (setq ivy-ignore-buffers '(\\` " \\`\\*magit"))
  (setq ivy-re-builders-alist '(
                                (t . ivy--regex-ignore-order)
                                ))
  (setq ivy-height 10)
  (setq counsel-find-file-at-point t)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :bind (
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x b" . ivy-switch-buffer)
         ("M-r" . counsel-ag)
         ("C-x C-d" . counsel-dired)
         ("C-x d" . counsel-dired)
         )
  :diminish
  :config
  (global-set-key [remap org-set-tags-command] #'counsel-org-tag))

(use-package swiper
  :bind(("M-C-s" . swiper)))

(use-package ivy-hydra)

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
	     (toml "https://github.com/tree-sitter/tree-sitter-toml")
	     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

      (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
)
