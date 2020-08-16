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
  '(flymake flymake-go flymake-python-pyflakes flymake-yaml flymake-easy go-mode magit git-commit dash protobuf-mode python-mode transient with-editor async yaml-mode company-jedi company blacken helm-swoop)
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

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(setq python-indent 4)
(setq js-indent-level 2)
(setq-default indent-tabs-mode t)

(add-hook 'java-mode-hook (lambda ()
                           (setq c-basic-offset 4)))

(require 'flymake)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

'(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(require 'python-mode)

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
 '(custom-enabled-themes (quote (deeper-blue)))
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(flymake-start-on-flymake-mode t)
 '(global-auto-revert-mode 1)
 '(highlight-beyond-fill-column t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (company-jedi yaml-mode transient python-mode python py-autopep8 protobuf-mode magit json-mode go-mode flymake-yaml flymake-python-pyflakes flymake-go flymake-cursor coffee-mode blacken)))
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
