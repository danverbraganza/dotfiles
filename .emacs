(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)


(defvar my-packages
  '(flymake flymake-go flymake-python-pyflakes flymake-yaml flymake-easy go-mode magit git-commit dash protobuf-mode python-mode transient with-editor async yaml-mode)
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

;(add-hook 'find-file-hook 'flymake-find-file-hook)

(setq python-indent 4)
(setq js-indent-level 2)
(setq-default indent-tabs-mode nil)

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

; Then run:
; sudo pip install pyflakes


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(fill-column 80)
 '(highlight-beyond-fill-column t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (go-mode protobuf-mode yaml-mode flymake magit python-mode flymake-yaml flymake-python-pyflakes flymake-go)))
 '(sentence-end-double-space nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "dark magenta"))))
 '(flymake-infoline ((((class color)) (:background "grey15"))))
 '(flymake-warnline ((((class color)) (:background "grey30")))))


;(require 'py-autopep8)
;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;(setq py-autopep8-options '("--ignore=E712"))

(setq skeleton-pair nil)


(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)

(fset 'new-python-bin
   "import logging\C-m\C-mfrom tornado import gen\C-mfrom tornado.options import define\C-m\C-mfrom hipmunk.lib import start\C-mfrom hipmunk.options import options, remaining_options\C-m\C-m\C-m@start.ioloop\C-mdef main(ioloop, *args):\C-mtry:\C-m#insert code here\C-m\C-mfinally:\C-mreturn ioloop.stop()\C-m\C-m\C-m\C-m\C-?\C-?\C-?\C-?if __name__ == '__main__':\C-mmain(*remaining_options)\C-m")
