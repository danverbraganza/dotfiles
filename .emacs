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

(load-file "/usr/local/share/emacs/site-lisp/emacs-for-python/epy-init.el")

(setq python-indent 4)
(setq js-indent-level 2)
(setq-default indent-tabs-mode nil)

(add-hook 'java-mode-hook (lambda ()
                           (setq c-basic-offset 4)))

(require 'flymake)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(add-hook 'feature-mode-hook
          '(lambda ()
             (define-key feature-mode-map "\C-m" 'newline-and-indent)))


(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/python-mode.el-6.0.10")
(setq py-install-directory "/usr/local/share/emacs/site-lisp/python-mode.el-6.0.10")
(require 'python-mode)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/go-mode.el/")
(require 'go-mode-autoloads)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/goflymake")
(require 'go-flymake)

(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\.proto$" . protobuf-mode))

(add-hook 'after-load-hook (lambda ()
                           (setq py-indent-offset 2)
                           (set-fill-column 80)
                           (highlight-beyond-fill-column)))

(put 'set-goal-column 'disabled nil)

; Then run:
; pushd /usr/local/share/emacs/site-lisp/
; sudo wget https://raw.github.com/yoshiki/yaml-mode/master/yaml-mode.el
; sudo wget https://raw.github.com/michaelklishin/cucumber.el/master/feature-mode.el
; sudo pip install pyflakes
; sudo wget http://www.emacswiki.org/emacs/download/flymake-cursor.el
; sudo wget https://launchpad.net/python-mode/trunk/6.0.10/+download/python-mode.el-6.0.10.tar.gz
; sudo tar -xzvf python-mode.el-6.0.10.tar.gz
; sudo wget http://protobuf.googlecode.com/svn/trunk/editors/protobuf-mode.el
; sudo git clone https://github.com/gabrielelanaro/emacs-for-python.git
; sudo git clone https://github.com/dominikh/go-mode.el.git
; sudo git clone https://github.com/dougm/goflymake.git
; M-x package-install [RET] coffee-mode [RET]

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(fill-column 80)
 '(highlight-beyond-fill-column t)
 '(sentence-end-double-space nil)
 '(column-number-mode t)
)

(custom-set-faces
 '(flymake-errline ((((class color)) (:background "dark magenta"))))
 '(flymake-warnline ((((class color)) (:background "grey30"))))
 '(flymake-infoline ((((class color)) (:background "grey15")))))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--ignore=E712"))

(setq skeleton-pair nil)
(custom-set-variables '(coffee-tab-width 4))

(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)

(fset 'new-python-bin
   "import logging\C-m\C-mfrom tornado import gen\C-mfrom tornado.options import define\C-m\C-mfrom hipmunk.lib import start\C-mfrom hipmunk.options import options, remaining_options\C-m\C-m\C-m@start.ioloop\C-mdef main(ioloop, *args):\C-mtry:\C-m#insert code here\C-m\C-mfinally:\C-mreturn ioloop.stop()\C-m\C-m\C-m\C-m\C-?\C-?\C-?\C-?if __name__ == '__main__':\C-mmain(*remaining_options)\C-m")
