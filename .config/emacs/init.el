;;; init.el --- My custom Emacs LISP configuration.  -*- lexical-binding: t -*-
;;; package --- Summary
;;; A simple Emacs config that doesn't use any external packages.

;; init.el
(defun jlib/path-join (root &rest dirs)
  "Join ROOT with DIRS to construct a path string in an OS independent way."
  (let ((res root))
    (dolist (el dirs res)
      (setq res (expand-file-name el res)))))

;; Nice global settings.
(setq
 show-paren-delay 0
 mac-right-command-modifier 'control
 ;; Fix warnings/errors when opening Python files by default
 python-indent-guess-indent-offset t
 python-indent-guess-indent-offset-verbose nil
 ;; Fix a dired warning on Mac OS
 dired-use-ls-dired nil
 ;; Don't litter the directories in which I'm working with ~ and # files
 backup-directory-alist `(("." . ,(jlib/path-join (getenv "HOME") ".config" "emacs" "backups")))
 mac-command-modifier 'meta)

;; If you don't call `show-paren-mode' again after changing `show-paren-delay' to 0,
;; configuring show-paren-delay above does absolutely nothing.
(show-paren-mode)

;; Make sure that typing `:' in C++ mode doesn't backward indent.
(add-hook 'c++-mode-hook (lambda () (define-key c++-mode-map ":" nil)))

;; Put Emacs in fullscreen mode by default.
(toggle-frame-fullscreen)

;;; Simple, good enough modeline
(setq-default mode-line-format '(" %b | %l:%C "))

;; Let Emacs know that we understand what `a' does in Dired.
(put 'dired-find-alternate-file 'disabled nil)

;; Force Dired to revert when we make changes to directories.
(add-hook 'dired-mode-hook #'auto-revert-mode)

;; Make ', ", and ( autocomplete themselves.
(electric-pair-mode)

;; Useful global keybindings
(global-set-key (kbd "C-M-f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "M-q") #'save-buffers-kill-terminal)
(global-set-key (kbd "C-M-d") #'xref-find-definitions)

;; Add a key binding for toggling documentation.
(defun jlib/find-first (predicate list)
  "Return the first element in LIST that satisfies PREDICATE, or nil if none found."
  (catch 'found
    (dolist (elem list)
      (when (funcall predicate elem)
        (throw 'found elem)))
    nil))

(defun jlib/toggle-docs ()
  "Toggle documentation on and off."
  (interactive)
  (let ((the-window (jlib/find-first (lambda (el) (string-match-p "*eldoc*" (buffer-name (window-buffer el)))) (window-list))))
    (cond
     (the-window (delete-window the-window))
     (:otherwise (eldoc-doc-buffer t)))))

(global-set-key (kbd "C-M-h") #'jlib/toggle-docs)

;; Change mark from CMD+SPC to CMD+J, since CMD+SPC is bound to the search bar in
;; OSX.
(global-set-key (kbd "C-j") #'set-mark-command)
;; C-j breaks in the scratch buffer if we don't do this.
(define-key lisp-interaction-mode-map (kbd "C-j") #'set-mark-command)

(with-eval-after-load 'python
  ;; Without these magical lines of code, M-e and M-b don't work inside of
  ;; Python buffers.
  (define-key python-mode-map [remap backward-sentence] nil)
  (define-key python-mode-map [remap forward-sentence] nil))

;; Window management
(defvar *jlib/window-adjustment-amount* 10
  "Control how large window adjustments are.")

(defun jlib/push-border-left ()
  "Push the border of the current window I am editing to the left."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-right (get-buffer-window))
        (enlarge-window-horizontally *jlib/window-adjustment-amount*)
      (shrink-window-horizontally *jlib/window-adjustment-amount*))))

(defun jlib/push-border-right ()
  "Push the border of the current window I am editing to the right."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-left (get-buffer-window))
        (enlarge-window-horizontally *jlib/window-adjustment-amount*)
      (shrink-window-horizontally *jlib/window-adjustment-amount*))))

(defun jlib/push-border-down ()
  "Push the border of the current window I am editing downwards."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-in-direction 'below)
        (enlarge-window *jlib/window-adjustment-amount*)
      (shrink-window *jlib/window-adjustment-amount*))))

(defun jlib/push-border-up ()
  "Push the border of the current window I am editing upwards."
  (interactive)
  (when (> (length (window-list)) 1)
    (if (window-in-direction 'above)
        (enlarge-window *jlib/window-adjustment-amount*)
      (shrink-window *jlib/window-adjustment-amount*))))

(defun jlib/other-window-backwards ()
  "Go to the previous window backwards."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-c l") #'jlib/push-border-left)
(global-set-key (kbd "C-c h") #'jlib/push-border-right)
(global-set-key (kbd "C-c j") #'jlib/push-border-down)
(global-set-key (kbd "C-c k") #'jlib/push-border-up)
(global-set-key (kbd "C-S-j") #'other-window)
(global-set-key (kbd "C-S-k") #'jlib/other-window-backwards)

;; Theme configuration
(defadvice load-theme (before theme-dont-propagate activate)
  "Prevent themes from interfering with one another."
  (mapc #'disable-theme custom-enabled-themes))

;; Load my favorite default theme, Leuven
(load-theme 'leuven t)

;; Set the default font size.
(defvar *jlib/default-font-size* 200
  "Default font size to revert to on changes.")

;; Set the default font family.

(defvar *jlib/default-font-name* "Hack Nerd Font Mono"
  "Default font to use with Emacs.")

;; Actually set the initial font.
(set-face-attribute
 'default nil
 :font *jlib/default-font-name*
 :height *jlib/default-font-size*)

;; Define some functions that do the right thing (TM) with our fonts.
(defun jlib/reset-font-size ()
  "Reset my Emacs font size."
  (interactive)
  (set-face-attribute
   'default nil :height *jlib/default-font-size*))

(defun jlib/increase-font-size ()
  "Make the current font bigger."
  (interactive)
  (set-face-attribute
   'default nil
   :height (+ (face-attribute 'default :height) 20)))

(defun jlib/decrease-font-size ()
  "Make the current font smaller."
  (interactive)
  (set-face-attribute
   'default nil
   :height (- (face-attribute 'default :height) 20)))

;; And, at last, add keybindings for the fonts.
(global-set-key (kbd "C-0") #'jlib/reset-font-size)
(global-set-key (kbd "C-=") #'jlib/increase-font-size)
(global-set-key (kbd "C--") #'jlib/decrease-font-size)
(global-set-key (kbd "M-q") #'save-buffers-kill-terminal)

;; Now we handle project management.
(defvar *jlib/project-file*
  (jlib/path-join (getenv "HOME") ".current_project")
  "Path to a file that contains the project on which I was last working.")

(defun jlib/get-current-project ()
  "Get the path to the current project on which I am working."
  (let ((project
	 (string-trim
	  (condition-case nil
	      (with-temp-buffer
		(insert-file-contents *jlib/project-file*)
		(buffer-string))
	    (file-error (getenv "HOME"))))))
    (if (file-directory-p project) project (getenv "HOME"))))

(defun jlib/set-current-project ()
  "Save the current location to a convenient spot."
  (interactive)
  (write-region
   (replace-regexp-in-string "~" (getenv "HOME") default-directory)
   nil *jlib/project-file*))

(defun jlib/goto-current-project ()
  "Go to my last working project."
  (interactive)
  (dired (jlib/get-current-project)))

;; Set the initial buffer choice to the current project.
(setq initial-buffer-choice (jlib/get-current-project))

;; Add keybindings for jumping to various projects.
(global-set-key (kbd "C-c d p") #'jlib/goto-current-project)
(global-set-key (kbd "C-c d s") #'jlib/set-current-project)

(defmacro jlib/def-dired (fname keybinding path)
  "Generate a function FNAME bound to KEYBINDING which takes you to PATH."
  `(progn
     (fset ,fname (lambda () (interactive) (dired ,path)))
     (global-set-key (kbd ,keybinding) ,fname)))

(jlib/def-dired
 'jlib/goto-github
 "C-c d g"
 (jlib/path-join (getenv "HOME") "code" "github"))

(jlib/def-dired
 'jlib/goto-emacs
 "C-c d e"
 (jlib/path-join (getenv "HOME") ".config" "emacs"))

(jlib/def-dired
 'jlib/goto-ml
 "C-c d m"
 (jlib/path-join (getenv "HOME") "code" "github" "materiumlabs"))

;; Add a generic programming mode hook
(defun jlib/prog-mode-hook ()
  "My global settings for programming."
  (display-line-numbers-mode)
  (flymake-mode))

(add-hook 'prog-mode-hook #'jlib/prog-mode-hook)

;;; Custom Formatting
(load (jlib/path-join user-emacs-directory "efmt.el"))

;; LISP formatting
(defun jlib/indent-lisp ()
  "Indent the buffer in a LISP-y way for me."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Buffer indented successfully."))

(setq
 *efmt-format-alist*
 `((json-mode ("prettier" "-w" "<TARGET>"))
   (js-mode ("prettier" "-w" "<TARGET>"))
   (c-mode ("clang-format" "-i" "<TARGET>"))
   (c++-mode ("clang-format" "-i" "<TARGET>"))
   (haskell-mode ("ormolu" "-i" "<TARGET>"))
   ("el" ,#'jlib/indent-lisp)))

(global-set-key (kbd "C-c p") #'efmt)


;; PACKAGES
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(use-package esup :ensure t :demand t)

;; General nice changes
(use-package vertico :ensure t :demand t :init (vertico-mode))
(use-package ctrlf :ensure t :demand t :init (ctrlf-mode))
(use-package rg :ensure t :demand t :init (global-set-key (kbd "C-M-s") #'rg))
(use-package corfu :ensure t :demand t :init (global-corfu-mode))

;; Haskell
(use-package haskell-mode :ensure t :demand t)

(add-hook
 'haskell-mode-hook
 (lambda ()
   (eglot-ensure)))

;; Now that init has finished, we have to reset GC and the `file-name-handler-alist'
;; to something sane.
(setq gc-cons-threshold (* 1024 1024 100))

;;; Commentary:
;; This is my Emacs config.  There are many others like it, but this one is mine.

(provide 'init)
;;; init.el ends here
