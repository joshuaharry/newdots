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

;; Put Emacs in fullscreen mode by default.
(toggle-frame-fullscreen)

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

;; Set the default font size.
(defvar *jlib/default-font-size* 200
  "Default font size to revert to on changes.")
					; Set the default font family.
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

;; Bootstrap elpaca, our package manager.
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil :depth 1
			      :files (:defaults "elpaca-test.el" (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
				       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; DO NOT DELETE THIS LINE OF CODE! If you do, *none* of the packages we load
;; below will actually work.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :ensure t :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))

;;; GENERIC MODES
;; Use esup for Emacs profiling.
(use-package esup :ensure t :demand t)
;; Add a nice selection of colorschemes.
(use-package doom-themes :ensure t :demand t)
;; Add a nice modeline.
(use-package telephone-line :ensure t :demand t)
;; Add nice selection capabilities.
(use-package vertico :ensure t :demand t)
;; Make searching significantly nicer.
(use-package ctrlf :ensure t :demand t)
;; Use a custom terminal emulator
(use-package vterm :ensure t :demand t)
;; Use the custom formatter that I wrote.
(use-package efmt
  :demand t
  :ensure (:host github :repo "joshuaharry/efmt" :main "efmt.el"))
;; Use the shell to set up our exec path.
(use-package exec-path-from-shell :ensure t :demand t)
;; Use Magit for an awesome git experience.
;; For mysterious reasons, we have to load `transient' before we load magit in
;; order to get the package to actually work.
(use-package transient :ensure (:fetcher github :repo "magit/transient"))
(use-package magit :ensure t)
;; Use Flycheck for linting.
(use-package flycheck :ensure t :demand t)
;; Use Yasnippet for snippets.
(use-package yasnippet :ensure t :demand t)
;; Use direnv for direnv support.
(use-package direnv :ensure t :demand t)

;;; LANGUAGE MODES
;; Markdown support.
(use-package markdown-mode :ensure t :demand t)
;; OCaml support.
(use-package tuareg :ensure t :demand t)
(use-package dune :ensure t :demand t)
(use-package merlin :ensure t :demand t)
;; Use Web mode for HTML/CSS/JavaScript
(use-package web-mode :ensure t :demand t)

(elpaca-wait)

;; Formatting
(defun jlib/indent-lisp ()
  "Indent the buffer in a LISP-y way for me."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Buffer indented successfully."))

(setq *efmt-format-alist*
      `(("el" ,#'jlib/indent-lisp)
	("ml" ("ocamlformat" "--enable-outside-detected-project" "<TARGET>"))
	("html" ("prettier" "-w" "<TARGET>"))
	("css" ("prettier" "-w" "<TARGET>"))
	("js" ("prettier" "-w" "<TARGET>"))
	("md" ("prettier" "-w" "<TARGET>"))
	("go" ("gofmt" "-w" "<TARGET>"))))

;; Do not use any shell arguments with `exec-path-from-shell'.
(setq exec-path-from-shell-arguments nil)

;; Work around a bug where esup tries to step into the byte-compiled
;; version of `cl-lib', and fails horribly.
(setq esup-depth 0)

;; Enable snippets everywhere.
(yas-global-mode)
;; Enable formatting everywhere.
(global-set-key (kbd "C-c p") #'efmt)
;; Enable the modeline.
(telephone-line-mode 1)
;; Enable nice selections.
(vertico-mode)
;; Enable better searching.
(ctrlf-mode +1)
;; Toggle a nice theme.
(load-theme 'doom-one-light t)
;; Load the $PATH from the shell.
(exec-path-from-shell-initialize)
;; Enable direnv.
(direnv-mode)

;; Terminals in Emacs
(defvar *jlib/term-fn* #'vterm
  "The function I use for creating a terminal.")

(defun jlib/summon-terminal ()
  "Summon a terminal."
  (interactive)
  (funcall *jlib/term-fn*))

(defun jlib/summon-terminal-right ()
  "Summon a terminal to the right of where I am."
  (interactive)
  (split-window-right)
  (other-window 1)
  (funcall *jlib/term-fn*))

(defun jlib/summon-terminal-left ()
  "Summon a terminal to the left of where I am."
  (interactive)
  (split-window-right)
  (funcall *jlib/term-fn*)
  (other-window 1))

(defun jlib/summon-terminal-below ()
  "Summon a terminal below where I am."
  (interactive)
  (split-window-vertically)
  (funcall *jlib/term-fn*))

(defun jlib/summon-terminal-above ()
  "Summon a terminal above where I am."
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (funcall *jlib/term-fn*))

(global-set-key (kbd "C-c t t") #'jlib/summon-terminal)
(global-set-key (kbd "C-c t h") #'jlib/summon-terminal-left)
(global-set-key (kbd "C-c t j") #'jlib/summon-terminal-below)
(global-set-key (kbd "C-c t k") #'jlib/summon-terminal-above)
(global-set-key (kbd "C-c t l") #'jlib/summon-terminal-right)

;; Global Programming settings
(defun jlib/prog-mode-hook ()
  "My global settings for programming."
  (display-line-numbers-mode)
  (flycheck-mode))

(add-hook 'prog-mode-hook #'jlib/prog-mode-hook)

;; Ocmal programming settings
(defun jlib/tuareg-mode-hook ()
  "My settings for hacking OCaml code."
  (merlin-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(setq web-mode-enable-current-element-highlight t)

(defun jlib/web-mode-hook ()
  "Hook for entering and editing web mode files."
  ;; Treat ' as a string. I spent *years* trying to figure out how to make
  ;; web-mode do this correctly, and I only found the solution after I tried
  ;; to write a major mode of my own. You live and you learn :)
  (modify-syntax-entry ?' "\"" web-mode-syntax-table)
  (modify-syntax-entry ?` "\"" web-mode-syntax-table)
  (setq
   web-mode-auto-close-style 2
   web-mode-markup-indent-offset 2
   web-mode-enable-auto-quoting nil
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2))

(add-hook 'tuareg-mode-hook #'jlib/tuareg-mode-hook)

;; Now that init has finished, we have to reset GC and the `file-name-handler-alist'
;; to something sane.
(setq
 file-name-handler-alist '(("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
			   ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
			   ("\\`/:" . file-name-non-special))
 gc-cons-threshold (* 1024 1024 100))
