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

;; Make sure that typing `:' in C++ mode doesn't backward indent.
(add-hook 'c++-mode-hook (lambda () (define-key c++-mode-map ":" nil)))	  

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

;; Now that init has finished, we have to reset GC and the `file-name-handler-alist'
;; to something sane.
(setq
 gc-cons-threshold (* 1024 1024 100))
