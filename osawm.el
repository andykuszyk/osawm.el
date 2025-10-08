;;; osawm.el --- a Mac OS window manager for Emacs  -*- lexical-binding:t; -*-
;;; Commentary:
;;; Code:

(require 'f)

(defgroup osawm
  nil
  "A Mac OS window manager for Emacs."
  :group 'applications
  :prefix "osawm-")

;; TODO: try to determine this programmatically
(defcustom osawm-titlebar-height
  68
  "The height of the title bar, which is taken into account when resizing windows."
  :type 'number)

;; TODO: try to determine this programmatically
(defcustom osawm-modeline-height
  20
  "The height of the modeline, which is taken into account when resizing windows."
  :type 'number)

(defvar osawm--focus-changing nil "Flags when the frame focus is changing.")

(defvar-local osawm--window-name nil
  "The buffer-local name of window represented by the buffer.
This window name combined with 'osawm--app-name' uniquely identifies
the window associated with the buffer.")

(defvar-local osawm--app-name nil
  "The buffer-local name of app represented by the buffer.
This app name combined with 'osawm--window-name' uniquely identifies
the window associated with the buffer.")

(defun osawm--get-window-bounds ()
  "Get the bounds of the current window.
Returns a plist with the following keys: :left, :right, :top, :bottom."
  (let* ((left (window-pixel-left))
	 (right (+ (window-pixel-left) (window-pixel-width)))
	 (top (+  (window-pixel-top) osawm-titlebar-height))
	 (bottom (- (+ (window-pixel-top)
		       (window-pixel-height)
		       osawm-titlebar-height)
		    osawm-modeline-height)))
    (list :left left :right right :top top :bottom bottom)))

(defun osawm-search-chrome (query)
  "Open a new Chrome window with the search QUERY."
  (interactive "MSearch: ")
  (osawm-launch-chrome
   (format "https://www.google.com/search?q=%s" query)
   query
   "normal"))

(defun osawm-launch-chrome (url name mode)
  "Open a new Chrome window named NAME at URL in MODE.
MODE should be either 'normal' or 'incognito'"
  (interactive "MURL: \nMName: \nMMode: ") ; TODO bind args
  (assert (or (string-equal mode "normal") (string-equal mode "incognito")))
  (shell-command (format "
osascript <<EOF
tell application \"Google Chrome\"
    activate
    set newWindow to make new window with properties {mode: \"%s\"}
    set URL of active tab of newWindow to \"%s\"
    set given name of newWindow to \"%s\"
end tell
EOF" mode url name))
  (let* ((window-bounds (osawm--get-window-bounds)))
    (osawm--resize-chrome-window name window-bounds)
    (switch-to-buffer
     (osawm--make-buffer
      name
      (osawm--take-screenshot name window-bounds)))))

(defun osawm--make-buffer (name filepath)
  "Make a new OSAWM buffer called NAME with the image at FILEPATH.
Returns the newly created buffer."
  (let* ((buffer-name (format "*osawm %s*" name))
	 (osawm-buffer (get-buffer-create buffer-name)))
    (with-current-buffer osawm-buffer
      (insert-file-contents filepath)
      (osawm-mode)
      (setq osawm--window-name name))
    osawm-buffer))

(defun osawm--take-screenshot (name window-bounds)
  "Take a screenshot for NAME using the WINDOW-BOUNDS.
Returns the filename of the captured screenshot as an absolute file path."
  (let* ((temp-file-name
	  (f-join (temporary-file-directory) (format "%s.png" name)))
	 (left (plist-get window-bounds :left))
	 (top (plist-get window-bounds :top)))
    (shell-command
     (format
      "screencapture -x -R %d,%d,%d,%d \"%s\""
      left
      top
      (- (plist-get window-bounds :right) left)
      (- (plist-get window-bounds :bottom) top)
      temp-file-name))
    temp-file-name))

(defun osawm--resize-chrome-window (name window-bounds)
  "Resize the Chrome window with the given name NAME to WINDOW-BOUNDS."
  (shell-command
   (format
    "osascript <<EOF
tell application \"Google Chrome\"
    activate
    repeat with w in every window
        if given name of w is equal to \"%s\" then
            set bounds of w to {%d, %d, %d, %d} -- {left, top, right, bottom}
        else
            set bounds of w to {0, 0, 10, 10}
        end if
    end repeat
end tell
EOF"
    name
    (plist-get window-bounds :left)
    (plist-get window-bounds :top)
    (plist-get window-bounds :right)
    (plist-get window-bounds :bottom))))

(defun osawm--assert-osawm-buffer ()
  (if (and (eq major-mode 'osawm-mode)
	   (boundp 'osawm--window-name)
           osawm--window-name)
      t
    (error "not a valid osawm buffer")))

(defun osawm-activate ()
  "Activates the window associated with the current OSAWM buffer."
  (interactive)
  (osawm--assert-osawm-buffer)
  (let* ((window-bounds (osawm--get-window-bounds))
	 (inhibit-read-only t)
	 (name osawm--window-name))
    (osawm--resize-chrome-window osawm--window-name window-bounds)
    (insert-file-contents
     (osawm--take-screenshot osawm--window-name window-bounds)
     nil nil nil t)
    (osawm-mode)
    (setq osawm--window-name name)))

(defun osawm--ensure-single-window ()
  "Ensure osawm buffer is only displayed in one window."
  (let ((buffer (current-buffer)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (unless (eq window (selected-window))
        (set-window-buffer window (other-buffer buffer))))))

(defvar osawm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'osawm-activate)
    map)
  "Keymap for osawm-major-mode.")

(define-derived-mode osawm-mode image-mode "OSAWM"
  "Major mode for OSAWM window buffers."
  :keymap osawm-mode-map
  (display-line-numbers-mode -1)
  (add-hook
   'window-configuration-change-hook
   #'osawm--window-configuration-changed nil t)
  (osawm--ensure-single-window))

(defun osawm--window-configuration-changed ()
  "Update the display of apps upon a window configuration change."
  (unless (active-minibuffer-window)
    (when (eq major-mode 'osawm-mode)
      (osawm--ensure-single-window)
      (osawm--after-focus-change))))

(defun osawm--after-focus-change ()
  "Function to handle focus change when global-osawm-mode is active."
  (when (frame-focus-state)
    (if osawm--focus-changing
	(setq osawm--focus-changing nil)
      (when (and (eq major-mode 'osawm-mode) osawm--window-name)
	(setq osawm--focus-changing t)
	(osawm-activate)
	(shell-command (format "
osascript <<EOF
tell application \"System Events\"
    set targetProcess to first process whose unix id is %d
    set frontmost of targetProcess to true
end tell
EOF" (emacs-pid)))))))

(define-minor-mode global-osawm-mode
  "Global minor mode for OSAWM window management.
When enabled, automatically updates OSAWM buffers when they gain focus."
  :global t
  :lighter " OSAWM"
  (if global-osawm-mode
      (add-function :after after-focus-change-function #'osawm--after-focus-change)
    (remove-function after-focus-change-function #'osawm--after-focus-change)))

(provide 'osawm)
;;; osawm.el ends here
