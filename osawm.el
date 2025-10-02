;;; osawm.el --- a Mac OS window manager for Emacs  -*- lexical-binding:t; -*-
;;; Commentary:
;;; Code:

(require 'f)

(defgroup osawm
  nil
  "A Mac OS window manager for Emacs."
  :group 'applications
  :prefix "osawm-")

(defcustom osawm-titlebar-height
  68
  "The height of the title bar, which is taken into account when resizing windows."
  :type 'number)

(defcustom osawm-modeline-height
  20
  "The height of the modeline, which is taken into account when resizing windows."
  :type 'number)

(defvar-local osawm--buffer-app nil
  "The buffer-local name of the app represented by the buffer.")

(defun osawm-bind-app (app)
  "Binds APP to a new osawm buffer.
Binds an already open app on your system to a new osawm buffer, so that
the apps location and size can be controlled by manipulating the buffer's
window."
  (interactive
   (list (completing-read "Application: " (osawm--list-applications) nil t)))
  (let* ((buffer-name (format "*osawm %s*" app)))
    (find-file (osawm--refresh-screenshot app))
    (setq osawm--buffer-app app)
    (osawm-major-mode)
    (rename-buffer buffer-name)))

(defun osawm-update-buffer ()
  "Update the current buffer.
Refreshes the image of the current buffer's application by resizing
the application, and taking a new screenshot."
  (interactive)
  (when (and (eq major-mode 'osawm-major-mode)
             (boundp 'osawm--buffer-app)
             osawm--buffer-app)
  (insert-file-contents (osawm--refresh-screenshot osawm--buffer-app))
  (revert-buffer)))

(defun osawm--refresh-screenshot (app &optional focus-frame)
  "Update the size of APP, and take a new screenshot.
If FOCUS-FRAME is non-nil, focus the Emacs frame after capturing."
  (let* ((left (window-pixel-left))
	 (right (+ (window-pixel-left) (window-pixel-width)))
	 (top (+  (window-pixel-top) osawm-titlebar-height))
	 (bottom (- (+ (window-pixel-top)
		       (window-pixel-height)
		       osawm-titlebar-height)
		    osawm-modeline-height))
	 (temp-file-name
	  (f-join (temporary-file-directory) (format "%s.png" app))))
    (osawm--resize-application app left top right bottom)
    (shell-command
     (format
      "screencapture -R %d,%d,%d,%d \"%s\""
      left
      top
      (- right left)
      (- bottom top)
      temp-file-name))
    (when focus-frame
      (select-frame-set-input-focus (selected-frame)))
    temp-file-name))

(defun osawm--ensure-single-window ()
  "Ensure osawm buffer is only displayed in one window."
  (let ((buffer (current-buffer)))
    (dolist (window (get-buffer-window-list buffer nil t))
      (unless (eq window (selected-window))
        (set-window-buffer window (other-buffer buffer))))))

(defun osawm--enforce-single-window ()
  "Enforce single window constraint for osawm buffers."
  (when (eq major-mode 'osawm-major-mode)
    (osawm--ensure-single-window)))

(defun osawm-activate ()
  "Activates the app associated with the current osawm buffer."
  (interactive)
  (insert-file-contents (osawm--refresh-screenshot osawm--buffer-app nil))
  (revert-buffer))

(defvar osawm-major-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'osawm-activate)
    (define-key map (kbd "C-c RET") 'osawm-update-buffer)
    map)
  "Keymap for osawm-major-mode.")

(define-derived-mode osawm-major-mode image-mode "OSAWM"
  "Major mode for OSAWM application buffers."
  :keymap osawm-major-mode-map
  (display-line-numbers-mode -1)
  (add-hook 'window-configuration-change-hook #'osawm--enforce-single-window nil t)
  (osawm--ensure-single-window))

(defun osawm--resize-application (application left top right bottom)
  "Resize APPLICATION to LEFT, RIGHT, BOTTOM, TOP."
  (message (format  "resizing application  %s" application))
  (shell-command
   (format
    "
osascript <<EOF
tell application \"%s\"
	activate
	tell window 1
		set bounds to {%d, %d, %d, %d} -- {left, top, right, bottom}
	end tell
end tell
EOF"
    application left top right bottom)))

(defun osawm--list-applications ()
  "Lists open applications."
  (interactive)
  (mapcar #'string-trim
          (string-split (shell-command-to-string "
osascript <<EOF
tell application \"System Events\"
    -- Get the names of all open (non-background) applications
    set openApps to name of every process whose background only is false
end tell

-- Join the list of application names into a single string with newlines
set AppleScript's text item delimiters to \",\"
set openAppsList to openApps as string

-- Output the list to the terminal
do shell script \"echo \" & quoted form of openAppsList
EOF
") ",")))

(provide 'osawm)
;;; osawm.el ends here
