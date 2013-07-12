;;; quake-mode.el --- 
;; 
;; Filename: quake-mode.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: Fri Jul 12 13:01:38 2013 (-0400)
;; Version: 
;; Last-Updated: Fri Jul 12 13:01:47 2013 (-0400)
;;           By: jorbi
;;     Update #: 1
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;;---------------------------------------------------------------------------
;; Quake-mode
;;---------------------------------------------------------------------------

;;;###autoload
(define-minor-mode quake-mode
  "Minor mode for racking up massive killing sprees"
  :init-value nil
  :lighter " quake"
  :global t
  ;; body
  (quake/init-default-frags)
  (if quake-mode
      (quake/enable-fragging)
    (quake/disable-fragging)))

(defun quake/init-default-frags()
  (quake/define-frag kill-word)
  (quake/define-frag kill-comment)
  (quake/define-frag kill-line)
  (quake/define-frag kill-whole-line)
  (quake/define-frag kill-rectangle)
  (quake/define-frag kill-region)
  (quake/define-frag kill-buffer))

(defvar quake/killing-spree 0
  "The current killing spree kill count.")

(defvar quake/last-kill-time (current-time)
  "The time `kill-line' was last used.")

(defvar quake/killing-spree-interval 1 
  "The number of seconds that may pass between kills part of the same spree.")

(defvar quake/last-holy-shit-time (current-time)
  "The time `quake/HOLY-SHIT' was last used.")

(defvar quake/holy-shit-interval 10
  "The number of seconds before another holy shit can occur")

(defcustom quake/fullscreen-holy-shit t
  "If non-nil, use the special fullscreen holy shit message.")
(defmacro quake/announce(msg)
  `(message "%s" (propertize ,msg 'face 
			     '(:foreground
			       "red"
			       :weight
			       'bold
			       :height 
			       ,(* 2 (face-attribute 'default :height))))))

(defun quake/try-print-spree-message()
  (if (> quake/killing-spree 2)
      (cond
       ((= quake/killing-spree 3) (quake/announce "TRIPLE KILL!"))
       ((= quake/killing-spree 5) (quake/announce "MULTI KILL!"))
       ((= quake/killing-spree 8) (quake/announce "MEGA KILL!"))
       ((= quake/killing-spree 11) (quake/announce "ULTRA KILL!"))
       ((= quake/killing-spree 15) (quake/announce "MONSTER KILL!"))
       ((= quake/killing-spree 20) (quake/announce "LUDACRIS KILL!"))
       ((= quake/killing-spree 25) (quake/announce "GODLIKE!"))
       ((= quake/killing-spree 35) (quake/HOLY-SHIT)))))

(defun quake/HOLY-SHIT()
  "HOLY SHIT!
ripped from yell/ fix me."
  (if (or (not quake/fullscreen-holy-shit)
	  (<= (time-to-seconds (time-since quake/last-holy-shit-time)) 
	      quake/holy-shit-interval))
      (quake/announce "HOOOOOOOLY SHIIIT!")
    (progn (lexical-let
	       ((old-config (current-window-configuration))
		(old-bg (face-background 'default))
		(background (face-background 'default))
		(yell-buffer (generate-new-buffer (generate-new-buffer-name ":yell"))))
	     (delete-other-windows)
	     (switch-to-buffer yell-buffer)
	     (set-face-background 'default background)
	     (insert (propertize 
		      (concat "\nHOOOOOOLY\nSHIIIIT\n")
		      'face `(:foreground "red"
					  :height ,(* 20 (face-attribute 'default :height)))))
	     
	     (run-with-timer 1 nil
			     (lambda()
			       (set-face-background 'default old-bg)
			       (set-window-configuration old-config)
			       (kill-buffer yell-buffer))))))
  (setq quake/last-holy-shit-time (current-time)))

(defun quake/kill-tic()
  "Increments your killing spree count or resets it if it has been longer than `quake/killing-spree-interval' seconds 
since your last frag."
  (if quake-mode
      (progn (if (zerop quake/killing-spree)
		 (setq quake/last-kill-time (current-time)
		       quake/killing-spree (1+ quake/killing-spree))
	       (if (<= (time-to-seconds (time-since quake/last-kill-time)) quake/killing-spree-interval)
		   (progn (quake/try-print-spree-message)
			  (setq quake/last-kill-time (current-time)
				quake/killing-spree (1+ quake/killing-spree)))
		 (setq quake/last-kill-time (current-time)
		       quake/killing-spree 0)))
	     quake/killing-spree)))


(defmacro quake/define-frag(func-name)
  "Marks an unquoted function, FUNC-NAME, for counting as a frag (kill) towards your killing spree."
  (assert (fboundp func-name) nil 
	  (format "Error adding %s as a quake kill function: function not found" func-name))
  (let ((new-ad-name (concat "quake/kill-ad-for-" (symbol-name func-name))))
    `(progn (defadvice ,func-name (before ,(intern new-ad-name))
	      (if (called-interactively-p 'interactive) (quake/kill-tic))))))

(defun quake/disable-fragging()
  "Disables the tracking of your killing sprees.
Fragging can be re-enabled using `quake/enable-fragging'."
  (ad-disable-regexp "\\<quake/kill-ad-for-.*")
  (ad-update-regexp "\\<quake/kill-ad-for-.*"))

(defun quake/enable-fragging()
  "Enables the tracking of your killing sprees if they have been disabled by `quake/disable-fragging'."
  (ad-enable-regexp "\\<quake/kill-ad-for-.*")
  (ad-activate-regexp "\\<quake/kill-ad-for-.*")
  (ad-update-regexp "\\<quake/kill-ad-for-.*"))


(provide 'quake-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quake-mode.el ends here


