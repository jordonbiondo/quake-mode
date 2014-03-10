;;; quake-mode.el --- Quake/Unreal style (text) killings sprees
;; 
;; Filename: quake-mode.el
;; Description: Quake/Unreal style (text) killings sprees
;; Author: Jordon Biondo
;; Maintainer: Jordon Biondo <biondoj@mail.gvsu.edu>
;; Created: Fri Jul 12 13:01:38 2013 (-0400)
;; Version: .1
;; Last-Updated: Wed Sep 11 22:03:12 2013 (-0400)
;;           By: Jordon Biondo
;;     Update #: 6
;; URL: github.com/jordonbiondo/quake-mode
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
  (quake/init-default-events)
  (quake/choose-play-sound-async-function)
  (if quake-mode
      (progn (quake/enable-fragging)
	     (quake/play-sound-async "prepare3.wav")
	     (add-hook 'compilation-finish-functions 'quake/compilation-result))
    (progn (quake/disable-fragging)
	   (quake/play-sound-async "flawless.wav")
	   (remove-hook 'compilation-finish-functions 'quake/compilation-result))))

;;---------------------------------------------------------------------------
;; Define Frags
;;---------------------------------------------------------------------------  

(defun quake/init-default-frags()
  (quake/deffrag kill-word)
  (quake/deffrag backward-kill-word)
  (quake/deffrag kill-comment)
  (quake/deffrag kill-line)
  (quake/deffrag kill-visual-line)
  (quake/deffrag kill-whole-line)
  (quake/deffrag kill-rectangle)
  (quake/deffrag kill-region)
  (quake/deffrag kill-buffer))

(defmacro quake/deffrag(func-name)
  "Marks an unquoted function, FUNC-NAME, for counting as a frag (kill) towards your killing spree."
  (assert (fboundp func-name) nil 
	  (format "Error adding %s as a quake kill function: function not found" func-name))
  (let ((new-ad-name (concat "quake/kill-ad-for-" (symbol-name func-name))))
    `(progn (defadvice ,func-name (before ,(intern new-ad-name))
	      (if (called-interactively-p 'interactive) (quake/kill-tic))))))

(defmacro quake/undeffrag(func-name)
  "The function, FUNC-NAME will no longer count as a frag."
  (let ((curr-ad-name (concat "quake/kill-ad-for-" (symbol-name func-name))))
    `(progn
       (ad-remove-advice ,(quote func-name) (quote before) ,(quote (intern curr-ad-name)))
       (ad-update ,(quote func-name)))))

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


;;---------------------------------------------------------------------------
;; Humiliating compilaiton failure
;;---------------------------------------------------------------------------  
(defun quake/compilation-result(buf msg)
  (if (string-match "^finished" msg)
      (quake/play-sound-async "flawless.wav")
    (let ((humiliation-sounds (list "knife.wav" "knife2.wav" "knife3.wav" "suicide2.wav")))
      (quake/play-sound-async (nth (random (length humiliation-sounds)) humiliation-sounds))))
  nil)


;;---------------------------------------------------------------------------
;; Variables
;;---------------------------------------------------------------------------  
(defvar quake/killing-spree 0
  "The current killing spree kill count.")

(defvar quake/last-kill-time (current-time)
  "The time `kill-line' was last used.")

(defvar quake/killing-spree-interval 1 
  "The number of seconds that may pass between kills part of the same spree.")

(defvar quake/display-announcer-text nil)

(defvar quake/play-announcer-sounds t)

(defvar quake/last-holy-shit-time (current-time)
  "The time `quake/HOLY-SHIT' was last used.")

(defvar quake/holy-shit-interval 10
  "The number of seconds before another holy shit can occur")

(defcustom quake/fullscreen-holy-shit t
  "If non-nil, use the special fullscreen holy shit message.")

(defvar quake/play-sound-async-function nil)

(defvar quake/spree-event-map (make-hash-table :test 'equal)
  "Map holding event information, keys are ints.")

(defvar quake/sound-directory nil)
(when load-file-name
  (setq quake/sound-directory (format "%sQuakeSounds/" (file-name-directory load-file-name))))

;;---------------------------------------------------------------------------
;; Functions
;;---------------------------------------------------------------------------  

;;---------------------------------------------------------------------------
;; Killing Spree events
;;---------------------------------------------------------------------------  

(defun quake/init-default-events()
  (clrhash quake/spree-event-map)
  (quake/define-spree-event 3  "TRIPLE KILL" "triplekill.wav")
  (quake/define-spree-event 5 "MULTI KILL" "multikill.wav")
  (quake/define-spree-event 8  "MEGA KILL" "megakill.wav")
  (quake/define-spree-event 11 "ULTRA KILL" "ultrakill.wav")
  (quake/define-spree-event 15 "MONSTER KILL" "monsterkill.wav")
  (quake/define-spree-event 20 "LUDICROUS KILL" "ludicrouskill.wav")
  (quake/define-spree-event 25 "GODLIKE" "godlike.wav")
  (quake/define-spree-event 35 "HOLY SHIT" "holyshit.wav"))

(defmacro quake/define-spree-event(kill-count text sound)
  "Define a new killing spree event.

When `quake/killing-spree' reaches the integer KILL-COUNT, the string TEXT  will be
displayed and the sound file SOUND will by played.

If there is already an event bound to this KILL-COUNT, it will be overwritten."
  (assert (and (integerp kill-count) (stringp text) (stringp sound)))
  `(progn (puthash ,kill-count (list ,text ,sound) ,(quote quake/spree-event-map))))

(defun quake/eventp(event)
  "Returns true if EVENT is a quake/event.
an event: '(text filename)"
  (and event (listp event) (= (length event) 2)))

(defun quake/assert-eventp(event)
  "Signal an error if EVENT does not pass `quake/eventp'."
  (if (not (quake/eventp event)) 
      (signal 'wrong-type-argument (format "%s %s" 'quake/eventp event))))
								      
(defun quake/event-text(event)
  "Returns the announcement text from an event object."
  (quake/assert-eventp event)
  (first event))

(defun quake/event-sound-file(event)
  "Returns the sound file from an event object."
  (quake/assert-eventp event)
  (second event))
		 
(defun quake/try-execute-event()
  (let ((event (gethash quake/killing-spree quake/spree-event-map)))
    (when (quake/eventp event)
      (when quake/play-announcer-sounds
	(quake/play-sound-async (quake/event-sound-file event)))
      (if quake/display-announcer-text
	  (quake/announce (quake/event-text event))))))

(defmacro quake/announce(msg)
  `(message "%s" (propertize 
		  ,msg 'face '(:foreground "red"
					   :height ,(* 2 (face-attribute 'default :height))))))

;; (defun quake/try-print-spree-message()
;;   (if (> quake/killing-spree 2)
;;       (cond
;;        ((= quake/killing-spree 3) (quake/announce "TRIPLE KILL!"))
;;        ((= quake/killing-spree 5) (quake/announce "MULTI KILL!"))
;;        ((= quake/killing-spree 8) (quake/announce "MEGA KILL!"))
;;        ((= quake/killing-spree 11) (quake/announce "ULTRA KILL!"))
;;        ((= quake/killing-spree 15) (quake/announce "MONSTER KILL!"))
;;        ((= quake/killing-spree 20) (quake/announce "LUDACRIS KILL!"))
;;        ((= quake/killing-spree 25) (quake/announce "GODLIKE!"))
;;        ((= quake/killing-spree 35) (quake/HOLY-SHIT)))))


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
		      (concat "\n HOLY\n SHIT\n")
		      'face `(:foreground "red"
					  :height ,(* 20 (face-attribute 'default :height)))))
	     (quake/play-sound-async "holyshit.wav")
	     (read-only-mode t)
	     (run-with-timer 1 nil
			     (lambda()
			       (redisplay)
			       (set-face-background 'default old-bg)
			       (set-window-configuration old-config)
			       (kill-buffer yell-buffer))))))
  (setq quake/last-holy-shit-time (current-time)))

(defun quake/kill-tic()
  "Increments your killing spree count or resets it if it has been longer than `quake/killing-spree-interval' seconds 
since your last frag."
  (if quake-mode 
      (progn (if (<= (time-to-seconds (time-since quake/last-kill-time)) quake/killing-spree-interval)
		 (progn (setq quake/last-kill-time (current-time)
			      quake/killing-spree (1+ quake/killing-spree))
			(quake/try-execute-event))
	       (setq quake/last-kill-time (current-time)
		     quake/killing-spree 1)))
    quake/killing-spree))

;;---------------------------------------------------------------------------
;; Sounds support
;;---------------------------------------------------------------------------  
(defun quake/can-play-sound-async()
  "Returns true if the system can play sound asynchronously."
  (if (display-graphic-p) ;; must be a window system
      (cond
       ((equal system-type 'windows-nt) ;; windows
	(with-temp-buffer
	  ;;(clog/todo "Let's just do this one time in the future")
	  (shell-command "ver" (current-buffer))
	  (princ "")
	  (goto-char (point-min))
	  (if (search-forward-regexp "Version 6\\.[0-9]\\.[0-9]+" nil t) t))) 
       
       ((equal system-type 'darwin) t) ;; mac
       
       ((or (equal system-type 'gnu/linux)
	    (equal system-type 'gnu)
	    (equal system-type 'gnu/kfreebsd)) nil) ;; linx etc
       
       (t nil)) ;; other
    ))

(defun quake/play-sound-async (sound-file)
  "Attempts to play SOUND-FILE."
  (if (and t ;;(quake/can-play-sound-async) Puts a message in the minibuf, annoying
	   quake/play-sound-async-function)
      (apply quake/play-sound-async-function (list sound-file))))

(defun quake/windows-play-sound-async(sound-file)
  (lexical-let ((proc 
		 (start-process "quake/sound" nil "powershell" "-Command"
				(format "%s%s%s"
					(format "(New-Object Media.SoundPlayer \"%s"
						quake/sound-directory)
					filename
					"\").PlaySync();exit"))))
    ;; powershell doesn't signal??????
    (run-with-timer 8 nil (lambda()
			    (process-send-eof proc)))))

(defun quake/osx-play-sound-async(sound-file) 
  (start-process "quake/sound" nil "afplay" (format "%s%s" quake/sound-directory sound-file)))

(defun quake/choose-play-sound-async-function()
  "Sets the value of `quake/play-sound-async-function' to a value appropriate for the
`system-type'."
  (setq quake/play-sound-async-function 
	(cond
	 ((not (quake/can-play-sound-async)) nil)
	 ((equal system-type 'windows-nt) 'quake/windows-play-sound-async)
	 ((equal system-type 'darwin) 'quake/osx-play-sound-async) ;; mac
	 ((or (equal system-type 'gnu/linux)
	      (equal system-type 'gnu)
	      (equal system-type 'gnu/kfreebsd)) nil) ;; linx etc
	 (t nil))))


(provide 'quake-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quake-mode.el ends here


