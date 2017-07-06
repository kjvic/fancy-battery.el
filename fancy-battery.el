;;; fancy-battery.el --- Fancy battery display       -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/lunaryorn/fancy-battery.el
;; Keywords: convenience tools hardware
;; Package-Version: 0.3-cvs
;; Package-Requires: ((emacs "24.1"))

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide `krista-fancy-battery-mode', which is like `display-battery-mode' but with
;; fancier display, and more customization options.

;; Customize `fancy-battery-mode-line' to change the appearance of the battery
;; status information.  Take a look at `fancy-battery-default-mode-line' for the
;; default value and for inspiration.

;; Customize `mode-line-format' and `mode-line-misc-info' to change the position
;; at which the battery status appears in the mode line.  Typically it's at the
;; very end after the minor mode list, so you may want to move
;; `mode-line-misc-info` more to the front of `mode-line-format`.

;; TODO:
;; - Create `defcustom' entries for each of the %-sequences that a user could want
;; - Modify `fancy-battery-default-mode-line' to read user preferences
;; - Either rm, or beef up the code for `fancy-battery-status-update-functions'
;;   - Probably should modify the logic in
;;     `fancy-battery-default-mode-line' to this hook

;;; Code:

(require 'battery)

;;
;; Variables for users to configure
;; --------------------------------
;;
(defcustom fancy-battery-show-percentage nil
  "When non-nil show battery load percentage in mode line.

Otherwise, show the remaining time to charge or discharge if
available.

Has no effect if `fancy-battery-mode-line' does not evaluate
`fancy-battery-default-mode-line'.")


;; Set to t if you want to see these
;;
;; Example: this next line will display the time until charge/discharge
(setq fancy-battery--show-time-mixed-format t)

(setq fancy-battery--show-percentage t)
(setq fancy-battery--show-status-verbose t)
(setq fancy-battery--show-power-source t)

(defcustom fancy-battery--show-current-capacity nil "asdf")
(defcustom fancy-battery--show-rate-of-charge-or-discharge nil "asdf")
(defcustom fancy-battery--show-status-verbose nil "asdf")
(defcustom fancy-battery--show-status nil "asdf")
(defcustom fancy-battery--show-temp nil "asdf")
(defcustom fancy-battery--show-power-source nil "asdf")
(defcustom fancy-battery--show-percentage nil "asdf")
(defcustom fancy-battery--show-time-in-minutes nil "asdf")
(defcustom fancy-battery--show-time-in-hours nil "asdf")
(defcustom fancy-battery--show-time-mixed-format nil "asdf")
(defcustom fancy-battery--show-linux-driver-version nil "asdf")
(defcustom fancy-battery--show-apm-bios-version nil "asdf")
(defcustom fancy-battery--show-apm-bios-status-verbose nil "asdf")
(defcustom fancy-battery--show-bsd-power-saving-mode-state nil "asdf")

;;
;; Faces
;; -----
;;
(defface fancy-battery-critical '((t :inherit error))
  "Face for critical battery status"
  :group 'fancy-battery)

(defface fancy-battery-charging '((t :inherit success))
  "Face for charging battery status."
  :group 'fancy-battery)

(defface fancy-battery-discharging '((t :inherit warning))
  "Face for charging battery status."
  :group 'fancy-battery)

(defface fancy-battery--default-face '((t :inherit mode-line-highlight))
  "Face for charging battery status."
  :group 'fancy-battery)

(defface fancy-battery--current-capacity-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--rate-of-charge-or-discharge-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--status-verbose-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--status-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--temp-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--power-source-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--percentage-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--time-in-minutes-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--time-in-hours-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--time-mixed-format-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--linux-driver-version-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--apm-bios-version-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--apm-bios-status-verbose-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)
(defface fancy-battery--bsd-power-saving-mode-state-face '((t :inherit fancy-battery-default-face))
  "thing"
  :group 'fancy-battery)

;;
;; Internal details
;; -----------------
;;

;; This next line (...autoload) is an "autoload cookie" (aka "magic
;; comment that you shouldn't touch"). Yikes!
;; See also: https://www.gnu.org/software/emacs/manual/html_node/elisp/Autoload.html
;;;###autoload
(define-minor-mode krista-fancy-battery-mode
  "Display battery status in the mode line.

Like `display-battery-mode', but fancier, and with more
customization options.

With prefix argument ARG, enable Fancy Battery Mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

If `battery-status-function' is nil, the mode is not enabled.

The text in the mode line is controlled by `fancy-battery-mode-line'.
Battery information is obtained from `battery-status-function', 
and updated every `battery-update-interval' seconds."
  :global t
  :group 'battery
  ;; Mark the `fancy-battery-timer' as "dealt with"
  (when fancy-battery-timer
    (cancel-timer fancy-battery-timer))

  ;; Update the `global-mode-string' to include the battery info
  ;;
  ;; NOTE: `global-mode-string' is the mode line construct for
  ;; miscellaneous information. Modes that want to write to the mode
  ;; line are supposed to append data to the `global-mode-string'. In
  ;; the event that `global-mode-string' is nil, we cannot directly
  ;; append to it. To get around this, it is convention to set the
  ;; `global-mode-string' to the empty string, and *then* append.
  (unless global-mode-string
    (setq global-mode-string '("")))

  (cond

   ;; CASE: `krista-fancy-battery-mode' was disabled after it had already been enabled
   ;; => Remove any lingering fancy-battery info from mode-line
   ((not krista-fancy-battery-mode)
    (setq global-mode-string
          (delq 'fancy-battery-mode-line global-mode-string)))

   ;; CASE: Battery info is not available
   ;; => Disable `krista-fancy-battery-mode'
   ((not battery-status-function)
    (krista-fancy-battery-mode -1))

   ;; CASE: Default (i.e. `krista-fancy-battery-mode' is enabled)
   ;; => Append `fancy-battery-mode-line' to the `global-mode-string'
   ;;    (if it's not already present)
    ;; => Run the function `fancy-battery-update' every
   ;;    `battery-update-interval' seconds, starting now
   (t
    (add-to-list 'global-mode-string 'fancy-battery-mode-line t)
    (setq fancy-battery-timer
          (run-at-time
           nil      ; run `fancy-battery-update' for the first time right now
           battery-update-interval
           #'fancy-battery-update)))))

(defun fancy-battery-update ()
  "Update battery information.

Obtain the current battery status and store it in
`fancy-battery-last-status'. Run
`fancy-battery-status-update-functions', and finally update the
mode line."
  ;; Note: `battery-status-function' holds the name of the function
  ;; that provides info about the battery status
  (when battery-status-function
    (let ((status (funcall battery-status-function)))
      (setq fancy-battery-last-status status)

      ;; NOTE: this hook is never actually used by anything. The actual
      ;; 'mode-line'-updating logic is in `fancy-battery-default-mode-line'
      (run-hook-with-args 'fancy-battery-status-update-functions status)

      )
    
    ;; Force the mode-line to redraw itself (need to call
    ;; `force-mode-line-update' because the mode-line doesn't
    ;; automatically redraw itself when you change it)
    (force-mode-line-update 'all)))

;; This defcustom does not need to exist!!!
(defcustom fancy-battery-status-update-functions nil
  "Functions to run after a battery status update.

Each function is called with the status alist as returned by
`battery-status-function' as single argument.  If the battery
status is not available, the argument is nil.

This variable is an abnormal hook (i.e. the hook functions either
accept parameter(s), or return a value). See Info
Node `(elisp)Hooks'."
  :group 'fancy-battery
  :type 'hook)

(defun fancy-battery-default-mode-line ()
  "Assemble a mode line string for Fancy Battery Mode.

Display the remaining battery time, if available and
`fancy-battery-show-percentage' is non-nil, otherwise the
percentage.  If the battery is critical, use
`battery-critical-face'.  Otherwise use `fancy-battery-charging'
or `fancy-battery-discharging', depending on the current state."

  ;; (message "show time: %S" fancy-battery--show-time-mixed-format)
  (when fancy-battery-last-status
    (let* (

           (time (fancy-battery--translate-status
                  'fancy-battery--remaining-time-in-hours-and-min
                  fancy-battery-last-status))
           (percentage (fancy-battery--translate-status
                        'fancy-battery--battery-load-percentage
                        fancy-battery-last-status))
           (status-verbose (fancy-battery--translate-status
                            'fancy-battery--status-verbose
                            fancy-battery-last-status))
           (power-source (fancy-battery--translate-status
                            'fancy-battery--power-source
                            fancy-battery-last-status))
           ;; (face (pcase (cdr (assq ?b fancy-battery-last-status))
           ;;         ("!" 'fancy-battery-critical)
           ;;         ("+" 'fancy-battery-charging)
           ;;         (_ 'fancy-battery-discharging)))
           ;; (status-display (if
           ;;                     (or fancy-battery-show-percentage
           ;;                         (string= time "N/A"))
           ;;                     (and percentage (concat percentage "%%"))
           ;;                   time))
           )
      (message "percentage %S" percentage)
      (message "time %S" time)
      (message "status-verbose %S" status-verbose)
      (message "power-source %S" power-source)
      ;; (if nil ;status-display
          ;; (propertize status-display 'face face)
        ;; Battery status is not available
      ;; (propertize "N/A" 'face 'error)))
    )))


(defun fancy-battery--translate-status (battery-info-key status)
  "Extract information from `status' (i.e. the output of calling
  the `battery-status-function'). Recognize the
  following keys (and return appropriate data):

`fancy-battery--current-capacity'
    Current capacity (mAh or mWh)

`fancy-battery--rate-of-charge-or-discharge'
    Current rate of charge or discharge

`fancy-battery--status-verbose'
    Battery status (verbose). Examples: 'critical', 'high',
    'low', 'charging'

`fancy-battery--status'
    Battery status: empty string means high, `-' means low, `!'
    means critical, and `+' means charging

`fancy-battery--temp'
    Temperature (in degrees Celsius)

`fancy-battery--power-source'
    Aka 'AC line status (verbose)'

`fancy-battery--percentage'

`fancy-battery--time-in-minutes'
    Remaining time (to charge or discharge) in minutes

`fancy-battery--time-in-hours'
    Remaining time (to charge or discharge) in hours

`fancy-battery--time-mixed-format'
    Remaining time (to charge or discharge) in the form `h:min'

`fancy-battery--linux-driver-version'
`fancy-battery--apm-bios-version'
`fancy-battery--apm-bios-status-verbose'

`fancy-battery--bsd-power-saving-mode-state'
    Advanced power saving mode state (verbose). BSD only
"
  ;; I *STRONGLY* recommend that you do *NOT* try to understand this
  ;; function. Although I have done my best to document its inner
  ;; workings, this function wraps some particularly ugly logic.
  ;;
  ;;
  ;; THE DETAILS:
  ;; -------------
  ;;
  ;; Calling the `battery-status-function' will return `status', an
  ;; alist of so-called '%-sequences'.
  ;;
  ;; Example value of `status':
  ;;
  ;;     ((76 . \"AC\")
  ;;      (112 . \"53\")
  ;;      (66 . \"charging\")
  ;;      (98 . \"+\")
  ;;      (104 . \"2\")
  ;;      (109 . \"117\")
  ;;      (116 . \"1:57\"))
  ;;
  ;; Example %-sequence listed in `battery.el':
  ;;
  ;;     %m Remaining time (to charge or discharge) in minutes
  ;;
  ;; This particular %-sequence means that the ASCII value of 'm' is
  ;; mapped to the 'Remaining time (to charge or discharge) in
  ;; minutes' in `status'. The ASCII value of 'm' is 109 (*), so we
  ;; see that the 'Remaining time (to charge or discharge) in minutes'
  ;; is 117 minutes.
  ;;
  ;; (*) Note: You can see the ASCII value of 'm' by running '?m'
  ;;
  ;; This function exists to translate the %-sequences into an alist of.
  ;; For instance, this function executes the line
  ;; 
  ;;     (fancy-battery--time-in-minutes . (cdr (assq ?m status)))
  ;;
  ;; in order to map the 'Remaining time (to charge or discharge) in
  ;; minutes' to the symbol `fancy-battery--time-in-minutes'

  ;; REMAPPING THE %-SEQUENCES
  ;; --------------------------
  ;;
  ;; The set of %-sequences returned is dependent on the value of
  ;; `battery-status-function', which in turn is (usually) dependent
  ;; on your operating system.
  ;;
  ;; For example, if you're on BSD, then your battery-status-function
  ;; is probably going to be `battery-bsd-apm'. You'll get a
  ;; different set of %-sequences than you would if you were on
  ;; Darwin/macOS (because the Darwin battery-status-function is
  ;; `battery-pmset', and because `battery-pmset' returns different
  ;; values than `battery-bsd-apm').
  ;;
  ;; I have indicated the %-sequences which are only available on
  ;; certain systems. These indications are repeated from `battery.el'
  ;;
  ;; NOTE: If a %-sequence is not provided by the
  ;; `battery-status-function', then the corresponding fancy-battery
  ;; variable here will be set to nil.
  ;; (message "battery-info-key: %S" battery-info-key)
  (pcase battery-info-key
   ;; %-sequences that are generally available:
   ('fancy-battery--current-capacity                  (cdr (assq ?c status)))
   ('fancy-battery--rate-of-charge-or-discharge       (cdr (assq ?r status)))
   ('fancy-battery--status-verbose                    (cdr (assq ?B status)))
   ('fancy-battery--status                            (cdr (assq ?b status)))
   ('fancy-battery--temp                              (cdr (assq ?d status)))
   ('fancy-battery--power-source                      (cdr (assq ?L status)))
   ('fancy-battery--percentage                        (cdr (assq ?p status)))
   
   ('fancy-battery--time-in-minutes                   (cdr (assq ?m status)))
   ('fancy-battery--time-in-hours                     (cdr (assq ?h status)))
   ('fancy-battery--time-mixed-format                 (cdr (assq ?t status)))
   ;; `battery-linux-proc-apm' only:
   ('fancy-battery--linux-driver-version              (cdr (assq ?v status)))
   ('fancy-battery--apm-bios-version                  (cdr (assq ?V status)))
   ('fancy-battery--apm-bios-status-verbose           (cdr (assq ?I status)))
   ;; `battery-bsd-apm' only:
   ('fancy-battery--bsd-power-saving-mode-state       (cdr (assq ?P status)))
   (_                  (error "Unknown battery-info-key: %S" battery-info-key))
   )
  )

;; Define the `fancy-battery' Group
;;
;; - The customize-variable widget supports "Groups" of variables.
;; - This line creates a Group of variables called `fancy-battery' to
;;   tag variables that are relevant to the `fancy-battery-mode' package
;; - See also: defgroup documentation
(defgroup fancy-battery
  ;; Link to the `fancy-battery' Group from the
  ;; `battery-update-interval' page in the custom-variable widget
  '((battery-update-interval custom-variable))
  "Powerful and fancy battery status updates."
  :group 'battery ; Inherit from the `battery' group
  :prefix "fancy-battery-")

(defcustom fancy-battery-mode-line
  '(:eval (fancy-battery-default-mode-line))
  "Mode line string for `fancy-battery-mode'.

This variable is a mode line format sexp.  See Info
Node `(elisp)Mode Line Format' for more information, and
`fancy-battery-default-mode-line' for the default value.

Do *not* call `battery-status-function' in the mode line format.
This would *significantly* slow down mode line updates.  Instead,
use the cached status in `fancy-battery-last-status'."
  :type 'sexp
  :group 'fancy-battery
  :risky t)

(defvar fancy-battery-timer nil
  "Timer to update the battery information.")

(defvar fancy-battery-last-status nil
  "Last battery status.")

;(provide 'fancy-battery)
(krista-fancy-battery-mode t)
(message "Buffer evaluated!!!")
;;; fancy-battery.el ends here
