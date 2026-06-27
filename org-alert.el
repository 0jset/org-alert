;;; org-alert.el --- Notify org deadlines via notify-send

;; Copyright (C) 2015 Stephen Pegoraro

;; Author: Stephen Pegoraro <spegoraro@tutive.com>
;; Version: 0.2.0
;; Package-Requires: ((org "9.0") (alert "1.2") (org-ql "0.9-pre") (ts "0.2-pre"))
;; Keywords: org, org-mode, notify, notifications, calendar
;; URL: https://github.com/spegoraro/org-alert

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides functions to display system notifications for
;; any org-mode deadlines that are due in your agenda. To perform a
;; one-shot check call (org-alert-deadlines). To enable repeated
;; checking call (org-alert-enable) and to disable call
;; (org-alert-disable). You can set the checking interval by changing
;; the org-alert-interval variable to the number of seconds you'd
;; like.


;;; Code:

(require 'cl-lib)
(require 'alert)
(require 'org-agenda)
(require 'org-ql)
(require 'ts)
(require 'map)

(defgroup org-alert nil
  "Notify org deadlines via notify-send."
  :group 'org-agenda)

;; TODO look for a property of the agenda entry as suggested in
;; https://github.com/spegoraro/org-alert/issues/20
(defcustom org-alert-notify-cutoff nil
  "Default time in minutes before a deadline a notification should be sent.
- nil means just use timers.
- non-nil means check each headline and use `alert'
  if it's time < now + `org-alert-notify-cutoff'.
setting it will break SCHEDULED handling."
  :group 'org-alert
  :type '(choice integer (const nil)))

(defcustom org-alert-notification-title "*org*"
  "Title to be sent with notify-send."
  :group 'org-alert
  :type 'string)

(defcustom org-alert-cutoff-prop
  "REMINDERN"
  "org property used to set a custom cutoff for an individual entry"
  :group 'org-alert
  :type 'string)

(defcustom org-alert-notification-category
  'org-alert
  "The symbol to pass to alert as the :category property, in order
to allow differentiation from other uses of alert"
  :group 'org-alert
  :type 'symbol)

(defcustom org-alert-timers
  `((:timer (t 300)
     :doc "check for timestamp-events that are going to be very soon"
     :alert ,(lambda ()
               `(:where (and (not (done))
                             (not (scheduled))
                             (ts-active :with-time t
                                        :from ,(ts-now)
                                        :to ,(ts-adjust 'hour 1 (ts-now)))))))
    (:timer (t ,(* 30 60))
     :doc "check for timestamp-events that are today without time, or with time and later"
     :alert ,(lambda ()
               `(:where (and (not (done))
                             (not (scheduled))
                             ;; events that will happen today
                             (ts-active :on today)
                             ;; events that are happening all day
                             (or (ts-active :with-time nil)
                                 ;; not events that already happen
                                 (not (ts-active :with-time t :to ,(ts-now))))))))
    (:timer (t ,(* 60 60 2))
     :doc "check for all events that are in near future or that need to be done"
     :alert ,(lambda ()
               `(:where
                 (and (not (done))
                      (cond
                       ((deadline) (deadline auto))
                       ;; if task was scheduled before now - then we should get an alert
                       ((scheduled) (scheduled :to ,(ts-now)))
                       ((ts
                         :from ,(ts-now)
                         :to ,(ts-update
                               (make-ts :hour 0 :minute 0 :second 0
                                        :day (+ 5 (ts-d (ts-now)))
                                        :month (ts-m (ts-now))
                                        :year (ts-Y (ts-now)))))
                        (not (ts-inactive))))
                      )))))
  "Timers and alerts to create.
:timer - timer arguments
:doc - just for clarity what this alert does
:alert - function that returns arguments for org-ql-query

NOTE:
- (planning) without time won't match deadline without specified time
  (only date).
- each alert should be a function because it can have something that
  changes over time inside like (ts-now)
- BUG: scheduled timestamp can be active but (not (ts-inactive))
  filters it out.
- BUG: (cond ((ts-active))) doesn't work
- PUSH: org-ql doesn't match only 1 timestamp in entry - add functionality
  to org-ql or use one of the forks with this functionality.
")

(defun org-alert--read-subtree ()
  "Return the current subtree as a string.
Adapted from `org-copy-subtree` from `org-mode'."
  (org-preserve-local-variables
   (let (beg end folded (beg0 (point)))
     (org-back-to-heading t)
     (setq beg (point))
     (skip-chars-forward " \t\r\n")
     (save-match-data
       (save-excursion (outline-end-of-heading)
                       (setq folded (org-invisible-p)))
       (ignore-errors (org-forward-heading-same-level (1- n) t))
       (org-end-of-subtree t t))
     ;; Include the end of an inlinetask
     (when (and (featurep 'org-inlinetask)
                (looking-at-p (concat (org-inlinetask-outline-regexp)
                                      "END[ \t]*$")))
       (end-of-line))
     (setq end (point))
     (goto-char beg0)
     (when (> end beg)
       (setq org-subtree-clip-folded folded)
       (buffer-substring-no-properties beg end)))))

;; I think this is unnecessary now that we're using read-subtree
;; instead of copy-subtree
(defun org-alert--strip-text-properties (text)
  "Strip all of the text properties from a copy of TEXT and
return the stripped copy"
  (let ((text (substring text)))
    (set-text-properties 0 (length text) nil text)
    text))

(defun org-alert--grab-subtree ()
  "Return the current org subtree as a string with the
text-properties stripped, along with the cutoff to apply"
  (let* ((subtree (org-alert--read-subtree))
         (props (org-entry-properties))
         (prop (alist-get org-alert-cutoff-prop props org-alert-notify-cutoff nil #'string-equal))
         (prop (if (stringp prop)
                   (string-to-number prop)
                 prop))
         (text (org-alert--strip-text-properties subtree)))
    (list
     (apply #'concat
            (cl-remove-if #'(lambda (s) (string= s ""))
                          (cdr (split-string text "\n"))))
     prop)))

(defun org-alert--parse-entry ()
  "Parse an entry from the org agenda.
Returns a list of:
1. category
2. heading
3. timestamp with scheduled/deadline if exist.
4. cutoff to apply"
  (let* ((category (org-entry-get nil "CATEGORY" 't))
         (heading (org-get-heading t t t t))
         (head (org-alert--strip-text-properties heading))
         (entry
          (cl-destructuring-bind (body cutoff) (org-alert--grab-subtree)
            (cond
             ((string-match (org-re-timestamp 'active) head)
              (list (replace-match "" nil nil head) ;; we want to have timestamp in other place
                    (match-string 1 head) cutoff))
             ;; NOTE: we choose deadline if deadline and scheduled are together in an entry
             ((or (string-match (org-re-timestamp 'deadline) body)
                  (string-match (org-re-timestamp 'scheduled) body))
              (list head (match-string 0 body) cutoff))
             ((string-match (org-re-timestamp 'active) body)
              (list head (match-string 1 body) cutoff))))))
    (cons category entry)))

(defun org-alert--is-time-ok (time cutoff)
  "Check if TIME is less than CUTOFF (in minutes) from now."
  (let* ((time (ts-parse-org time))
         (diff (/ (ts-diff time (ts-now)) 60)))
    (and (> 0 diff) (< cutoff diff))))

(defun org-alert--dispatch ()
  "Parse header at point and call `alert' on it if it has timestamp."
  (interactive) ;; for debugging
  (let ((entry (org-alert--parse-entry)))
    (when entry
      (cl-destructuring-bind (category head time cutoff) entry
        (if time
            (when (or (not cutoff) (org-alert--is-time-ok time cutoff))
              (alert head
                     :title (concat time " | " category)
                     :category org-alert-notification-category))
          (alert head :title (concat org-alert-notification-title " | " category)
                 :category org-alert-notification-category))))))

(defun org-alert-check--in-list (get-plist)
  "Call `org-ql-query' with arguments from GET-PLIST."
  (let* ((query (map-merge 'plist
                           (list :select #'org-alert--dispatch
                                 :from (org-agenda-files)
                                 :order-by '(date priority))
                           (funcall get-plist)))
         (org-ql-cache (make-hash-table)))
    (apply #'org-ql-query query)))

(defun org-alert-check (num)
  "Call NUM timer alert from `org-alert-timers'."
  (interactive "nTimer function to call: ")
  (org-alert-check--in-list (plist-get (nth num org-alert-timers) :alert)))

;;;###autoload
(defun org-alert-enable ()
  "Enable the notification timer. Cancels existing timers if running."
  (interactive)
  (org-alert-disable)
  (cl-dolist (i (number-sequence 0 (1- (length org-alert-timers))))
    (let* ((timer-plist (nth i org-alert-timers))
           (timer-args (plist-get timer-plist :timer))
           (alert (plist-get timer-plist :alert)))
      (apply #'run-at-time (append timer-args (list #'org-alert-check--in-list alert))))))

(defun org-alert-disable ()
  "Cancel the running notification timer."
  (interactive)
  (dolist (timer timer-list)
    (when (string-prefix-p "org-alert-check"
                           (condition-case nil (symbol-name (elt timer 5))
                             (error "")))
      (cancel-timer timer))))

(provide 'org-alert)

;;; org-alert.el ends here
