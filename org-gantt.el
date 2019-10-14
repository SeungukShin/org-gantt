;;; org-gantt.el --- Create integrated pgf gantt charts from task headlines

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(require 'cl)
(require 'cl-lib)
(require 'parse-time)
(require 'ob-latex)

;; Customization
(defgroup org-gantt nil "Customization of org-gantt."
  :group 'org)

(defcustom org-gantt-weekend '(0 6)
  "Weekend and holiday.
0 is Sunday and 6 is Saturday."
  :type '(repeat integer)
  :group 'org-gantt)

(defcustom org-gantt-holiday-list '("2019-08-15" "2019-09-12" "2019-09-13" "2019-10-03" "2019-10-09")
  "The list of holidays."
  :type '(repeat string)
  :group 'org-gantt)

(defcustom org-gantt-holiday-vrule "red!15"
  "The style for the holiday lines."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-weekend-style "{dashed}"
  "The style for the weekend lines."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-weekday-style "{dashed}"
  "The style for the weekday lines."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-title "year, month=name, day"
  "The style for the title calendar."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-compressed-title "year, month"
  "The style for the compressed title calendar."
  :type '(string)
  :group 'org-gantt)

;; To filter headlines for id option
(defconst org-gantt-target-stage-off 0
  "Target id does not exist in the option.")
(defconst org-gantt-target-stage-prepare 1
  "Target id exists in the option but is not yet present.")
(defconst org-gantt-target-stage-current 2
  "Target id exists in the option and the current headline is in it.")
(defconst org-gantt-target-stage-exit 3
  "Target id exists in the option and the current headline is out of it.")
(defvar org-gantt-target-stage org-gantt-target-stage-off
  "Indicator whether current headline is included in target or not.")

(defvar org-gantt-target-id nil
  "Target ID to translate gantt chart.")

(defvar org-gantt-target-level nil
  "Target level to translate gantt chart.")

;; Options for gantt chart
(defvar org-gantt-start-date-from-option nil)
(defvar org-gantt-end-date-from-option nil)
(defvar org-gantt-start-date nil
  "Start date for gantt chart.")
(defvar org-gantt-end-date nil
  "End date for gantt chart.")

;; Hash table for properties of headlines
(defconst org-gantt-id-prop :id)
(defconst org-gantt-level-prop :level)
(defconst org-gantt-start-from-org-prop :start-from-org
  "Start time is set from scheduled of org file. t or nil")
(defconst org-gantt-end-from-org-prop :end-from-org
  "End time is set from deadline of org file. t or nil")
(defconst org-gantt-start-time-prop :start-time)
(defconst org-gantt-end-time-prop :end-time)
(defconst org-gantt-effort-time-prop :effort-time)
(defconst org-gantt-linked-to-prop :linked-to)
(defconst org-gantt-parent-prop :parent)
(defconst org-gantt-has-child-prop :has-child)
(defconst org-gantt-milestone-prop :milestone)

(defvar org-gantt-hash-table nil
  "Hash table for properties of headlines")

;; Index of id generator
(defvar org-gantt-id-index 0)


(defun dbg-msg (format-string &rest args)
  "Print debug message."
  ;(apply #'message format-string args)
  (insert (apply #'format format-string args)))

(defun* org-gantt-headline-debug-output (headline)
  "Print headline information.
HEADLINE is a headline from the org-data."
  (unless org-gantt-hash-table
    (return-from org-gantt-headline-debug-output))
  (unless headline
    (return-from org-gantt-headline-debug-output))
  (let (id prop start-time end-time effort-time linked-to parent)
    (setq id (org-element-property :ID headline))
    (setq prop (gethash id org-gantt-hash-table))
    (unless prop
      (return-from org-gantt-headline-debug-output))
    (dbg-msg "%s %s %s\n" id
             (plist-get prop org-gantt-level-prop)
             (org-element-property :raw-value headline))
    (setq start-time (plist-get prop org-gantt-start-time-prop)
          end-time (plist-get prop org-gantt-end-time-prop)
          effort-time (plist-get prop org-gantt-effort-time-prop)
          linked-to (plist-get prop org-gantt-linked-to-prop)
          parent (plist-get prop org-gantt-parent-prop))
    (if start-time
        (dbg-msg "  start: %s\n" (format-time-string "%Y-%m-%d" start-time)))
    (if end-time
        (dbg-msg "  end: %s\n" (format-time-string "%Y-%m-%d" end-time)))
    (if effort-time
        (dbg-msg "  effort: %d\n" effort-time))
    (dbg-msg "  link: %s\n" linked-to)
    (dbg-msg "  parent: %s\n" parent)))

(defun* org-gantt-mark-holiday (start-date end-date)
  (setq end-date (time-add end-date (* 24 60 60)))
  (if (time-less-p end-date start-date)
      (return-from org-gantt-mark-holiday))
  (let (body current holiday)
    (setq current start-date
          body "")
    (while (time-less-p current end-date)
      (if (member (string-to-number (format-time-string "%w" current)) org-gantt-weekend)
          (setq body (concat body "  \\ganttvrule{}{" (format-time-string "%Y-%m-%d" current) "}\n")))
      (setq current (time-add current (* 24 60 60))))
    (setq start-date (time-subtract start-date (* 24 60 60)))
    (dolist (holiday org-gantt-holiday-list)
      (setq current (apply 'encode-time (org-parse-time-string holiday)))
      (if (and (time-less-p start-date current)
               (time-less-p current end-date))
          (setq body (concat body "  \\ganttvrule{}{" holiday "}\n"))))
    (unless (equal body "")
      (setq body (concat "\n\\begin{scope}[on background layer]\n" body))
      (setq body (concat body "\\end{scope}")))
    body))

(defun* org-gantt-headline-output-link (headline)
  "Output gantt links.
HEADLINE is a headline from the org-data."
  (unless org-gantt-hash-table
    (return-from org-gantt-headline-output-link))
  (unless headline
    (return-from org-gantt-headline-output-link))
  (let (id prop start-time end-time linked-to msg)
    (setq id (org-element-property :ID headline))
    (setq prop (gethash id org-gantt-hash-table))
    (unless prop
      (return-from org-gantt-headline-output-link))
    (setq start-time (plist-get prop org-gantt-start-time-prop)
          end-time (plist-get prop org-gantt-end-time-prop))
    (when (or (time-less-p end-time org-gantt-start-date)
              (time-less-p org-gantt-end-date start-time))
      (return-from org-gantt-headline-output-link))
    (setq linked-to (plist-get prop org-gantt-linked-to-prop))
    (when linked-to
      (dolist (link linked-to)
        (setq prop (gethash link org-gantt-hash-table))
        (when prop
          (setq start-time (plist-get prop org-gantt-start-time-prop)
                end-time (plist-get prop org-gantt-end-time-prop))
          (when (and (time-less-p start-time org-gantt-end-date)
                     (time-less-p org-gantt-start-date end-time))
            (setq msg (concat msg "\n\\ganttlink{" id "}{" link "}")))))
      (unless msg
        (return-from org-gantt-headline-output-link))
      msg)))

(defun* org-gantt-headline-output-bar (headline)
  "Output gantt bars.
HEADLINE is a headline from the org-data."
  (unless org-gantt-hash-table
    (return-from org-gantt-headline-output-bar))
  (unless headline
    (return-from org-gantt-headline-output-bar))
  (let (id prop level start-time end-time tags msg)
    (setq id (org-element-property :ID headline))
    (setq prop (gethash id org-gantt-hash-table))
    (unless prop
      (return-from org-gantt-headline-output-bar))
    (setq level (plist-get prop org-gantt-level-prop)
          start-time (plist-get prop org-gantt-start-time-prop)
          end-time (plist-get prop org-gantt-end-time-prop)
          tags (org-element-property :tags headline))
    (when (or (time-less-p end-time org-gantt-start-date)
              (time-less-p org-gantt-end-date start-time))
      (return-from org-gantt-headline-output-bar))
    (setq msg (concat msg "\\\\\n"))
    (setq msg (concat msg (make-string (* 2 (- level 1)) ? )))
    (cond ((plist-get prop org-gantt-has-child-prop)
           (setq msg (concat msg "\\ganttgroup")))
          ((and tags (member "milestone" tags))
           (setq msg (concat msg "\\ganttmilestone")))
          (t
           (setq msg (concat msg "\\ganttbar"))))
    (setq msg (concat msg "[name=" id "]"))
    (setq msg (concat msg "{" (org-element-property :raw-value headline) "}"))
    (if (time-less-p start-time org-gantt-start-date)
        (setq start-time org-gantt-start-date))
    (setq msg (concat msg (format-time-string "{%Y-%m-%d}" start-time)))
    (if (time-less-p org-gantt-end-date end-time)
        (setq end-time org-gantt-end-date))
    (setq msg (concat msg (format-time-string "{%Y-%m-%d}" end-time)))
    msg))

(defconst org-gantt-update-caller-headline 0)
(defconst org-gantt-update-caller-child 1)
(defconst org-gantt-update-caller-linked-from 2)
(defun* org-gantt-update-entry (id start-time end-time from)
  "Update start and end time with effort, parent, and linked-to.
ID is an identifier of the target to update time.
START-TIME is an start time for the target.
END-TIME is an end time for the target.
FROM is caller where 0 is headline, 1 is child, and 2 is linked-from."
  (unless id
    (return-from org-gantt-update-entry))
  (unless org-gantt-hash-table
    (return-from org-gantt-update-entry))
  (let (prop start-from-org end-from-org current-start-time current-end-time effort-time parent linked-to)
    (setq prop (gethash id org-gantt-hash-table))
    (unless prop
      (return-from org-gantt-update-entry))
    (setq start-from-org (plist-get prop org-gantt-start-from-org-prop)
          end-from-org (plist-get prop org-gantt-end-from-org-prop)
          current-start-time (plist-get prop org-gantt-start-time-prop)
          current-end-time (plist-get prop org-gantt-end-time-prop)
          effort-time (plist-get prop org-gantt-effort-time-prop)
          parent (plist-get prop org-gantt-parent-prop)
          linked-to (plist-get prop org-gantt-linked-to-prop))
    (if (= from org-gantt-update-caller-child)
        (plist-put prop org-gantt-has-child-prop "t"))
    (when (and start-time
               (not start-from-org)
               (or (not current-start-time)
                   (time-less-p start-time current-start-time)))
      (setq current-start-time start-time)
      (plist-put prop org-gantt-start-time-prop start-time))
    (when (and end-time
               (not end-from-org)
               (or (not current-end-time)
                   (time-less-p current-end-time end-time)))
      (setq current-end-time end-time)
      (plist-put prop org-gantt-end-time-prop end-time))
    ;; calculate start time or end time from effort time
    (when (and effort-time current-start-time (not current-end-time))
      (setq current-end-time (time-add current-start-time (* (- effort-time 24) 60 60)))
      (plist-put prop org-gantt-end-time-prop current-end-time))
    (when (and effort-time (not current-start-time) current-end-time)
      (setq current-start-time (time-subtract current-end-time (* (- effort-time 24) 60 60)))
      (plist-put prop org-gantt-start-time-prop current-start-time))
    ;; avoid weekend
    (when current-start-time
      (while (member (string-to-number (format-time-string "%w" current-start-time)) org-gantt-weekend)
        (setq current-start-time (time-add current-start-time (* 24 60 60))))
      (plist-put prop org-gantt-start-time-prop current-start-time))
    (when current-end-time
      (while (member (string-to-number (format-time-string "%w" current-end-time)) org-gantt-weekend)
        (setq current-end-time (time-subtract current-end-time (* 24 60 60))))
      (plist-put prop org-gantt-end-time-prop current-end-time))
    ;; fix wrong schedule and deadline
    (if (time-less-p current-end-time current-start-time)
        (cond ((and start-from-org (not end-from-org))
               (setq current-end-time current-start-time)
               (plist-put prop org-gantt-end-time-prop current-end-time))
              ((not start-from-org)
               (setq current-start-time current-end-time)
               (plist-put prop org-gantt-start-time-prop current-start-time))))
    ;; update org-gantt-start-date and org-gantt-end-date to display gantt chart
    (if (and (not org-gantt-start-date-from-option)
             (or (not org-gantt-start-date)
                 (time-less-p current-start-time org-gantt-start-date)))
        (setq org-gantt-start-date current-start-time))
    (if (and (not org-gantt-end-date-from-option)
             (or (not org-gantt-end-date)
                 (time-less-p org-gantt-end-date current-end-time)))
        (setq org-gantt-end-date current-end-time))
    ;; propagate start time and end time to parent and linked-to
    (when parent
      (org-gantt-update-entry parent current-start-time current-end-time org-gantt-update-caller-child))
    (dolist (link linked-to)
      (org-gantt-update-entry link (time-add current-end-time (* 24 60 60)) nil org-gantt-update-caller-linked-from))))

(defun* org-gantt-headline-process (headline)
  "Update a hash entry with related entries.
HEADLINE is a headline from the org-data."
  (unless org-gantt-hash-table
    (return-from org-gantt-headline-process))
  (unless headline
    (return-from org-gantt-headline-process))
  (let (id prop start-time end-time)
    (setq id (org-element-property :ID headline))
    (setq prop (gethash id org-gantt-hash-table))
    (unless prop
      (return-from org-gantt-headline-process))
    (setq start-time (plist-get prop org-gantt-start-time-prop)
          end-time (plist-get prop org-gantt-end-time-prop))
    (org-gantt-update-entry id start-time end-time org-gantt-update-caller-headline)))

(defun* org-gantt-get-timestamp (type headline)
  "Get scheduled or deadline time.
TYPE is :scheduled or :deadline.
HEADLINE is a headline from the org-data."
  (unless type
    (return-from org-gantt-get-timestamp))
  (unless headline
    (return-from org-gantt-get-timestamp))
  (let (timestamp timestamp-string)
    (setq timestamp (org-element-property type headline))
    (unless timestamp
      (return-from org-gantt-get-timestamp))
    (setq timestamp-string (org-element-property :raw-value timestamp))
    (apply 'encode-time (org-parse-time-string timestamp-string))))

(defun* org-gantt-get-duration (headline)
  "Get effort duration in hour.
HEADLINE is a headline from the org-data"
  (unless headline
    (return-from org-gantt-get-duration))
  (let ((effort-time (org-element-property :EFFORT headline)))
    (unless effort-time
      (return-from org-gantt-get-duration))
    (/ (org-duration-string-to-minutes effort-time) 60)))

(defun* org-gantt-headline-set-hash (id level headline)
  "Initialize a hash entry with information of a headline.
ID is an identifier of the headline.
LEVEL is a level of the headline.
HEADLINE is a headline from the org-data."
  (unless org-gantt-hash-table
    (return-from org-gantt-headline-set-hash))
  (unless headline
    (return-from org-gantt-headline-set-hash))
  (let (prop start-time end-time effort-time parent linked-to)
    (setq prop `(,org-gantt-id-prop ,id))
    (plist-put prop org-gantt-level-prop level)
    ;; start, end and effort time
    (setq start-time (org-gantt-get-timestamp :scheduled headline)
          end-time (org-gantt-get-timestamp :deadline headline)
          effort-time (org-gantt-get-duration headline))
    (if start-time
        (plist-put prop org-gantt-start-from-org-prop "t"))
    (if end-time
        (plist-put prop org-gantt-end-from-org-prop "t"))
    (if (and start-time effort-time)
        (plist-put prop org-gantt-end-from-org-prop "t"))
    (if (and end-time effort-time)
        (plist-put prop org-gantt-start-from-org-prop "t"))
    (plist-put prop org-gantt-start-time-prop start-time)
    (plist-put prop org-gantt-end-time-prop end-time)
    (plist-put prop org-gantt-effort-time-prop effort-time)
    ;; parent
    (setq parent (org-element-property :parent headline))
    (plist-put prop org-gantt-parent-prop (org-element-property :ID parent))
    ;; linked-to
    (setq linked-to (org-element-property :LINKED-TO headline))
    (if linked-to
        (plist-put prop org-gantt-linked-to-prop (split-string linked-to)))
    (puthash id prop org-gantt-hash-table)))

(defun* org-gantt-headline-prepare (headline)
  "Filter out a headline which is not included in target.
HEADLINE is a headline from the org-data."
  (unless headline
    (return-from org-gantt-headline-prepare))
  (let ((id (org-element-property :ID headline))
        (hide (org-element-property :HIDE headline))
        (level (org-element-property :level headline)))
    (if hide
        (return-from org-gantt-headline-prepare))
    (when (not id)
      (setq org-gantt-id-gen (1+ org-gantt-id-gen))
      (setq id (format "org-gantt-%d" org-gantt-id-gen))
      (org-element-put-property headline :ID id))
    (cond ((eq org-gantt-target-stage org-gantt-target-stage-off)
           (org-gantt-headline-set-hash id level headline))
          ((eq org-gantt-target-stage org-gantt-target-stage-prepare)
           (when (string= org-gantt-target-id id)
             (setq org-gantt-target-level level
                   org-gantt-target-stage org-gantt-target-stage-current)
             (org-gantt-headline-set-hash id level headline)))
          ((eq org-gantt-target-stage org-gantt-target-stage-current)
           (if (>= org-gantt-target-level level)
               (setq org-gantt-target-stage org-gantt-target-stage-exit)
             (org-gantt-headline-set-hash id level headline)))
          ((eq org-gantt-target-stage org-gantt-target-stage-exit)))))

(defun org-gantt-generate-vgrid ()
  "Generate virtual grid setting based on weekday and weekend"
  (let (day-of-week day-of-week-list num count vgrid current-type-of-day type-of-day)
    (setq day-of-week (string-to-number (format-time-string "%w" org-gantt-start-date)))
    (setq day-of-week-list (number-sequence day-of-week 6))
    (if (< (length day-of-week-list) 8)
        (setq day-of-week-list (append  day-of-week-list (number-sequence 0 day-of-week))))
    (setq num 0
          count 0
          vgrid "vgrid={"
          type-of-day 3)				; unknown value
    (while (< num 7)
      (if (or
           (member (nth num day-of-week-list) org-gantt-weekend)
           (member (nth (1+ num) day-of-week-list) org-gantt-weekend))
          (setq current-type-of-day 0)			; weekend
        (setq current-type-of-day 1))			; weekday
      (if (or (= type-of-day 3)
              (= current-type-of-day type-of-day))
          (setq type-of-day current-type-of-day
                count (1+ count))
        (if (= type-of-day 0)
            (setq vgrid (concat vgrid (format "*%d%s," count org-gantt-weekend-style)))
          (setq vgrid (concat vgrid (format "*%d%s," count org-gantt-weekday-style))))
        (setq type-of-day current-type-of-day
              count 1))
      (if (= num 6)
          (if (= type-of-day 0)
              (setq vgrid (concat vgrid (format "*%d%s," count org-gantt-weekend-style)))
            (setq vgrid (concat vgrid (format "*%d%s," count org-gantt-weekday-style)))))
      (setq num (1+ num)))
    (setq vgrid (substring vgrid 0 -1))		; remove the last \n
    (setq vgrid (concat vgrid "}"))
    vgrid))

(defun org-gantt-plist-to-alist (plist)
  "Transform property list PLIST into an association list."
  (cl-loop for p on plist by #'cddr
	   collect (cons (car p) (cadr p))))

(defun org-dblock-write:org-gantt-chart (params)
  "Update gantt chard code.
PARAMS determine several options of the gantt chart."
  (setq org-gantt-hash-table (make-hash-table :test 'equal))
  (setq org-gantt-id-gen 0)
  (setq org-gantt-target-id (plist-get params :id))
  (setq org-gantt-target-level nil)
  (setq org-gantt-start-date-from-option nil
        org-gantt-end-date-from-option nil
        org-gantt-start-date nil
        org-gantt-end-date nil)
  (if org-gantt-target-id
      (setq org-gantt-target-stage org-gantt-target-stage-prepare)
    (setq org-gantt-target-stage org-gantt-target-stage-off))
  (with-current-buffer (current-buffer)
    (let (parsed-buffer tikz-scale tikz-options
                        start-date-string end-date-string today-value parameters
                        header footer body)
      (setq parsed-buffer (org-element-parse-buffer)
            tikz-scale (plist-get params :tikz-scale)
            tikz-options (plist-get params :tikz-options)
            start-date-string (plist-get params :start-date)
            end-date-string (plist-get params :end-date)
            today-value (plist-get params :today)
            parameters (plist-get params :parameters)
            header ""
            footer ""
            body "")
      (when start-date-string
        (setq org-gantt-start-date-from-option "t")
        (setq org-gantt-start-date (apply 'encode-time (org-parse-time-string start-date-string))))
      (when end-date-string
        (setq org-gantt-end-date-from-option "t")
        (setq org-gantt-end-date (apply 'encode-time (org-parse-time-string end-date-string))))
      (org-element-map parsed-buffer 'headline #'org-gantt-headline-prepare)
      (org-element-map parsed-buffer 'headline #'org-gantt-headline-process)
      (dolist (msg (org-element-map parsed-buffer 'headline #'org-gantt-headline-output-bar))
        (setq body (concat body msg)))
      (dolist (msg (org-element-map parsed-buffer 'headline #'org-gantt-headline-output-link))
        (setq body (concat body msg)))
      (setq body (concat body (org-gantt-mark-holiday org-gantt-start-date org-gantt-end-date)))
      ;; tikz options
      (when (or tikz-scale tikz-options)
        (setq header (concat header "\\begin{tikzpicture}[")))
      (when tikz-scale
        (setq header (concat header "scale=" tikz-scale ", every node/.style={scale=" tikz-scale "}")))
      (when tikz-options
        (setq header (concat header ", " tikz-options)))
      (when (or tikz-scale tikz-options)
        (setq header (concat header "]\n"))
        (setq footer (concat "\\end{tikzpicture}\n" footer)))
      ;; gantt chart options
      (setq header (concat header "\\begin{ganttchart}[time slot format=isodate, "))
      (setq header (concat header "canvas/.append style={fill=none}, "))
      (setq header (concat header (org-gantt-generate-vgrid) ", "))
      (setq header (concat header "vrule offset=.5, vrule/.style={draw=" org-gantt-holiday-vrule ", line width=\\ganttvalueof{x unit}"))
      (if tikz-scale
        (setq header (concat header "*" tikz-scale)))
      (setq header (concat header "}"))
      (if (plist-get params :compress)
          (setq header (concat header ", compress calendar")))
      (when today-value
        (if (equal t today-value)
            (setq today-value (format-time-string "%Y-%m-%d" (current-time))))
        (setq header (concat header ", today=" today-value)))
      (if (plist-get params :parameters)
          (setq header (concat header ", " (plist-get params :parameters))))
      (setq header (concat header "]"))
      ;; start & end date
      (setq header (concat header (format-time-string "{%Y-%m-%d}" org-gantt-start-date)))
      (setq header (concat header (format-time-string "{%Y-%m-%d}\n" org-gantt-end-date)))
      ;; gantt title
      (if (plist-get params :compress)
          (setq header (concat header "\\gantttitlecalendar{" org-gantt-compressed-title "}"))
        (setq header (concat header "\\gantttitlecalendar{" org-gantt-title "}")))
      (setq footer (concat (concat "\\end{ganttchart}\n" footer)))
      (setq footer (substring footer 0 -1))		; remove the last \n
      (setq body (concat header body "\n" footer))
      (if (plist-get params :file)
          (progn
            (org-babel-execute:latex body
                                     (org-babel-merge-params (org-gantt-plist-to-alist (append params (list :fit t :headers "\\usepackage{pgfgantt}\n")))))
            (insert (org-babel-result-to-file (plist-get params :file)))
            (org-redisplay-inline-images))
        (insert body)))))

(defun org-insert-dblock:org-gantt-chart ()
  "Insert org-gantt dynamic block."
  (interactive)
  (org-create-dblock
   (list :name "org-gantt"
         :file "gantt.jpg"
         :imagemagick t
         :tikz-scale: "3.0"
         :weekend-style "{draw=blue!10, line width=1pt}"
         :workday-style "{draw=blue!5, line width=.75pt}"
         :show-progress 'if-value
         :progress-source 'cookie-clocksum
         :no-date-headlines 'inactive
         :parameters "y unit title=.7cm, y unit chart=.9cm"
         :tags-group-style '(("test"."group label font=\\color{blue}")
                             ("toast"."group label font=\\color{green}"))
         :tags-bar-style '(("test"."bar label font=\\color{blue}")
                           ("toast"."bar label font=\\color{green}")))))

;;;;###autoload
;(org-dynamic-block-define "gantt" 'org-insert-dblock:org-gantt-chart)

(provide 'org-gantt)

;;; org-gantt.el ends here
