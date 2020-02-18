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

(defcustom org-gantt-weekstart 0
  "Start of week.
0 is Sunday and 6 is Saturday"
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-weekstart-style "{black}"
  "The style for the weekend lines."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-weekday-style "{dashed}"
  "The style for the weekday lines."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-comp-weekday-style "{draw=none}"
  "The style for the weekday lines for the compressed calendar."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-weekend '(0 6)
  "Weekend and holiday.
0 is Sunday and 6 is Saturday."
  :type '(repeat integer)
  :group 'org-gantt)

(defcustom org-gantt-holiday-list '("2020-01-01")
  "The list of holidays."
  :type '(repeat string)
  :group 'org-gantt)

(defcustom org-gantt-holiday-vrule "red!15"
  "The style for the holiday."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-title "year, month=name, day"
  "The style for the title calendar."
  :type '(string)
  :group 'org-gantt)

(defcustom org-gantt-comp-title "year, month"
  "The style for the compressed title calendar."
  :type '(string)
  :group 'org-gantt)

;; target information
(defvar org-gantt-target-id nil
  "Target ID to translate gantt chart.")

(defvar org-gantt-target-level nil
  "Target level to translate gantt chart.")

;; scan status
(defvar org-gantt-scan-status nil
  "Status of scanning org file.")
(defconst org-gantt-target-wait 0
  "Target id is not present yet.")
(defconst org-gantt-target-on 1
  "Target id is present and on it now.")
(defconst org-gantt-target-out 2
  "Target id is present and out of it now.")

;; index of auto id generation
(defvar org-gantt-index-id 0
  "Index of id for headline without id")

;; update reason
(defconst org-gantt-update-none 0
  "Update time of parent only")
(defconst org-gantt-update-from 1
  "Update time of parent and from")
(defconst org-gantt-update-to 2
  "Update time of parent and to")
(defconst org-gantt-update-both 3
  "Update time of parent, from and to")

;; key: id
;; value: id: id
;;        level: level of headline
;;        raw: string for headline
;;        start: start date
;;        start-fix: start date from org file in bool
;;        end: end date
;;        end-fix: end date from org file in bool
;;        effort: effort in day
;;        tags: milestone state in bool
;;        parent: parent id
;;        children: children ides in list
;;        to: linked to ides in list
;;        from: linked from ides in list
(defvar org-gantt-hash-table nil
  "Hash table for properties of headlines")

(defvar org-gantt-root nil
  "Root ides in list.")

;; start and end date for gantt table
(defvar org-gantt-start nil
  "Start date form hash table")
(defvar org-gantt-end nil
  "End date from hash table")

;; information for propagation to update time
(defvar org-gantt-update nil
  "Update state of one scan")
(defvar org-gantt-count 0
  "Update loop count")
(defconst org-gantt-timeout 1000
  "Maximum loop count to update time")

;; type of finding working day
(defconst org-gantt-advance 1
  "Find a working day before start time")
(defconst org-gantt-postpone 2
  "Find a working day after start time")


(defun dbg-msg (format-string &rest args)
  "Print debug message."
  ;(apply #'message format-string args)
  (insert (apply #'format format-string args)))

(defun org-gantt-dump (id)
  (let (prop level raw start start-fix end end-fix effort tags parent children to from)
    (setq prop (gethash id org-gantt-hash-table)
          level (plist-get prop :level)
          raw (plist-get prop :raw)
          start (plist-get prop :start)
          start-fix (plist-get prop :start-fix)
          end (plist-get prop :end)
          end-fix (plist-get prop :end-fix)
          effort (plist-get prop :effort)
          tags (plist-get prop :tags)
          parent (plist-get prop :parent)
          children (plist-get prop :children)
          to (plist-get prop :to)
          from (plist-get prop :from))
    (dbg-msg "%20s (%20s:%d %15s %10s(%3s)~%10s(%3s) %3s)\n" raw id level tags
             (org-gantt-time-to-string start) start-fix
             (org-gantt-time-to-string end) end-fix effort)
    (dbg-msg "  parent: %s\n" parent)
    (dbg-msg "  children: %s\n" children)
    (dbg-msg "  to: %s\n" to)
    (dbg-msg "  from: %s\n" from)))

(defun org-gantt-plist-to-alist (plist)
  "Transform property list PLIST into an association list."
  (cl-loop for p on plist by #'cddr
           collect (cons (car p) (cadr p))))

(defun* org-gantt-timestamp-to-time (timestamp)
  "Change org timestamp to time value
TIMESTAMP is org timestamp"
  (unless timestamp
    (return-from org-gantt-timestamp-to-time))
  (let ((raw (org-element-property :raw-value timestamp)))
    (apply 'encode-time (org-parse-time-string raw))))

(defun* org-gantt-time-to-string (time)
  "Change time value to string
TIME is time value"
  (unless time
    (return-from org-gantt-time-to-string))
  (format-time-string "%Y-%m-%d" time))

(defun* org-gantt-duration-to-day (duration)
  "Change duration string to integer in day
DURATION is a string which represents duration"
  (unless duration
    (return-from org-gantt-duration-to-day))
  (truncate (/ (org-duration-string-to-minutes duration) 60 24)))

(defun time-equal-p (t1 t2)
  (if (or (not t1)
          (not t2))
      nil
    (equal t1 t2)))

(defun org-gantt-insert-holiday (start end)
  "Insert holiday to gantt table
START is start date for gantt table
END is end date for gantt table"
  (let (msg current holiday)
    (setq current start
          msg "")
    ;; insert weekend
    (setq end (time-add end (* 24 60 60)))
    (while (time-less-p current end)
      (if (member (string-to-number (format-time-string "%w" current)) org-gantt-weekend)
          (setq msg (concat msg "  \\ganttvrule{}{" (org-gantt-time-to-string current) "}\n")))
      (setq current (time-add current (* 24 60 60))))
    ;; insert holiday
    (setq start (time-subtract start (* 24 60 60)))
    (dolist (holiday org-gantt-holiday-list)
      (setq current (apply 'encode-time (org-parse-time-string holiday)))
      (if (and (time-less-p start current)
               (time-less-p current end))
          (setq msg (concat msg "  \\ganttvrule{}{" holiday "}\n"))))
    ;; insert header and footer
    (unless (equal msg "")
      (setq msg (concat "\\begin{scope}[on background layer]\n" msg)
            msg (concat msg "\\end{scope}\n")))
    msg))

(defun org-gantt-insert-link (id)
  "Insert gantt link for id to gantt table
ID is an id to insert gantt link"
  (let (prop level raw start start-fix end end-fix effort tags parent children to from i msg)
    (setq prop (gethash id org-gantt-hash-table)
          level (plist-get prop :level)
          raw (plist-get prop :raw)
          start (plist-get prop :start)
          start-fix (plist-get prop :start-fix)
          end (plist-get prop :end)
          end-fix (plist-get prop :end-fix)
          effort (plist-get prop :effort)
          tags (plist-get prop :tags)
          parent (plist-get prop :parent)
          children (plist-get prop :children)
          to (plist-get prop :to)
          from (plist-get prop :from)
          msg "")
    (dolist (i to)
      (setq msg (concat msg "\\ganttlink{" id "}{" i "}\n")))
    msg))

(defun org-gantt-insert-bar (id)
  "Inert gantt bar for id to gantt table
ID is an id to insert gantt bar"
  (let (prop level raw start start-fix end end-fix effort tags parent children to from msg)
    (setq prop (gethash id org-gantt-hash-table)
          level (plist-get prop :level)
          raw (plist-get prop :raw)
          start (plist-get prop :start)
          start-fix (plist-get prop :start-fix)
          end (plist-get prop :end)
          end-fix (plist-get prop :end-fix)
          effort (plist-get prop :effort)
          tags (plist-get prop :tags)
          parent (plist-get prop :parent)
          children (plist-get prop :children)
          to (plist-get prop :to)
          from (plist-get prop :from)
          msg "")
    (setq msg (concat msg (make-string (* 2 (- level 1)) ? )))
    (cond (children
           (setq msg (concat msg "\\ganttgroup")))
          ((and tags (member "milestone" tags))
           (setq msg (concat msg "\\ganttmilestone")))
          (t
           (setq msg (concat msg "\\ganttbar"))))
    (setq msg (concat msg "[name=" id "]"
                      "{" raw "}"
                      "{" (org-gantt-time-to-string start) "}"
                      "{" (org-gantt-time-to-string end) "}"))
    (if (and tags (member "milestone" tags))
        (setq msg (concat msg "\n"))
      (setq msg (concat msg "\\\\\n")))
    msg))

(defun org-gantt-vgrid (start compress)
  "Set style of vgrid
START is start date for gantt chart"
  (let ((start-day (string-to-number (format-time-string "%w" start)))
        (weekday-style nil)
        (before 0)
        (after 0)
        (vgrid "vgrid={"))
    (if compress
        (setq weekday-style org-gantt-comp-weekday-style)
      (setq weekday-style org-gantt-weekday-style))
    (setq before (- org-gantt-weekstart start-day))
    (if (<= before 0)
        (setq before (+ before 6)))
    (setq after (- 6 before))
    (if (/= before 0)
        (setq vgrid (concat vgrid (format "*%d%s," before weekday-style))))
    (setq vgrid (concat vgrid (format "*1%s," org-gantt-weekstart-style)))
    (if (/= after 0)
        (setq vgrid (concat vgrid (format "*%d%s," after weekday-style))))
    (setq vgrid (substring vgrid 0 -1))		; remove the last ,
    (setq vgrid (concat vgrid "}"))
    vgrid))

(defun org-gantt-find-start-end (id)
  "Update start and end time for gantt table.
ID is an id to check start and end time for gantt table"
  (let (prop start end)
    (setq prop (gethash id org-gantt-hash-table)
          start (plist-get prop :start)
          end (plist-get prop :end))
    (if (or (not org-gantt-start)
            (time-less-p start org-gantt-start))
        (setq org-gantt-start start))
    (if (or (not org-gantt-end)
            (time-less-p org-gantt-end end))
        (setq org-gantt-end end))))

(defun org-gantt-find-workingday (time type)
  "Find working day from time based on type
TIME is start time to find working day
TYPE is type to find working day, org-gantt-postpone or org-gantt-advance"
  (while (and time
              (or (member (string-to-number (format-time-string "%w" time)) org-gantt-weekend)
                  (member (org-gantt-time-to-string time) org-gantt-holiday-list)))
    (if (= type org-gantt-postpone)
        (setq time (time-add time (* 24 60 60)))
      (setq time (time-subtract time (* 24 60 60)))))
  time)

(defun org-gantt-find-min-max (nlist)
  "Find minimum and maximum start and end time for the list.
NLIST is a list of ides to find minimum and maximum"
  (let (prop start start-min start-max
             end end-min end-max i)
    (setq start-min nil
          start-max nil
          end-min nil
          end-max nil)
    (dolist (i nlist)
      (setq prop (gethash i org-gantt-hash-table)
            start (plist-get prop :start)
            end (plist-get prop :end))
      (when start
        (if (or (not start-min)
                (time-less-p start start-min))
            (setq start-min start))
        (if (or (not start-max)
                (time-less-p start-max start))
            (setq start-max start)))
      (when end
        (if (or (not end-min)
                (time-less-p end end-min))
            (setq end-min end))
        (if (or (not end-max)
                (time-less-p end-max end))
            (setq end-max end))))
    (list start-min start-max end-min end-max)))

(defun *org-gantt-update-time (id &rest nlist)
  "Update start and end time for id.
ID is an id to update start and end time
NLIST is a list for additional arguments
NLIST-CALLER is an id which calls this function
NLIST-UPDATE is the why this function is called"
  (let (prop start start-fix end end-fix effort parent children to from
             start-min start-max end-min end-max rlist change
             caller update i test-id)
    (setq prop (gethash id org-gantt-hash-table)
          start (plist-get prop :start)
          start-fix (plist-get prop :start-fix)
          end (plist-get prop :end)
          end-fix (plist-get prop :end-fix)
          effort (plist-get prop :effort)
          parent (plist-get prop :parent)
          children (plist-get prop :children)
          to (plist-get prop :to)
          from (plist-get prop :from)
          change nil
          caller (nth 0 nlist)
          update (nth 1 nlist))
    ;; If this is milestone, do not update schedule.
    (if (and tags (member "milestone" tags))
        (return-from org-gantt-update-time))
    ;; If this is parent, ignore its schedule and set schedule including all children's schedule.
    (when children
      (setq rlist (org-gantt-find-min-max children)
            start-min (nth 0 rlist)
            start-max (nth 1 rlist)
            end-min (nth 2 rlist)
            end-max (nth 3 rlist))
      (when (and start-min
                 (or (not start)
                     (not (time-equal-p start-min start))))
        (setq start start-min
              change t))
      (when (and end-max
                 (or (not end)
                     (not (time-equal-p end end-max))))
        (setq end end-max
              change t)))
    ;; If this is child, calculate start and end date.
    (unless children
      ;; own
      (when (and start-fix (not end-fix) effort)
        (setq end (time-add start (* effort 24 60 60))
              end-fix t
              change t))
      (when (and end-fix (not start-fix) effort)
        (setq start (time-subtract end (* effort 24 60 60))
              start-fix t
              change t))
      ;; from
      (setq rlist (org-gantt-find-min-max from)
            start-min (nth 0 rlist)
            start-max (nth 1 rlist)
            end-min (nth 2 rlist)
            end-max (nth 3 rlist))
      (when end-max
        (setq end-max (time-add end-max (* 24 60 60)))
        (when (or (not start)
                  (and (time-less-p start end-max)
                       (not start-fix)))
          (setq start (org-gantt-find-workingday end-max org-gantt-postpone))
          (if (and effort (not end-fix))
              (setq end (org-gantt-find-workingday (time-add start (* effort 24 60 60))
                                                   org-gantt-postpone)))
          (setq change t)))
      ;; to
      (setq rlist (org-gantt-find-min-max to)
            start-min (nth 0 rlist)
            start-max (nth 1 rlist)
            end-min (nth 2 rlist)
            end-max (nth 3 rlist))
      (when start-min
        (setq start-min (time-subtract start-min (* 24 60 60)))
        (when (or (not end)
                  (and (time-less-p start-min end)
                       (not end-fix)))
          (setq end (org-gantt-find-workingday start-min org-gantt-advance))
          (if (and effort (not start-fix))
              (setq start (org-gantt-find-workingday (time-subtract end (* effort 24 60 60))
                                                     org-gantt-advance)))
          (setq change t)))
      )
    (when change
      (setq org-gantt-update t)
      (plist-put prop :start start)
      (plist-put prop :end end)
      (plist-put prop :start-fix start-fix)
      (plist-put prop :end-fix end-fix)
      (puthash id prop org-gantt-hash-table))
    ))

(defun org-gantt-update-from (id)
  "Update from entry for id.
ID is an id to update from entry"
  (let (prop to)
    (setq prop (gethash id org-gantt-hash-table)
          to (plist-get prop :to))
    (let (prop i from)
      (dolist (i to)
        (setq prop (gethash i org-gantt-hash-table)
              from (plist-get prop :from)
              from (append from (list id)))
        (plist-put prop :from from)
        (puthash i prop org-gantt-hash-table)))))

(defun org-gantt-map-hash (nlist func)
  "Map func to hash table from nlist to children.
NLIST is a start list for the hash table
FUNC is a function to call"
  (let (id prop children ret)
    (setq ret nil)
    (dolist (id nlist)
      (setq prop (gethash id org-gantt-hash-table)
            children (plist-get prop :children))
      (setq ret (append ret (list (funcall func id))))
      (if children
          (setq ret (append ret (org-gantt-map-hash children func)))))
    ret))

(defun* org-gantt-init-hash (headline)
  "Initialize hash table from headline. Fill only id, level, headline, child.
HEADLINE is a headline from the org file"
  (unless headline
    (return-from org-gantt-init-hash))
  (let ((id (org-element-property :ID headline))
        (level (org-element-property :level headline))
        (hide (org-element-property :HIDE headline)))
    ;; If hide property is set, skip the headline.
    (if hide
        (return-from org-gantt-init-hash))
    ;; If we are already out of target, skip the headline.
    (if (= org-gantt-scan-status org-gantt-target-out)
        (return-from org-gantt-init-hash))
    ;; If we are waiting for target, check id.
    ;; If id is matched, we are on the target.
    (when (and (= org-gantt-scan-status org-gantt-target-wait)
               (string= id org-gantt-target-id))
      (setq org-gantt-scan-status org-gantt-target-on
            org-gantt-target-level level))
    ;; If we are not on the target, skip the headline.
    (if (/= org-gantt-scan-status org-gantt-target-on)
        (return-from org-gantt-init-hash))
    ;; We are always on the target here.
    ;; If target exists and current level is smaller than target's one,
    ;; we are out of target.
    (when (and org-gantt-target-id
               (not (string= org-gantt-target-id id))
               (>= org-gantt-target-level level))
      (setq org-gantt-scan-status org-gantt-target-out)
      (return-from org-gantt-init-hash))
    ;; The headline should be added to the hash table here.
    ;; Generate id if id property is missing.
    (when (not id)
      (setq org-gantt-index-id (1+ org-gantt-index-id)
            id (format "org-gantt-%d" org-gantt-index-id))
      (org-element-put-property headline :ID id))
    ;; Add id to org-gantt-root, if the headline is root.
    (if (or (= level org-gantt-target-level)
            (= level 1))
        (setq org-gantt-root (append org-gantt-root (list id))))
    ;; Add the headline to the hash table.
    (let ((prop `(,:id, id))
          (raw (org-element-property :raw-value headline))
          (start (org-gantt-timestamp-to-time (org-element-property :scheduled headline)))
          (end (org-gantt-timestamp-to-time (org-element-property :deadline headline)))
          (effort (org-gantt-duration-to-day (org-element-property :EFFORT headline)))
          (tags (org-element-property :tags headline))
          (parent (org-element-property :ID (org-element-property :parent headline)))
          (to (org-element-property :LINKED-TO headline)))
      (plist-put prop :level level)
      (plist-put prop :raw raw)
      (if (not start)
          (plist-put prop :start-fix nil)
        (plist-put prop :start start)
        (plist-put prop :start-fix t))
      (if (not end)
          (plist-put prop :end-fix nil)
        (plist-put prop :end end)
        (plist-put prop :end-fix t))
      (when effort
        (plist-put prop :effort effort))
      (when tags
        (plist-put prop :tags tags))
      (when parent
        (plist-put prop :parent parent)
        ;; Parent should be push to the hash table already.
        ;; Update parent's children
        (let (prop children)
          (setq prop (gethash parent org-gantt-hash-table)
                children (plist-get prop :children)
                children (append children (list id)))
          (plist-put prop :children children)
          (puthash parent prop org-gantt-hash-table)))
      (when to
        ;; We cannot sure linked-to is already in the hash table or not
        ;; Update later
        (plist-put prop :to (split-string to)))
      (puthash id prop org-gantt-hash-table))))

(defun org-dblock-write:org-gantt-chart (params)
  "Update gantt chart code.
PARAMS determine several options of the gantt chart."
  (with-current-buffer (current-buffer)
    (setq org-gantt-hash-table (make-hash-table :test 'equal)
          org-gantt-target-id (plist-get params :id)
          org-gantt-target-level 0
          org-gantt-index-id 0
          org-gantt-root nil
          org-gantt-start nil
          org-gantt-end nil)
    (if org-gantt-target-id
        (setq org-gantt-scan-status org-gantt-target-wait)
      (setq org-gantt-scan-status org-gantt-target-on))
    (let ((org-buffer (org-element-parse-buffer))
          (today (plist-get params :today))
          (start-date (plist-get params :start-date))
          (end-date (plist-get params :end-date))
          (tikz-scale (plist-get params :tikz-scale))
          (tikz-options (plist-get params :tikz-options))
          (compress (plist-get params :compress))
          (parameters (plist-get params :parameters))
          (file (plist-get params :file))
          (parsed-buffer nil)
          (header "")
          (body "")
          (footer "")
          (msg ""))
      ;; parse options
      (if today
          (if (string= today "t")
              (setq today (current-time))
            (setq today (apply 'encode-time (org-parse-time-string today)))))
      (if start-date
          (setq start-date (apply 'encode-time (org-parse-time-string start-date))))
      (if end-date
          (seq end-date (apply 'encode-time (org-parse-time-string end-date))))
      ;; parse the org file
      (org-element-map org-buffer 'headline #'org-gantt-init-hash)
      (org-gantt-map-hash org-gantt-root #'org-gantt-update-from)
      (setq org-gantt-update t
            org-gantt-count org-gantt-timeout)
      (while (and org-gantt-update
                  (/= org-gantt-count 0))
        (setq org-gantt-update nil
              org-gantt-count (1- org-gantt-count))
        (org-gantt-map-hash org-gantt-root #'org-gantt-update-time))
      (if (= org-gantt-count 0)
          (dbg-msg "\ntimeout\n"))
      (org-gantt-map-hash org-gantt-root #'org-gantt-find-start-end)
      (if (not start-date)
          (setq start-date org-gantt-start)
        (setq org-gantt-start start-date))
      (if (not end-date)
          (setq end-date org-gantt-end)
        (setq org-gantt-end end-date))
      ;; set header and footer - tikzpicture
      (when (or tikz-scale tikz-options)
        (setq header (concat header "\\begin{tikzpicture}["))
        (if tikz-scale
            (setq header (concat header "scale=" tikz-scale
                                 ", every node/.style={scale=" tikz-scale "}")))
        (if tikz-options
            (setq header (concat header ", " tikz-options)))
        (setq header (concat header "]\n"))
        (setq footer (concat "\n\\end{tikzpicture}" footer)))
      ;; set header and footer - ganttchart
      (setq header (concat header "\\begin{ganttchart}[inline"
                           ", time slot format=isodate"
                           ", canvas/.append style={fill=none}"
                           ", " (org-gantt-vgrid org-gantt-start compress)
                           ", vrule offset=.5"
                           ", vrule/.style={draw=" org-gantt-holiday-vrule
                           ", line width=\\ganttvalueof{x unit}"))
      (if tikz-scale
          (setq header (concat header "*" tikz-scale)))
      (setq header (concat header "}"))
      (if compress
          (setq header (concat header ", compress calendar")))
      (if today
          (setq header (concat header ", today=" (org-gantt-time-to-string today))))
      (if parameters
          (setq header (concat header ", " parameters)))
      (setq header (concat header "]"))
      (setq header (concat header "{" (org-gantt-time-to-string org-gantt-start) "}"
                           "{" (org-gantt-time-to-string org-gantt-end) "}\n"))
      (setq footer (concat "\\end{ganttchart}" footer))
      ;; set header and footer - gantttitle
      (if compress
          (setq header (concat header "\\gantttitlecalendar{" org-gantt-comp-title "}\\\\\n"))
        (setq header (concat header "\\gantttitlecalendar{" org-gantt-title "}\\\\\n")))
      ;; set body - bar
      (dolist (msg (org-gantt-map-hash org-gantt-root #'org-gantt-insert-bar))
        (setq body (concat body msg)))
      (setq body (substring body 0 -3))		; remove the last \\\\\n
      (setq body (concat body "\n"))
      ;; set body - link
      (dolist (msg (org-gantt-map-hash org-gantt-root #'org-gantt-insert-link))
        (setq body (concat body msg)))
      ;; set body - holiday
      (setq body (concat body (org-gantt-insert-holiday org-gantt-start org-gantt-end)))

;      (dbg-msg "\norg-gantt-dump %s-%s\n" (org-gantt-time-to-string org-gantt-start)
;               (org-gantt-time-to-string org-gantt-end))
;      (org-gantt-map-hash org-gantt-root #'org-gantt-dump)

      ;; insert gantt chart
      (setq body (concat header body footer))
      (if file
          (progn
            (org-babel-execute:latex
             body (org-babel-merge-params
                   (org-gantt-plist-to-alist
                    (append params
                            (list :fit t :headers "\\usepackage{kotex}\n\\usepackage{pgfgantt}\n")))))
            (insert (org-babel-result-to-file file))
            (org-redisplay-inline-images))
        (insert body))
      ) ;let
    ) ;with-current-buffer
  )


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

(require 'ert)

(ert-deftest org-gantt-tc-duration-to-day ()
  (let ((s "2w")
        (d nil))
    (setq d (org-gantt-duration-to-day s))
    (should (equal d 14))))
