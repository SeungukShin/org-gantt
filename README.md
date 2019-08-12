# org-gantt
An emacs package to create gantt chart using pgfgantt LaTeX-package on org-mode.

This is reimplementation of https://github.com/swillner/org-gantt to modify calculation of effort.

Currently only following features are supported.
* Set weekend using custom variable.
  * The style of weekday and weekend cannot be defined.
  * Weekend cannot be modified by parameter
* `:id` parameter
* `:compressed` parameter
* `:today` parameter
* `:parameters` parameter
* `:file` parameter
* `:LINKED-TO:` property always works even if the target has `SCHEDULED` time stamp.
* `:LINKED-TO:` property can point multiple headlines using separator with white space.

