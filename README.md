# org-gantt
An emacs package to create gantt chart using pgfgantt LaTeX-package on org-mode.

Basic implementation is came from https://github.com/swillner/org-gantt.

## Customize Variables
* `org-gantt-weekstart` (number)

  Day number of start of week. 0 is Sunday and 6 is Saturday.
  Default is Sunday ("0").

* `org-gantt-weekstart-style` (string)

  Style of line for start of week.
  Default is black line ("{black}").

* `org-gantt-weekday-style` (string)

  Style of line for except start of week.
  Default is dashed line ("{dashed}").

* `org-gantt-weekend` (numbers in list)

  Day numbers for weekend. 0 is Sunday and 6 is Saturday.
  Default is Saturday and Sunday ("0 6").

* `org-gantt-holiday-list` (string in list)

  List of holiday in string. String format is "YYYY-MM-DD".
  Default is "2020-01-01".

* `org-gantt-holiday-vrule` (string)

  Style of holidays include weekend.
  Default is "red!15".

* `org-gantt-title` (string)

  Header format for title.
  Default is "year, month=name, day".

* `org-gantt-comp-title` (string)

  Header format for compressed title.
  Default is "year, month".

## Properties in org file

* `ID` (string)

  Identifier for current headline.
  It should be unique in org file.
  If this is not defined, `org-gantt` generates temporary id.
  It is used for `LINKED-TO` property.

* `HIDE` (t or nil)

  This is used for hiding current headline.

* `LINKED-TO` (ides in list)

  Headlines which dependant on current headline.
  This can include multiple ides.

* `scheduled` (time stamp)

  Start date of current headline.
  `org-gantt` only takes date and ignores time.

* `deadline` (time stamp)

  End date of current headline.
  `org-gantt` only takes date and ignores time.

* `EFFORT` (time duration)

  Duration of current headline requires.
  `org-gantt` ignores time smaller than a day.
  It can usg following keyword. (`d` day, `w` week, `m` month)

* `tags`

  * `milestone`

    Current headline will be milestone.
    It does not occupy a line and only occupies a day.

## Properties in org-gantt-chart

* `id`

  If this property is defined, the gantt chart displays only this headline including its children.
  If this property is not defined, entire headline will be shown in the gantt chart.

* `today`

  This property shows `today` mark.
  If this property is set as `t`, `org-gantt` shows `today` mark in today.
  If this property is set as `YYYY-MM-DD`, `org-gantt` shows `today` mark in `YYYY-MM-DD`.

* `start-date`

  This property limits start date of the gantt chart.
  If this property is not set, `org-gantt` set this property as the earliest date in the headlines.

* `end-date`

  This property limits end date of the gantt chart.
  If this property is not set, `org-gantt` set this property as the latest date in the headlines.

* `tikz-scale`

  Scale factor for the output image.

* `tikz-options`

  Options fot `tikz`.
  `org-gantt` bypasses this property to `tikz`.

* `compress`

  If this property is defined, compressed style of title is shown.
  Otherwise normal style of title is shown.

* `parameters`

  Option for `ganttchart`.
  `org-gantt` bypasses this property to `ganttchart`.

* `file`
  This property defines output format.
  If this property is set as an image file, `org-gantt` generates an image file and displays in org file.
  If this property is not set, latex string is updated in org file.
  And it can be converted to pdf file later.

## How to calculate start and end date in the gantt chart

### Type of Headline

* Parent

Parent is the headline without any tags and includes other headlines.
Its `scheduled`, `deadline` and `EFFORT` are ignored.
Its start and end date are calculated with following rules.

* Children

Child is the headline without any tags and does not include other headline.
Its `scheduled`, `deadline` and `EFFORT` are optional.
If these are defined, `org-gantt` will use them without any change.
If these are missing, `org-gantt` will calculate them with following rules.

* Milestone

Milestone is the headline which has `milestone` in its tags.
It should have `scheduled` and `deadline` with same date.
`org-gantt` will not change its start and end date.

### Rules of Calculation

* Parent's start and end date will be minimum period which includes all period of its children.
* Child's start date will be next day of the latest end date of headlines which point to this headline.
* Child's end date will be previous day of the eariest start date of headline which this headline point to.
* If start date is not working day, it will be previous date.
* If end date is not working day, it will be next date.
