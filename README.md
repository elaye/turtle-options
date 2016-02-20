Turtle options
==============

This package provides additional command line options for [Turtle](https://hackage.haskell.org/package/turtle-1.2.5/docs/Turtle-Tutorial.html).

Percentage
----------

Parse a percentage (`20%`). The result is a floating point number (`Float`), corresponding to the given percentage divided by 100.

Scale
-----

Parse a scaling option in different ways. 
You can specify a size (`480x320`), a width (`480x`) or a height (`x320`) or a percentage (`50%` or `0.5`, needs to be positive).

Quality
-------

Parse a quality option. This can be a percentage or a keyword (`verylow`, `low`, `mediumlow`, `medium`, `mediumhigh`, `high`, `best`). The keywords are mapped to a percentage according to the following table:

|Keyword    |Percentage|
| --------- | -------- |
|verylow    |       10%|
|low        |       20%|
|mediumlow  |       35%|
|medium     |       50%|
|mediumhigh |       65%|
|high       |       80%|
|best       |      100%|