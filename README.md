Turtle options
==============

[![Build Status](https://travis-ci.org/elaye/turtle-options.svg?branch=master)](https://travis-ci.org/elaye/turtle-options)

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
|veryhigh   |       90%|
|best       |      100%|

Timecode
--------

Parse a timecode. 
A timecode is made of a number of hours, minutes, seconds and milliseconds. 
The time code can be given in different formats.
You don't have to give a number of seconds or minutes inferior to 60. For example if you give 75 minutes, it will be interpreted as 1 hour and 15 minutes. 
You can also provide a number of milliseconds superior to 1000.
The only required number is the number of seconds. 
The following table gives examples of valid timecodes and how they are interpreted:

| Timecode   | Result                               |
| ---------- | ------------------------------------ |
| 3          | 3 secs                               |
| 75         | 1 min 15 secs                        |
| 17:12      | 17 mins 12 secs                      |
| 80:23      | 1 hour 20 mins 23 secs               |
| 54:32:10   | 54 hours 32 mins 10 secs             |
| 43.7       | 43 secs 700 millisecs                |
| 4:13.85    | 4 mins 13 secs 850 millisecs         |
| 7:4:13.437 | 7 hours 4 mins 13 secs 437 millisecs |
| 5.2150     | 7 secs 150 milliseconds              |

You can also use the 00h00m00s000 format if you prefer. The same rules apply:

    1h34m12s345 gives 1 hour 34 mins 12 secs and 345 millisecs

A timecode can be negative: 

    -3:45 (or -3m45) gives minus 3 mins and 45 secs

