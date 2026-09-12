# rainbow-csv-mode

rainbow-csv-mode is a small, dependency-free major mode for viewing and
editing delimiter-separated files (CSV, TSV, semicolon-separated, etc).

## Features
 - Auto-detects the delimiter used in the buffer (comma, semicolon, tab)
 - Syntax highlighting for delimiters and quoted fields
 - Rainbow columns: each column (including the header row) is colored
   using a rotating palette, so it's easy to visually track a column
   down a long file (C-c C-r toggles it on/off)
 - Sticky header: the first row stays pinned to the top of the window
   as you scroll down (C-c C-t toggles it on/off)
 - Field-wise navigation (C-c C-f / C-c C-b)
 - Toggleable column alignment for readability (C-c C-a)
 - Sort all data rows by a chosen column (C-c C-s), or by whichever
   column point is currently in (C-c C-d)

## Installation

```cl
(require 'rainbow-csv-mode)
;; Files ending in .csv, .tsv will use this mode automatically.
```
