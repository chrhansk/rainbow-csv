;;; rainbow-csv-mode.el --- A lightweight major mode for editing CSV files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Christoph Hansknecht

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Christoph Hansknecht
;; Assisted-by: Claude Sonnet 5
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, data
;; URL: https://example.com/rainbow-csv-mode

;; rainbow-csv-mode is a small, dependency-free major mode for viewing and
;; editing delimiter-separated files (CSV, TSV, semicolon-separated, etc).
;;
;; Features:
;;  - Auto-detects the delimiter used in the buffer (comma, semicolon, tab)
;;  - Syntax highlighting for delimiters and quoted fields
;;  - Rainbow columns: each column (including the header row) is colored
;;    using a rotating palette, so it's easy to visually track a column
;;    down a long file (C-c C-r toggles it on/off)
;;  - Sticky header: the first row stays pinned to the top of the window
;;    as you scroll down (C-c C-t toggles it on/off)
;;  - Field-wise navigation (C-c C-f / C-c C-b)
;;  - Toggleable column alignment for readability (C-c C-a)
;;  - Sort all data rows by a chosen column (C-c C-s), or by whichever
;;    column point is currently in (C-c C-d)
;;
;; Installation:
;;   (require 'rainbow-csv-mode)
;;   ;; Files ending in .csv, .tsv will use this mode automatically.
;;

(require 'seq)
(require 'sort)
(require 'cl-lib)
(require 'subr-x)
(require 'face-remap)

(defgroup rainbow-csv nil
  "A lightweight major mode for CSV/TSV files."
  :group 'convenience
  :prefix "rainbow-csv-")

(defcustom rainbow-csv-default-separator ","
  "Fallback field separator used when auto-detection is inconclusive."
  :type 'string
  :group 'rainbow-csv)

(defcustom rainbow-csv-has-header t
  "Non-nil if buffers are assumed to have a header row on line 1.
This affects highlighting and sorting (the header line is skipped)."
  :type 'boolean
  :group 'rainbow-csv)

(defcustom rainbow-csv-rainbow-columns t
  "Non-nil to color each column with a rotating face.
The palette cycles through `rainbow-csv-column-faces'. The header row,
if present, is colored the same way as the data rows so column
colors stay consistent all the way down the file."
  :type 'boolean
  :group 'rainbow-csv)

(defcustom rainbow-csv-column-faces
  '(rainbow-csv-column-face-1
    rainbow-csv-column-face-2
    rainbow-csv-column-face-3
    rainbow-csv-column-face-4
    rainbow-csv-column-face-5
    rainbow-csv-column-face-6
    rainbow-csv-column-face-7)
  "Faces cycled through, left to right, to color successive columns.
Customize this to change the palette or its length; columns beyond
the end of the list wrap back around to the first face."
  :type '(repeat face)
  :group 'rainbow-csv)

(defcustom rainbow-csv-sticky-header t
  "Non-nil to keep the header row pinned to the top of the window.
Uses Emacs' `header-line-format' facility, so the header stays
visible even after scrolling down, independent of point. Only takes
effect when `rainbow-csv-has-header' is also non-nil; toggle at runtime
with `rainbow-csv-toggle-sticky-header'."
  :type 'boolean
  :group 'rainbow-csv)

;; Default rainbow palette. Chosen to be distinguishable on both light and
;; dark backgrounds; override individual faces or the whole
;; `rainbow-csv-column-faces' list to taste.

(defface rainbow-csv-column-face-1
  '((((background dark)) :foreground "#e06c75")
    (t :foreground "#af0000"))
  "Face for columns where (column-index mod 7) = 0."
  :group 'rainbow-csv)

(defface rainbow-csv-column-face-2
  '((((background dark)) :foreground "#d19a66")
    (t :foreground "#af5f00"))
  "Face for columns where (column-index mod 7) = 1."
  :group 'rainbow-csv)

(defface rainbow-csv-column-face-3
  '((((background dark)) :foreground "#e5c07b")
    (t :foreground "#875f00"))
  "Face for columns where (column-index mod 7) = 2."
  :group 'rainbow-csv)

(defface rainbow-csv-column-face-4
  '((((background dark)) :foreground "#98c379")
    (t :foreground "#005f00"))
  "Face for columns where (column-index mod 7) = 3."
  :group 'rainbow-csv)

(defface rainbow-csv-column-face-5
  '((((background dark)) :foreground "#56b6c2")
    (t :foreground "#005f5f"))
  "Face for columns where (column-index mod 7) = 4."
  :group 'rainbow-csv)

(defface rainbow-csv-column-face-6
  '((((background dark)) :foreground "#61afef")
    (t :foreground "#00005f"))
  "Face for columns where (column-index mod 7) = 5."
  :group 'rainbow-csv)

(defface rainbow-csv-column-face-7
  '((((background dark)) :foreground "#c678dd")
    (t :foreground "#5f00af"))
  "Face for columns where (column-index mod 7) = 6."
  :group 'rainbow-csv)

(defvar-local rainbow-csv--separator nil
  "The delimiter character in use for the current buffer, as a string.")

(defvar-local rainbow-csv--aligned nil
  "Non-nil if the buffer is currently showing aligned (padded) columns.")

(defvar-local rainbow-csv--unaligned-contents nil
  "Snapshot of buffer contents before alignment, used to undo it cleanly.")

;;; Delimiter detection

(defun rainbow-csv--detect-separator ()
  "Guess the field separator from the first line of the buffer.
Checks comma, semicolon, and tab, in that order of preference,
picking whichever appears most often. Falls back to
`rainbow-csv-default-separator'."
  (save-excursion
    (goto-char (point-min))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))
          (candidates '("," ";" "\t")))
      (car (or (seq-reduce
                (lambda (best sep)
                  (let ((count (cl-count (string-to-char sep) line)))
                    (if (and (> count 0)
                             (or (null best) (> count (cdr best))))
                        (cons sep count)
                      best)))
                candidates nil)
               (cons rainbow-csv-default-separator 0))))))

(defun rainbow-csv--separator-regexp ()
  "Return a regexp matching the current buffer's separator."
  (regexp-quote rainbow-csv--separator))

;;; Field navigation

(defun rainbow-csv--field-bounds-at-point ()
  "Return (START . END) of the field at point, excluding delimiters."
  (let ((sep (rainbow-csv--separator-regexp))
        (bol (line-beginning-position))
        (eol (line-end-position)))
    (save-excursion
      (let ((start (progn
                     (if (re-search-backward sep bol t)
                         (match-end 0)
                       bol)))
            (end (progn
                   (goto-char (point))
                   (if (re-search-forward sep eol t)
                       (match-beginning 0)
                     eol))))
        (cons start end)))))

(defun rainbow-csv-forward-field (&optional n)
  "Move point forward across N fields (default 1) on the current line.
Moves to the next line if at the end of the current one."
  (interactive "p")
  (setq n (or n 1))
  (let ((sep (rainbow-csv--separator-regexp)))
    (dotimes (_ n)
      (cond
       ((re-search-forward sep (line-end-position) t))
       ((< (line-end-position) (point-max))
        (forward-line 1)
        (beginning-of-line))
       (t (end-of-line))))))

(defun rainbow-csv-backward-field (&optional n)
  "Move point backward across N fields (default 1) on the current line.
Moves to the previous line if at the start of the current one."
  (interactive "p")
  (setq n (or n 1))
  (let ((sep (rainbow-csv--separator-regexp)))
    (dotimes (_ n)
      (cond
       ((re-search-backward sep (line-beginning-position) t))
       ((> (line-beginning-position) (point-min))
        (forward-line -1)
        (end-of-line))
       (t (beginning-of-line))))))

(defun rainbow-csv-tab-command ()
  "Align the buffer if needed, then move to the next field."
  (interactive)
  (rainbow-csv-forward-field 1))

;;; Row parsing helpers (naive: does not handle embedded delimiters in quotes
;;; across multiple lines, which is rare enough for a lightweight mode)

(defun rainbow-csv--split-line (line)
  "Split LINE into a list of fields using the buffer's separator."
  (split-string line (rainbow-csv--separator-regexp)))

(defun rainbow-csv--data-line-start ()
  "Return the line number (1-indexed) where data rows begin."
  (if rainbow-csv-has-header 2 1))

;;; Column alignment

(defun rainbow-csv-align-fields ()
  "Toggle padded column alignment for the whole buffer.
Calling this a second time restores the original, unpadded text."
  (interactive)
  (if rainbow-csv--aligned
      (rainbow-csv--unalign)
    (rainbow-csv--align)))

(defun rainbow-csv--align ()
  "Pad every field so columns line up visually."
  (setq rainbow-csv--unaligned-contents (buffer-string))
  (let* ((lines (split-string (buffer-string) "\n"))
         (rows (mapcar #'rainbow-csv--split-line lines))
         (col-count (apply #'max 0 (mapcar #'length rows)))
         (widths (make-vector col-count 0)))
    (dolist (row rows)
      (cl-loop for field in row
               for i from 0
               do (aset widths i (max (aref widths i) (length field)))))
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (dolist (row rows)
        (insert
         (mapconcat
          (lambda (pair)
            (let ((field (car pair)) (width (cdr pair)))
              (format (format "%%-%ds" width) field)))
          (cl-loop for field in row
                   for i from 0
                   collect (cons field (aref widths i)))
          (concat " " rainbow-csv--separator " ")))
        (insert "\n"))
      (goto-char (min pos (point-max))))
    (setq rainbow-csv--aligned t)
    (message "Fields aligned. Run the command again to restore original text.")))

(defun rainbow-csv--unalign ()
  "Restore buffer contents saved before `rainbow-csv--align'."
  (when rainbow-csv--unaligned-contents
    (let ((inhibit-read-only t)
          (pos (point)))
      (erase-buffer)
      (insert rainbow-csv--unaligned-contents)
      (goto-char (min pos (point-max)))))
  (setq rainbow-csv--aligned nil)
  (message "Alignment removed."))

;;; Sorting

(defun rainbow-csv--sort-by-column (column)
  "Sort data rows by field number COLUMN (1-indexed), leaving the header in place.
Non-interactive worker function; see `rainbow-csv-sort-by-column' and
`rainbow-csv-sort-dwim' for the interactive entry points."
  (when rainbow-csv--aligned
    (user-error "Un-align the buffer first (C-c C-a) before sorting"))
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (forward-line (1- (rainbow-csv--data-line-start)))
      ;; Narrow to just the data rows (excluding the header) before
      ;; calling sort-subr, the same way `sort-lines' narrows to its
      ;; region first — sort-subr processes records starting from
      ;; point, so leaving point at (point-max), as a previous version
      ;; of this function did right before calling sort-subr, gave it
      ;; nothing left to sort at all.
      (narrow-to-region (point) (point-max))
      (goto-char (point-min))
      (let (sort-fold-case)
        (sort-subr nil
                   #'forward-line
                   #'end-of-line
                   (lambda ()
                     (let* ((line (buffer-substring-no-properties
                                   (line-beginning-position) (line-end-position)))
                            (fields (rainbow-csv--split-line line)))
                       (or (nth (1- column) fields) "")))
                   nil
                   #'string<)))))

(defun rainbow-csv-sort-by-column (column)
  "Sort data rows by field number COLUMN (1-indexed), leaving the header in place."
  (interactive "nSort by column number: ")
  (rainbow-csv--sort-by-column column))

(defun rainbow-csv-sort-dwim ()
  "Sort data rows by whichever column point is currently in.
A do-what-I-mean wrapper around `rainbow-csv--sort-by-column': no
column number to look up or type in, just place point in the field
you want to sort by and run this."
  (interactive)
  (rainbow-csv--sort-by-column (1+ (rainbow-csv--column-index-at (point)))))

;;; Sticky header line

(defvar-local rainbow-csv--header-line-shown nil
  "Non-nil while the sticky header-line is currently displayed.")

(defvar-local rainbow-csv--header-line-face-remap nil
  "Cookie from `face-remap-add-relative', used to undo the remap on exit.")

(defun rainbow-csv--header-line-string ()
  "Build a propertized copy of the buffer's first line for the header-line.
Colors each field with the same rotating palette used for the body.
This colors the line's own text in place rather than rebuilding it
field by field, so no extra spacing is introduced around the
separator.

A leading zero-width spacer anchors the text to column 0 of the
window's text area via `:align-to', which by definition excludes
fringes, margins, and line-number display — rather than trying to
reproduce that width by hand (which previously made things worse),
this asks Emacs to compute the correct starting position directly."
  (let ((anchor (propertize " " 'display '(space :align-to 0))))
    (save-excursion
      (goto-char (point-min))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (if (string-empty-p line)
            anchor
          (let* ((sep-re (rainbow-csv--separator-regexp))
                 (faces rainbow-csv-column-faces)
                 (nfaces (length faces))
                 (result (copy-sequence line))
                 (len (length result))
                 (pos 0)
                 (idx 0))
            (while (<= pos len)
              (if (string-match sep-re result pos)
                  (let ((sep-start (match-beginning 0))
                        (sep-end (match-end 0)))
                    (when (> sep-start pos)
                      (put-text-property
                       pos sep-start 'face
                       (if (> nfaces 0) (nth (mod idx nfaces) faces) 'default)
                       result))
                    (put-text-property sep-start sep-end 'face
                                       'font-lock-comment-face result)
                    (setq idx (1+ idx))
                    (setq pos sep-end))
                (when (< pos len)
                  (put-text-property
                   pos len 'face
                   (if (> nfaces 0) (nth (mod idx nfaces) faces) 'default)
                   result))
                (setq pos (1+ len))))
            (concat anchor result)))))))

(defun rainbow-csv--refresh-header-line (&rest _ignore)
  "Show or hide the sticky header-line bar based on scroll position.
Rather than merely blanking its text, this removes the header-line
area entirely while the buffer's real first line is still visible in
the window, so it never appears duplicated near the top."
  (when (derived-mode-p 'rainbow-csv-mode)
    (let ((should-show (and rainbow-csv-has-header
                            rainbow-csv-sticky-header
                            (not (pos-visible-in-window-p
                                  (point-min) (selected-window))))))
      (unless (eq should-show rainbow-csv--header-line-shown)
        (setq rainbow-csv--header-line-shown should-show)
        (setq header-line-format
              (when should-show '(:eval (rainbow-csv--header-line-string))))
        (force-mode-line-update)))))

(defun rainbow-csv--update-header-line ()
  "(Re)install the hooks that drive the sticky header-line, if enabled.
Actual showing/hiding happens in `rainbow-csv--refresh-header-line',
called on scroll and after each command, so the header-line area
appears only once the real first line has scrolled out of view.
Also remaps the `header-line' face's font metrics (family/height) to
match the buffer's own font, since header-lines otherwise often use
a different typeface that makes identical text look more spaced out."
  (setq rainbow-csv--header-line-shown nil)
  (setq header-line-format nil)
  (remove-hook 'window-scroll-functions #'rainbow-csv--refresh-header-line t)
  (remove-hook 'post-command-hook #'rainbow-csv--refresh-header-line t)
  (when rainbow-csv--header-line-face-remap
    (face-remap-remove-relative rainbow-csv--header-line-face-remap)
    (setq rainbow-csv--header-line-face-remap nil))
  (when (and rainbow-csv-has-header rainbow-csv-sticky-header)
    (setq rainbow-csv--header-line-face-remap
          (face-remap-add-relative
           'header-line
           :family (face-attribute 'default :family)
           :height (face-attribute 'default :height)
           :box nil
           :underline nil
           :overline nil))
    (add-hook 'window-scroll-functions #'rainbow-csv--refresh-header-line nil t)
    (add-hook 'post-command-hook #'rainbow-csv--refresh-header-line nil t)
    (rainbow-csv--refresh-header-line)))

(defun rainbow-csv-toggle-sticky-header ()
  "Toggle whether the header row stays pinned to the top of the window."
  (interactive)
  (setq-local rainbow-csv-sticky-header (not rainbow-csv-sticky-header))
  (rainbow-csv--update-header-line)
  (force-mode-line-update)
  (message "Sticky header %s." (if rainbow-csv-sticky-header "enabled" "disabled")))

;;; Font lock: rainbow columns

(defun rainbow-csv--column-index-at (pos)
  "Return the 0-indexed column number of the field containing POS.
Counts how many separators precede POS on its own line."
  (save-excursion
    (let ((bol (progn (goto-char pos) (line-beginning-position)))
          (sep-re (rainbow-csv--separator-regexp))
          (count 0))
      (goto-char bol)
      (while (re-search-forward sep-re pos t)
        (setq count (1+ count)))
      count)))

(defun rainbow-csv--face-for-match ()
  "Return the face to use for the field last matched by the rainbow matcher.
The header row (if any) is colored with the same rotating palette as
the data rows, so column colors are consistent all the way down."
  (let* ((faces rainbow-csv-column-faces)
         (n (length faces))
         (idx (rainbow-csv--column-index-at (match-beginning 0))))
    (if (> n 0) (nth (mod idx n) faces) 'default)))

(defun rainbow-csv--font-lock-field-matcher (limit)
  "Font-lock MATCHER that walks fields line by line up to LIMIT.
Sets match data to the bounds of the next non-empty field (excluding
its surrounding separators) and moves point past it, so repeated
calls sweep the whole fontification region. Returns nil once LIMIT
is reached."
  (let (found)
    (while (and (not found) (< (point) limit))
      (cond
       ;; At or past the end of the current line: advance to the next one,
       ;; or stop if there is nothing left within LIMIT.
       ((>= (point) (line-end-position))
        (if (< (point) (point-max))
            (forward-line 1)
          (goto-char limit)))
       (t
        (let* ((sep-re (rainbow-csv--separator-regexp))
               (eol (min (line-end-position) limit))
               (start (point)))
          (if (re-search-forward sep-re eol t)
              (let ((end (match-beginning 0)))
                (if (> end start)
                    (progn (set-match-data (list start end)) (setq found t))
                  nil)) ; empty field; loop continues past the separator
            (goto-char eol)
            (when (> eol start)
              (set-match-data (list start eol))
              (setq found t)))))))
    found))

(defconst rainbow-csv--rainbow-keyword
  '(rainbow-csv--font-lock-field-matcher (0 (rainbow-csv--face-for-match) nil))
  "The font-lock rule that colors fields by rotating column.
Kept as its own keyword, separate from the base keywords, so it can
be added/removed at runtime with `font-lock-add-keywords' and
`font-lock-remove-keywords' without disturbing anything else.")

(defun rainbow-csv--font-lock-keywords ()
  "Build the base font-lock keywords using the buffer's detected separator.
Does not include the rainbow-column rule; that is managed separately
via `font-lock-add-keywords'/`font-lock-remove-keywords' so it can be
toggled at runtime (`font-lock-defaults' is only consulted once, the
first time font-lock-mode turns on in a buffer, so rebuilding this
list later has no effect on its own)."
  `((,(rainbow-csv--separator-regexp) . font-lock-comment-face)
    ("\"[^\"]*\"" 0 font-lock-string-face t)))

(defun rainbow-csv-toggle-rainbow-columns ()
  "Toggle rotating column colors on or off in the current buffer."
  (interactive)
  (setq-local rainbow-csv-rainbow-columns (not rainbow-csv-rainbow-columns))
  (if rainbow-csv-rainbow-columns
      (font-lock-add-keywords nil (list rainbow-csv--rainbow-keyword))
    (font-lock-remove-keywords nil (list rainbow-csv--rainbow-keyword)))
  (font-lock-flush)
  (font-lock-ensure)
  (message "Rainbow columns %s." (if rainbow-csv-rainbow-columns "enabled" "disabled")))

;;; Keymap

(defvar rainbow-csv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'rainbow-csv-forward-field)
    (define-key map (kbd "C-c C-b") #'rainbow-csv-backward-field)
    (define-key map (kbd "C-c C-a") #'rainbow-csv-align-fields)
    (define-key map (kbd "C-c C-s") #'rainbow-csv-sort-by-column)
    (define-key map (kbd "C-c C-d") #'rainbow-csv-sort-dwim)
    (define-key map (kbd "C-c C-r") #'rainbow-csv-toggle-rainbow-columns)
    (define-key map (kbd "C-c C-t") #'rainbow-csv-toggle-sticky-header)
    (define-key map (kbd "TAB") #'rainbow-csv-tab-command)
    map)
  "Keymap for `rainbow-csv-mode'.")

;;; Mode definition

;;;###autoload
(define-derived-mode rainbow-csv-mode text-mode "Rainbow-CSV"
  "A lightweight major mode for editing CSV/TSV files.

\\{rainbow-csv-mode-map}"
  (setq rainbow-csv--separator (rainbow-csv--detect-separator))
  (setq-local font-lock-defaults (list (rainbow-csv--font-lock-keywords)))
  (setq-local truncate-lines t)
  (rainbow-csv--update-header-line)
  (font-lock-mode 1)
  (when rainbow-csv-rainbow-columns
    (font-lock-add-keywords nil (list rainbow-csv--rainbow-keyword))
    (font-lock-flush)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.csv\\'" . rainbow-csv-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsv\\'" . rainbow-csv-mode))

(provide 'rainbow-csv-mode)

;;; rainbow-csv-mode.el ends here
