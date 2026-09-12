;;; rainbow-csv-mode-tests.el --- ERT tests for rainbow-csv-mode -*- lexical-binding: t; -*-

;; These tests focus on rainbow-csv-mode's column-coloring feature: that
;; font-lock actually assigns the right rotating face to each field, that
;; separators and quoted fields are handled correctly, and that the
;; rainbow-columns toggle actually adds/removes the coloring.
;;
;; Run from the command line with:
;;   emacs -Q --batch -L . -l rainbow-csv-mode.el -l rainbow-csv-mode-tests.el \
;;         -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'rainbow-csv-mode)

(defun rainbow-csv-tests--face-at-string (buffer-text search-string &optional occurrence)
  "Insert BUFFER-TEXT into a temp buffer in `rainbow-csv-mode', font-lock
it, and return the `face' property at the start of the OCCURRENCE-th
match (default 1st) of SEARCH-STRING."
  (with-temp-buffer
    (insert buffer-text)
    (rainbow-csv-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (dotimes (_ (1- (or occurrence 1)))
      (search-forward search-string))
    (search-forward search-string)
    (get-text-property (match-beginning 0) 'face)))

(defun rainbow-csv-tests--nth-face (n)
  "Return the Nth (0-indexed) face in `rainbow-csv-column-faces'."
  (nth (mod n (length rainbow-csv-column-faces)) rainbow-csv-column-faces))

;;; Basic per-column coloring

(ert-deftest rainbow-csv-test-fields-get-rotating-colors ()
  "Each field in a row is colored with the face matching its column."
  (with-temp-buffer
    (insert "a,b,c\n1,2,3\n")
    (rainbow-csv-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (dolist (pair '(("a" . 0) ("b" . 1) ("c" . 2)
                    ("1" . 0) ("2" . 1) ("3" . 2)))
      (goto-char (point-min))
      (search-forward (car pair))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  (rainbow-csv-tests--nth-face (cdr pair)))))))

(ert-deftest rainbow-csv-test-header-uses-same-rotation-as-data ()
  "The header row is colored with the same rotating palette as data rows,
not a separate override face."
  (with-temp-buffer
    (insert "a,b,c\n1,2,3\n")
    (rainbow-csv-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "a")
    (let ((header-face (get-text-property (match-beginning 0) 'face)))
      (search-forward "1")
      (should (eq header-face (get-text-property (match-beginning 0) 'face))))))

(ert-deftest rainbow-csv-test-separators-use-comment-face ()
  "Separator characters get `font-lock-comment-face', not a rainbow color."
  (with-temp-buffer
    (insert "a,b,c\n")
    (rainbow-csv-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward ",")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-face))))

;;; Palette wrap-around

(ert-deftest rainbow-csv-test-colors-wrap-around-palette-length ()
  "Columns past the end of `rainbow-csv-column-faces' wrap back to the start."
  (let ((n (length rainbow-csv-column-faces)))
    (with-temp-buffer
      ;; Build a row with one more column than the palette has faces.
      (insert (mapconcat (lambda (i) (format "c%d" i))
                         (number-sequence 1 (1+ n))
                         ","))
      (insert "\n")
      (rainbow-csv-mode)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward (format "c%d" (1+ n)))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  (rainbow-csv-tests--nth-face 0))))))

;;; Quoted fields

(ert-deftest rainbow-csv-test-quoted-field-overrides-rainbow-color ()
  "A quoted field, including any comma inside it, is colored as a string,
not split into separate rainbow-colored fields."
  (with-temp-buffer
    (insert "a,\"b,still b\",c\n")
    (rainbow-csv-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "\"b,still b\"")
    (let ((start (match-beginning 0)))
      (should (eq (get-text-property start 'face) 'font-lock-string-face))
      ;; The comma embedded inside the quotes must not be treated as a
      ;; field separator: it should carry the string face too, not
      ;; `font-lock-comment-face'.
      (should (eq (get-text-property (+ start 3) 'face)
                  'font-lock-string-face)))))

;;; Separator auto-detection feeding into coloring

(ert-deftest rainbow-csv-test-colors-follow-detected-separator ()
  "Column coloring uses whichever separator was auto-detected, not just comma."
  (with-temp-buffer
    (insert "a;b;c\n1;2;3\n")
    (rainbow-csv-mode)
    (should (equal rainbow-csv--separator ";"))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "b")
    (should (eq (get-text-property (match-beginning 0) 'face)
                (rainbow-csv-tests--nth-face 1)))))

;;; Toggle command

(ert-deftest rainbow-csv-test-toggle-removes-and-restores-colors ()
  "`rainbow-csv-toggle-rainbow-columns' actually adds/removes the coloring."
  (with-temp-buffer
    (insert "a,b,c\n")
    (rainbow-csv-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "b")
    (should (eq (get-text-property (match-beginning 0) 'face)
                (rainbow-csv-tests--nth-face 1)))
    ;; Turn rainbow columns off: the field should no longer carry a
    ;; rainbow-csv-column-face-*, though the buffer is of course still
    ;; readable (no assertion on what face, if any, remains).
    (rainbow-csv-toggle-rainbow-columns)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "b")
    (should-not (memq (get-text-property (match-beginning 0) 'face)
                      rainbow-csv-column-faces))
    ;; Turn it back on: coloring should return.
    (rainbow-csv-toggle-rainbow-columns)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "b")
    (should (eq (get-text-property (match-beginning 0) 'face)
                (rainbow-csv-tests--nth-face 1)))))

;;; Pure column-index helper (no font-lock involved)

(ert-deftest rainbow-csv-test-column-index-at ()
  "`rainbow-csv--column-index-at' reports the correct 0-indexed column
for a buffer position, independent of font-lock."
  (with-temp-buffer
    (insert "aa,bb,cc\n")
    (rainbow-csv-mode)
    (goto-char (point-min))
    (should (= (rainbow-csv--column-index-at (point-min)) 0))
    (search-forward "bb")
    (should (= (rainbow-csv--column-index-at (match-beginning 0)) 1))
    (search-forward "cc")
    (should (= (rainbow-csv--column-index-at (match-beginning 0)) 2))))

(provide 'rainbow-csv-mode-tests)

;;; rainbow-csv-mode-tests.el ends here
