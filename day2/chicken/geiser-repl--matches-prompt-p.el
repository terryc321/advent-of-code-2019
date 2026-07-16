
;; in emacs M-x h f geiser-repl--matches-prompt-p
;; when given txt "" as empty string it falls over
(geiser-repl--matches-prompt-p "")

(geiser-repl--matches-prompt-p "(+ 1 2)")

tries string-match-p against empty string

geiser-repl--connection
geiser-con--connection-prompt
(defsubst geiser-con--connection-prompt (c)
  (cdr (assq :prompt c)))

(defsubst geiser-con--connection-debug-prompt (c)
  (cdr (assq :debug-prompt c)))

(string-empty-p "")

(defun geiser-repl--matches-prompt-p (txt)
  (cond
   ((string-empty-p txt) nil)
   (t (let ((prompt1 (geiser-con--connection-prompt geiser-repl--connection)))
	(if prompt1
	    (string-match-p prompt1 txt)
	  (let ((prompt2 (geiser-con--connection-debug-prompt geiser-repl--connection)))
	    (if prompt2
		(string-match-p prompt2 txt)
	      nil)))))))


(defun geiser-repl--matches-prompt-p (txt)
  (or (string-match-p
       (geiser-con--connection-prompt geiser-repl--connection) <<<------ evaluates to "#[^;]*;[^:0-9]*:?[0-9]+> "
       txt)
      (string-match-p
       (geiser-con--connection-debug-prompt geiser-repl--connection)
       txt)))

(string-match-p "#[^;]*;[^:0-9]*:?[0-9]+> " "")

(geiser-con--connection-debug-prompt geiser-repl--connection) <<---- evaluates to nil , since not on the debug prompt


(defun my-geiser-chicken-safe-prompt-patch (txt)
  "Custom robust patch to prevent wrong-type-argument stringp nil in Geiser REPL."
  (cond
   ((not (stringp txt)) nil) 
   ((string-empty-p txt) nil)
   (t (or (let ((prompt1 (geiser-con--connection-prompt geiser-repl--connection)))
            (and prompt1 (string-match-p prompt1 txt)))
          (let ((prompt2 (geiser-con--connection-debug-prompt geiser-repl--connection)))
            (and prompt2 (string-match-p prompt2 txt)))))))

;; Tell Emacs to completely swap out the buggy function with your patched version
(advice-add 'geiser-repl--matches-prompt-p :override #'my-geiser-chicken-safe-prompt-patch)
