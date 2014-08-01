;;; intercal.el -- mode for editing INTERCAL code

;; This mode was written by Eric S. Raymond <esr@snark.thyrsus.com>
;; for the C-INTERCAL distribution, and is copyrighted by him 1992.  Free
;; redistribution encouraged.  Someday, maybe, this will be made part of GNU.
;; But probably not unless they take many mind-eroding drugs first.

;; This mode provides abbrevs for C-INTERCAL's statements, including COME FROM.
;; These abbrevs are context-sensitive and will generate either verb or gerund
;; form as appropriate.  The keys RET, ( and * are also bound in useful ways.

;; The intercal-politesse-level adjustment purports to assist the hapless
;; INTERCAL programmer in meeting INTERCAL's Miss Manners requirement.  In
;; INTERCAL-72 and C-INTERCAL releases after 0.7, no fewer than 1/5 and no
;; more than 1/3 of the program statements must contain a PLEASE to gratify
;; the iron whim of the INTERCAL compiler; this mode assists by randomly
;; expanding some fraction of the "do" abbrevs typed to PLEASE DO.
;; The intercal-politesse-level constant is the denominator of this fraction.

;; AIS: I corrected the READ/WRITE, IN/OUT reversal independent of the correction
;; that happened from 0.22 to 0.24 (this code was originally based on the 0.22
;; distribution and updated for 0.24 compatibility); this version is distributed
;; with C-INTERCAL 0.27.
;;
;; I also altered the code for ( so the user didn't have to go to the beginning
;; of the line, added C-c C-[a,c,r,s] for convenience when working with
;; constants, and added support for TRY AGAIN, ONCE, AGAIN, and Font Lock.
;; I added support for M-x compile so it could compile INTERCAL programs
;; correctly (assuming that ick has been installed).

;; 	$Id: intercal.el,v 1.5 1996/11/14 04:02:00 esr Exp $

(defconst intercal-politesse-level 4
  "Fraction of DOs that are automagically expanded to PLEASE DO.")

(defvar intercal-mode-map nil
  "Keymap for INTERCAL mode.")
(if intercal-mode-map
    nil
  (setq intercal-mode-map (make-sparse-keymap))
  (define-key intercal-mode-map "\t" 'tab-to-tab-stop)
  (define-key intercal-mode-map "\r" 'intercal-return)
  (define-key intercal-mode-map "\C-J" 'intercal-return)
  (define-key intercal-mode-map "\C-C\C-C" 'intercal-constant-convert) ;AIS
  (define-key intercal-mode-map "\C-C\C-R" 'intercal-constant-radix-convert) ;AIS
  (define-key intercal-mode-map "\C-C\C-S" 'intercal-char-constant-convert) ;AIS
  (define-key intercal-mode-map "\C-C\C-A" 'intercal-string-array-convert) ;AIS
  (define-key intercal-mode-map "\C-C\C-U" 'intercal-lnu) ;AIS
  (define-key intercal-mode-map "(" 'intercal-paren)
  (define-key intercal-mode-map "*" 'intercal-splat)
  (define-key intercal-mode-map "\177" 'backward-delete-char-untabify)
  )

(defvar intercal-mode-syntax-table nil
  "Syntax table in use in Intercal-mode buffers.")

(if intercal-mode-syntax-table
    nil
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq intercal-mode-syntax-table table)))

(defvar intercal-mode-abbrev-table nil
  "*Abbrev table in use in Intercal-mode buffers.")
(if intercal-mode-abbrev-table
    nil
  (define-abbrev-table 'intercal-mode-abbrev-table ())
  (define-abbrev intercal-mode-abbrev-table "pl"  "PLEASE" nil)
  (define-abbrev intercal-mode-abbrev-table "on"  "ONCE" nil) ;AIS
  (define-abbrev intercal-mode-abbrev-table "ag"  "AGAIN" nil) ;AIS
  (define-abbrev intercal-mode-abbrev-table "ne"  "" 'intercal-ne-abbrev)
  (define-abbrev intercal-mode-abbrev-table "fo"  "" 'intercal-fo-abbrev)
  (define-abbrev intercal-mode-abbrev-table "res" "" 'intercal-res-abbrev)
  (define-abbrev intercal-mode-abbrev-table "st"  "" 'intercal-st-abbrev)
  (define-abbrev intercal-mode-abbrev-table "ret" "" 'intercal-ret-abbrev)
  (define-abbrev intercal-mode-abbrev-table "ig"  "" 'intercal-ig-abbrev)
  (define-abbrev intercal-mode-abbrev-table "rem" "" 'intercal-rem-abbrev)
  (define-abbrev intercal-mode-abbrev-table "ab"  "" 'intercal-ab-abbrev)
  (define-abbrev intercal-mode-abbrev-table "rei" "" 'intercal-rei-abbrev)
  (define-abbrev intercal-mode-abbrev-table "gi"  "" 'intercal-gi-abbrev)
  (define-abbrev intercal-mode-abbrev-table "rea" "" 'intercal-rea-abbrev)
  (define-abbrev intercal-mode-abbrev-table "wr"  "" 'intercal-wr-abbrev)
  (define-abbrev intercal-mode-abbrev-table "co"  "" 'intercal-co-abbrev)
  (define-abbrev intercal-mode-abbrev-table "do"  "" 'intercal-do-abbrev)
  (define-abbrev intercal-mode-abbrev-table "wh"  "" 'intercal-wh-abbrev) ;AIS
  (define-abbrev intercal-mode-abbrev-table "goa" "" 'intercal-goa-abbrev) ;AIS
  (define-abbrev intercal-mode-abbrev-table "gob" "" 'intercal-gob-abbrev) ;AIS
  (define-abbrev intercal-mode-abbrev-table "cr"  "" 'intercal-cr-abbrev) ;AIS
  (define-abbrev intercal-mode-abbrev-table "ma"  "MAYBE" nil) ;AIS
  (define-abbrev intercal-mode-abbrev-table "tr"  "" 'intercal-tr-abbrev) ;AIS
  (define-abbrev intercal-mode-abbrev-table "cal" "" 'intercal-cal-abbrev) ;AIS
 )

(defun use-gerund ()
  (save-excursion
    (beginning-of-line)
    (or (looking-at ".*ABSTAIN") (looking-at ".*REINSTATE"))))

(defmacro make-intercal-abbrev (sym gerund verb)
  (list 'defun sym '() (list 'insert (list 'if '(use-gerund) gerund verb))))

(make-intercal-abbrev intercal-ne-abbrev "NEXTING" "NEXT")
(make-intercal-abbrev intercal-fo-abbrev "FORGETTING" "FORGET")
(make-intercal-abbrev intercal-res-abbrev "RESUMING" "RESUME")
(make-intercal-abbrev intercal-st-abbrev "STASHING" "STASH")
(make-intercal-abbrev intercal-ret-abbrev "RETRIEVING" "RETRIEVE")
(make-intercal-abbrev intercal-ig-abbrev "IGNORING" "IGNORE")
(make-intercal-abbrev intercal-rem-abbrev "REMEMBERING" "REMEMBER")
(make-intercal-abbrev intercal-ab-abbrev "ABSTAINING" "ABSTAIN FROM")
(make-intercal-abbrev intercal-rei-abbrev "REINSTATING" "REINSTATE")
(make-intercal-abbrev intercal-gi-abbrev "GIVING UP" "GIVE UP")
(make-intercal-abbrev intercal-rea-abbrev "READING OUT" "READ OUT")
(make-intercal-abbrev intercal-wr-abbrev "WRITING IN" "WRITE IN")
(make-intercal-abbrev intercal-co-abbrev "COMING FROM" "COME FROM")
(make-intercal-abbrev intercal-tr-abbrev "TRYING AGAIN" "TRY AGAIN") ;AIS
(make-intercal-abbrev intercal-cal-abbrev "CALCULATING" "cal") ;AIS
(make-intercal-abbrev intercal-wh-abbrev "WHILING" "WHILE") ;AIS
(make-intercal-abbrev intercal-goa-abbrev "GOING AHEAD" "GO AHEAD") ;AIS
(make-intercal-abbrev intercal-gob-abbrev "GOING BACK" "GO BACK") ;AIS
(make-intercal-abbrev intercal-cr-abbrev "CREATION" "CREATE") ;AIS

(defun intercal-do-abbrev ()
  "Emit a DO (usually).  Occasionally, emit PLEASE DO."
  (insert
   (if (zerop (% (random) intercal-politesse-level))
       "PLEASE DO"
     "DO")
   ))

(defun intercal-return ()
  "Insert LFD + tab, to bring us back to code-indent level."
  (interactive)
  (if (eolp) (delete-horizontal-space))
  (insert "\n")
  (delete-horizontal-space) ;AIS
  (tab-to-tab-stop)
  (if intercal-lnu-mode (intercal-lnu)) ;AIS
  )

;; AIS: This is the old intercal-paren function, modified slightly.
(defun intercal-paren-2 ()
  "Generate an INTERCAL label if at start of line. Otherwise type '('."
  (interactive)
  (if (and (bolp) (looking-at "[ \t]\\|$"))
      (insert (format "(%d)"
		      (save-restriction
			(widen)
			(save-excursion
			  (beginning-of-line)
			  (1+ (count-lines 1 (point))))))
	      "\t")
    (insert "(")))

;; AIS: I wrote this as a better intercal-paren-2.
(defun intercal-paren ()
  "Generate an INTERCAL label or (."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^[ \t]*$")
	(delete-horizontal-space)))
  (intercal-paren-2)
)

(defun intercal-splat ()
  "Begin an INTERCAL comment."
  (interactive)
  (insert "*")
  (forward-char -1)
  (delete-horizontal-space)
  (forward-char 1)
  (insert " ")
)

;; AIS: Added new abbreviations (marked ;AIS above), expanded the documentation.
(defun intercal-mode ()
  "A major editing mode for the language Intercal.
It activates the following abbrevs (each one appropriately modified to a
gerund if it occurs on a line with ABSTAIN or REINSTATE).

ab   ABSTAIN	co   COME FROM	fo   FORGET
gi   GIVE UP	ig   IGNORE	ne   NEXT
rea  READ OUT	rei  REINSTATE	rem  REMEMBER
res  RESUME	ret  RETRIEVE	st   STASH
wr   WRITE IN	pl   PLEASE     tr   TRY AGAIN
on   ONCE       ag   AGAIN      ma   MAYBE
goa  GO AHEAD   gob  GO BACK    do   DO, or sometimes PLEASE DO
cal  CALCULATE  cr   CREATE     wh   WHILE

Carriage return takes you to the first tab stop (code indent level).
Certain other single keys are bound to things which may or may not be useful.
You may consider discovering these one of the pleasures awaiting you in your
discovery of INTERCAL's unique ambience.

Typing C-c C-c converts a constant in the form #<legal C constant> to a legal
INTERCAL constant (such as #45 or #65280$#65280). If the answer is a mingled
one, it is only valid in binary.

For programmers who prefer TRI-INTERCAL, another constant conversion function
is given. Typing a number in the format (say) #3r2210210011 and pressing
C-c C-r will result in a (possibly mingled) decimal constant that works in
base 3. This works likewise for other bases, say #6r520314 would generate a
base-6 constant.

For binary I/O, C-c C-s is provided. This takes a constant of the form
#?<character> and converts it into a constant of the form #<character code>.
With a prefix argument (as in C-u C-c C-s), the output has its bottom 8 bits
reversed, and so is suitable for use with semi-standard output routines.

For longer strings, C-c C-a can produce sections of programs that store
strings in arrays. The command can give help on itself.

For information about C-c C-u and LNU, see `intercal-lnu-mode'.

Turning on Intercal mode calls the value of the variable intercal-mode-hook
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map intercal-mode-map)
  (setq major-mode 'intercal-mode)
  (setq mode-name "Intercal")
  (setq local-abbrev-table intercal-mode-abbrev-table)
  (set-syntax-table intercal-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "* ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (setq abbrev-mode t)
  (setq abbrev-all-caps t)
  (run-hooks 'intercal-mode-hook))

(provide 'intercal-mode)

;; AIS: I wrote everything from here down.

;; Line Number Update minor mode.
(defvar intercal-lnu-mode nil "Whether intercal-lnu-mode is on or not.")
(make-variable-buffer-local 'intercal-lnu-mode)

(defvar intercal-lnu-mode-initialized nil "Whether intercal-lnu-mode is initialized.")

(defun intercal-lnu-mode (arg)
"Minor mode used primarily when editing INTERCAL.
 (LNU stands for Line Number Update).
The mode tries to ensure that all line labels below 1000 are equal to the line
numbers of the lines they refer to. Renumbering is done when RET is pressed, the
mode is turned on, or C-c C-u is pressed. This mode has no effect unless
intercal-mode is also active, or its functions are called. It is inadvisable to
turn LNU mode on in long files, because line number updates take a long time to
run in long files. Instead, leave LNU mode off and use `intercal-lnu' (C-c C-u).
Labels greater than or equal to 1000 are ignored because they probably indicate
library functions."
  (interactive "P")
  (setq intercal-lnu-mode
	(if (null arg) (not intercal-lnu-mode) (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update)
  (if intercal-lnu-mode (intercal-lnu))
  (if (not intercal-lnu-mode-initialized) (intercal-lnu-initialize))
)

(defun intercal-lnu-initialize ()
"Initializes INTERCAL LNU mode. See `intercal-lnu-mode'."
  (if (not intercal-lnu-mode-initialized)
      (setq minor-mode-alist (cons '(intercal-lnu-mode " LNU") minor-mode-alist)
	    intercal-lnu-mode-initialized t))
)

(defsubst cadar (x)
  "Return the car of the cdr of the car of X."
  (car (cdr (car x))))

(defun intercal-lnu ()
"Updates line numbers in an INTERCAL program. See `intercal-lnu-mode'. This
function will run even if intercal-lnu-mode is off, but intercal-mode must be
on. Running this function manually (with C-c C-u) rather than automatically is
a good idea in long files, which take a long time to update."
  (interactive)
  (if (equal major-mode 'intercal-mode)
      (save-excursion
	(let ((line-number-update-list nil))
	  (goto-char (point-min))
	  (while (re-search-forward "^(\\([0-9]+\\))" nil t)
	    (if (< (string-to-number (match-string-no-properties 1)) 1000)
		(setq line-number-update-list (cons
					       (list
						(string-to-number (match-string-no-properties 1))
						(count-lines 1 (point)))
					       line-number-update-list))))
	  (while line-number-update-list
	    (intercal-lnu-individual (caar line-number-update-list) (cadar line-number-update-list))
	    (setq line-number-update-list (cdr line-number-update-list))))
	(goto-char (point-min))
	(while (search-forward "LNUChange:" nil t)
	  (replace-match "")))
    (error "The buffer is not in intercal-mode"))
)

(defun intercal-lnu-individual (oldlabel newlabel)
"Updates all occurrences of one label in an INTERCAL program for another."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (concat "(" (number-to-string oldlabel) ")") nil t)
      (replace-match (concat "(LNUChange:" (number-to-string newlabel) ")"))))
)

;; Font Lock settings.
;; We use keyword-face for commands, builtin-face for gerunds, constant-face
;; for meshes and mingle-mesh constants, and comment-face for initially
;; abstained lines (because these are usually comments, and anyway it's good
;; for them to stand out). Only the first line of multiline comments is marked.
;; MAYBE lines aren't marked because that's a big giveaway that the line won't
;; be abstained from all program.
(defvar intercal-font-lock-keywords
  '(("^\\(([0-9]+)\\|\\)[ \t]*\\(PLEASE DO\\|DO\\|PLEASE\\)[ \t]*\\(NOT\\|N'T\\).*$" . font-lock-comment-face)
    ("\\<\\(ABSTAIN\\|GIVE UP\\|READ OUT\\|RESUME\\|WRITE IN\\|PIN\\|COME\\|FROM\\|IGNORE\\|REINSTATE\\|RETRIEVE\\|PLEASE\\|DO\\|MAYBE\\|ONCE\\|AGAIN\\|FORGET\\|NEXT\\|REMEMBER\\|STASH\\|DON'T\\|NOT\\|TRY AGAIN\\|WHILE\\|GO AHEAD\\|GO BACK\\)\\>" . font-lock-keyword-face)
    ("\\<\\(ABSTAINING\\|GIVING UP\\|READING OUT\\|RESUMING\\|WRITING IN\\|PINNING\\|COMING\\|IGNORING\\|REINSTATING\\|RETRIEVING\\|FORGETTING\\|NEXTING\\|REMEMBERING\\|STASHING\\|TRYING AGAIN\\|CALCULATING\\|WHILING\\|GOING AHEAD\\|GOING BACK\\)\\>" . font-lock-builtin-face)
    ("[.,:;][0-9]+" . font-lock-variable-name-face)
    ("#[0-9]+\\(\\$#[0-9]+\\|\\)" . font-lock-constant-face))
  "Default expressions to highlight in Intercal mode.")

;; Find an error message produced by ick. If the famous RESUBNIT spelling error
;; is ever corrected, this will need to be corrected too.
(defvar intercal-error-regexp
  '("\\(ICL[0-9][0-9][0-9][IW].*\\)\n[ \t]*ON THE WAY TO \\([0-9]+\\)\n[ \t]*\\(\\(CORRECT\\)\\|\\(RECONSIDER\\)\\) SOURCE AND RESUBNIT$" nil 2 nil (5 . nil) 1)
  "Regexp that identifies an INTERCAL error message, and the file and line number positions in it.")

;; Detect the line with which ick was invoked, so the filename used can be
;; used by Emacs' compile-mode to find the error. This will need to be changed
;; if command-line arguments to ick other than letters are allowed.
(defvar intercal-file-regexp '(".*ick \\(-[a-zA-Z]+ \\)*\\(.*i\\)$" 2 nil nil 0 2)
  "Regexp that identifies the line with which ick was invoked, and the filename it was used on.")

;; Helper function for the binary I/O routines.
(defun intercal-reverse-bits-if-nonnull (flag num)
  "Returns num if flag is null, or num with bits reversed if flag is nonnull."
  (if (null flag) num
    (let ((ans 0) (count 8) (num1 num))
	 (while (> count 0)
	   (setq ans (* ans 2))
	   (if (= (% num1 2) 1) (setq ans (1+ ans)))
	   (setq num1 (/ num1 2))
	   (setq count (1- count)))
	 ans))
)

;; Internal memory for inter-function communication. Yes, I know it's
;; unLispish to do this sort of thing, and breaks if the user tries to
;; convert two strings simultaneously.
(defvar intercal-string-array-convert-options-buffer-used nil
  "Variable that keeps track of a buffer used to display options
for intercal-string-array-convert.")

;; Function that inputs options for intercal-string-array-convert.
;; If the user asks for help, it pops up a help buffer and returns nil.
;; Its documentation string is the information required to appear in
;; the help buffer.
(defun intercal-string-array-convert-options (opts)
  "Options for use with intercal-string-array-convert:
   ? Display the list of options
   1 Pack the string one char/element (default)
   2 Pack the string two chars/element (unimplemented)
   4 Pack the string four chars/element (unimplemented)
   $ Pack the string in mingled format (default)
      so \"abcd\" would become \"a$b\"$\"c$d\"
   < Pack the string in shifted format
      (concatentate the codes for each character)
   R Bit-reverse each character code (default)
   W Don't bit-reverse each character code
   , Treat entered number as a tail array (default)
   ; Treat entered number as a hybrid array
   0 Each character code is based at 0 (default)
   + Each character code is based at the previous code (unimplemented)"
  (interactive "sOptions: (type ?[RET] for help)")
  (if (string= opts "?")
      (progn
	(setq intercal-string-array-convert-options-buffer-used t)
	(with-output-to-temp-buffer "*Options*"
	  (princ (documentation 'intercal-string-array-convert-options)))
	nil)
    opts)
)

;; The displaying of the help buffer is done by
;; intercal-string-array-convert-options, if required. This function holds
;; tests to put the display back as it was in two possible common situations
;; involving the user asking for help (one window active, two windows active).
(defun intercal-string-array-convert (in arr &optional opts)
  "Generate INTERCAL code to initialize an array with information
that makes it suitable for use as a string. For more information,
run the command and enter a question mark for the options."
  (interactive "*MEnter string to convert:\nnEnter number of array required:")
  (let ((cb (current-buffer)) (w1p (one-window-p)) (ow nil))
    (if (null w1p) (setq ow other-window-scroll-buffer))
    (while (null opts)
      (setq opts (call-interactively 'intercal-string-array-convert-options)))
    (if intercal-string-array-convert-options-buffer-used
	(progn
	  (select-window (get-buffer-window cb))
	  (if w1p
	      (delete-other-windows)
	    (switch-to-buffer-other-window ow))))
    (setq intercal-string-array-convert-options-buffer-used nil))
  (let ((packing 1) (basis nil) (arrtype ",") (revbits t) (mingle t))
    (while (> (length opts) 0)
      (if (string= (substring opts 0 1) "1") (setq packing 1)
      (if (string= (substring opts 0 1) "2") (error "Unimplemented") ;(setq packing 2) ;unimplemented
      (if (string= (substring opts 0 1) "4") (error "Unimplemented") ;(setq packing 4) ;unimplemented
      (if (string= (substring opts 0 1) "$") (setq mingle t)  ;unimplemented, but has no effect on option 1
      (if (string= (substring opts 0 1) "<") (setq mingle nil);unimplemented, but has no effect on option 1
      (if (string= (substring opts 0 1) "R") (setq revbits t)
      (if (string= (substring opts 0 1) "W") (setq revbits nil)
      (if (string= (substring opts 0 1) ",") (setq arrtype ",")
      (if (string= (substring opts 0 1) ";") (setq arrtype ";")
      (if (string= (substring opts 0 1) "0") (setq basis nil)
      (if (string= (substring opts 0 1) "+") (error "Unimplemented") ;(setq basis t)   ;unimplemented
	(error "Invalid option"))))))))))))
      (setq opts (substring opts 1)))
    (intercal-return)
    (intercal-do-abbrev)
    (let ((al (/ (+ (length in) packing -1) packing)))
      (insert " " arrtype (int-to-string arr) " <- #" (int-to-string al)))
    (let ((lastchar 0) (arrind 1))
      (while (> (length in) 0)
	(intercal-return)
	(intercal-do-abbrev)
	(insert " " arrtype (int-to-string arr) " SUB #" (int-to-string arrind))
	(insert " <- #?" (substring in 0 1))
	(setq in (substring in 1))
	(intercal-char-constant-convert revbits))))
)

;; Converts char to int by subtracting ! and adding 33.
(defun intercal-char-constant-convert (arg)
  "Convert a constant of the form #?<character> into a legal
INTERCAL constant representing the character's character code.
With a prefix argument, gives bit-reversed output. This routine
is intended for use with the binary I/O capabilities of INTERCAL."
  (interactive "*P") ;Check prefix arg, not read-only
  (save-excursion
    (if (re-search-backward "#" (line-beginning-position) t)
	(if (re-search-forward "#\\?\\(.\\)" (line-end-position) t)
	    (replace-match
	     (concat "#" (int-to-string (intercal-reverse-bits-if-nonnull arg
			  (+ 33 (- (string-to-char
				    (match-string-no-properties 1)) ?!)))))))))
)


;; This is simpler than the next function because we're
;; converting from the target base, so it's simply a case
;; of selecting every second character if we need to produce
;; a mingled result
(defun intercal-constant-radix-convert ()
  "Convert a constant of the form #<base>r<integer in that base>
to a legal TRI-INTERCAL constant in that base. If the number is
sufficiently small, it is converted to decimal; otherwise, it is
converted to two mingled decimal constants."
  (interactive "*")
  (save-excursion
    (if (re-search-backward "#" (line-beginning-position) t)
	(if (re-search-forward "#\\([2-7]\\)r\\([0-6]+\\)"
			       (line-end-position) t)
	    (replace-match
	     (let ((radix (string-to-number
			   (match-string-no-properties 1) 10))
		   (str (match-string-no-properties 2)) (slen 0)
		   (mingle1 "") (mingle2 "") (num1 0) (num2 0))
	       (setq slen (length str))
	       (if (or (<= (* radix slen) 32) ; Calculate max onespot length
		       (or (and (= radix 6) (= slen 6))   ;on these
			   (and (= radix 7) (= slen 5)))) ;three lines
		   (progn
		     (setq num1 (string-to-number str radix))
		     (concat "#" (number-to-string num1)))
		 (progn
		   (while (/= 0 (length str))
		     (setq mingle2 (concat (substring str -1) mingle2))
		     (setq str (substring str 0 -1))
		     (if (/= 0 (length str))
			 (progn
			   (setq mingle1 (concat (substring str -1) mingle1))
			   (setq str (substring str 0 -1)))))
		   (setq num1 (string-to-number mingle1 radix))
		   (setq num2 (string-to-number mingle2 radix))
		   (concat "#" (number-to-string num1)
			   "$#" (number-to-string num2)))))))))
)

;; The following code needs a bit of explanation. INTERCAL
;; can handle constants up to 4294967295 (0xffffffff), but
;; Emacs's constants max out somewhere between 100 million
;; and 1 billion. So, when a high number is entered, the
;; code processes the last 6 digits (in whatever base the
;; number is entered in) separately from the rest of the
;; number. This ensures that the arithmetic routines never
;; overflow if the input is a valid integer in the range
;; 0-4294967295. The calculations used do not work for
;; higher numbers, but that doesn't matter because
;; INTERCAL wouldn't accept them. Constants higher than
;; 65535 are given in mingled form (e.g. #65280$#65280).
(defun intercal-constant-convert ()
  "Convert a constant of the form #<a valid C integer> to a
legal INTERCAL constant (#<16-bit decimal integer> or
#<16-bit decimal integer>$#<16-bit decimal integer>).
Decimal (#4294967295), octal (#037777777777) and
hex (#0xffffffff) formats are all supported."
  (interactive "*")
  (save-excursion
    (if (re-search-backward "#" (line-beginning-position) t)
	(if (re-search-forward "#\\(0?[xX]?\\)\\([0-9a-fA-F]+\\)"
			       (line-end-position) t)
	    (replace-match
	     (let ((str (match-string-no-properties 2))
		   (strhi "0") (strlow "0")
		   (base (if (string= (match-string-no-properties 1) "")
			     10
			   (if (string= (match-string-no-properties 1) "0")
			       8 16))))
	       (if (<= (length str) 6) (setq strlow str)
		 (setq strlow (substring str -6) strhi (substring str 0 -6)))
	       (let ((num (string-to-number strlow base))
		     (numhi (string-to-number strhi base)))
		 (if (and (<= num 65535) (string= strhi "0"))
		     (concat "#" (number-to-string num))
		   (let ((mingle1 0) (mingle2 0) (count 16) (temp 0))
		     (while (> count 0)
		       (progn
			 (setq temp (% num 4))
			 (setq num (/ num 4))
			 (if (= (% temp 2) 1) (setq mingle2 (+ mingle2 65536)))
			 (if (> temp 1) (setq mingle1 (+ mingle1 65536)))
			 (setq mingle1 (/ mingle1 2))
			 (setq mingle2 (/ mingle2 2))
			 (setq count (- count 1))
			 (if (= count 13)
			     (setq num (+ num (* (/ (expt base 6) 64) numhi))))))
		     (concat "#" (number-to-string mingle1)
			     "$#" (number-to-string mingle2))
		     ))))
	     t t))))
)

;; This wraps around compilation-parse-errors in compile, and makes sure that
;; the first line of the compilation buffer is blank. This is to make the
;; call of ick (which is needed to determine the source file) look like an
;; error message rather than a compiler call.
(defun intercal-error-wrap (limit-search find-at-least)
  "Parse the current buffer as ick error messages. This just wraps around
'compilation-parse-errors'. See variable 'compilation-parse-errors-function'
for the interface it uses."
  (save-excursion
    (beginning-of-buffer)
    (if (looking-at "^$") nil (newline)))
  (compilation-parse-errors limit-search find-at-least)
)

(require 'compile) ;Now a prerequisite for intercal.el, so its variables can be
                   ;changed to INTERCAL values upon loading an INTERCAL file

;; This hook changes settings in fontlock and compile, so that they
;; recognize INTERCAL syntax and ick error messages. Default options to
;; ick are to optimize, but I chose not to give -b as a default (because
;; it spoils the fun). I'm slightly tempted to use -m as default, but
;; I'm not sure I'm that evil, and I left -f out for the time being because
;; it's something of a radical change to the output.
(add-hook 'intercal-mode-hook
	  (function (lambda ()
		      (make-local-variable 'font-lock-defaults)
		      (setq font-lock-defaults '(intercal-font-lock-keywords t))
		      (make-local-variable 'compile-command)
		      (setq compile-command (concat "ick -O " (buffer-name)))
		      (make-local-variable 'compilation-parse-errors-function)
		      (setq compilation-parse-errors-function 'intercal-error-wrap))))

;; In recent versions of Emacs this must apparently be set globally;
;; also, the -error-regexp and -file-regexp were merged.
(setq compilation-error-regexp-alist (cons intercal-error-regexp
					   compilation-error-regexp-alist))
(if (boundp 'compilation-file-regexp-alist)
    (setq compilation-file-regexp-alist (cons intercal-file-regexp
					      compilation-file-regexp-alist))
  (setq compilation-error-regexp-alist (cons intercal-file-regexp
					     compilation-error-regexp-alist))
  )


;; AIS: End of section I wrote.

;;; intercal.el ends here
