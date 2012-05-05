(require 'font-lock)

(defvar tcl-font-lock-version
  "2.0a6 (fellowsd,Thursday October 21 15:21:29 BST 1999,Tcl8.0,{GNU-Emacs19.34,GNU-Emacs20.3.1,GNU-Emacs21.1,XEmacs20.4})"
  "A description of what version the highlighting for Tcl-mode is at,
who is the author, when they wrote the code, what version of Tcl it
was designed to be compatible with and what versions of emacs it was
designed to be compatible with.  Variation from the last two might
work, but the effects are not guaranteed to be useful or even
non-harmful.")

(defun tcl-font-lock-join (l)
  "Joins the list of strings/symbols into a \\| separated list within
a string."
  (concat "\\<\\(" (mapconcat (lambda (i) (format "%s" i)) l "\\|") "\\)\\>"))

(defun tcl-font-emacs-type (&optional error-message)
  "What sort of emacs interpreter have we got?"
  (cond
   ((and (boundp 'running-xemacs)
	  (eval 'running-xemacs))
    'xemacs)
   ((and (boundp 'emacs-major-version)
	  (eval '(> emacs-major-version 20)))
    'emacs21)
   ((and (boundp 'emacs-major-version)
	  (eval '(> emacs-major-version 19)))
    'emacs20)
   ((fboundp 'font-lock-make-face)
    'emacs19)
   (error-message (error error-message))))


(defun tcl-font-lock-makeface (sym &optional fore back bold ital)
  "Create a face with the given name with the given appearance.

The FORE parameter sets the foreground of the face to the given colour
if non-null.  The BACK parameter sets the background of the face to
the given colour if non-null.  If BOLD is non-null, attempt to make
the face emboldened or stronger.  If ITAL is non-null, attempt to make
the face italic or oblique.

Not all platforms or display subsystems will support all options for
all possible fonts.  Font family and size selection are left to the
user, as I believe that programs should not set those; one person's
usable is another person's illegible or idiotically large."
  (let ((type (tcl-font-emacs-type (format "Unable to make face %s!" sym))))
    (cond

     ((or (eq type 'xemacs) (eq type 'emacs21))
      (or (facep sym) (progn (eval '(make-face sym))
			          (and fore (set-face-foreground sym fore))
				       (and back (set-face-background sym back))
				            (and bold (eval '(make-face-bold   sym)))
					         (and ital (eval '(make-face-italic sym)))))
      (set sym sym))

     ((eq type 'emacs20)
      (or (facep sym) (progn (eval '(make-empty-face sym))
			          (and fore (set-face-foreground     sym fore))
				       (and back (set-face-background     sym back))
				            (and bold (eval '(make-face-bold   sym nil t)))
					         (and ital (eval '(make-face-italic sym nil t)))))
      (set sym sym))

     ((eq type 'emacs19)
      (eval '(font-lock-make-face (list sym fore back bold ital)))))))

(defun tcl-font-lock-makefacelist (facelist)
  "Make all the faces given."
  (if (null facelist) nil
    (let ((f (car facelist)))
      (tcl-font-lock-makeface (nth 0 f)
			            (nth 1 f)
				          (nth 2 f)
					        (nth 3 f)
						      (nth 4 f)))
    (tcl-font-lock-makefacelist (cdr facelist))))

(tcl-font-lock-makefacelist
;;; See tcl-font-lock-makeface for more details.

 '(;; For Comments
   (tcl-font-lock-comment-face "firebrick"   nil nil  italic)

   ;; For Strings
   (tcl-font-lock-string-face  "grey40"      nil nil  nil   )

   ;; For the [source] and [load] Commands
   (tcl-font-lock-source-face  "violet"      nil nil  nil   )

   ;; For Procedure Definitions
   (tcl-font-lock-fundef-face  "blue2"       nil bold nil   )

   ;; For Commands that Define Variables
   (tcl-font-lock-vardef-face  "forestgreen" nil bold nil   )

   ;; For Commands that Delete Things
   (tcl-font-lock-delete-face  "red2"        nil bold nil   )

   ;; For Commands that Implement the Rest of the Core Set
   (tcl-font-lock-struct-face  "blue"        nil nil  nil   )

   ;; For Other Tcl Command Names
   (tcl-font-lock-command-face  nil          nil bold nil   )

   ;; For Binding Sequences
   (tcl-font-lock-binding-face "orange2"     nil bold nil   )

   ;; For Tk-Related Command Names
   (tcl-font-lock-tkword-face  "sienna"      nil nil  nil   )))
 
(defvar tcl-font-lock-keywords nil
  "Donal's keywords for fontifying Tcl/Tk")

(let ((startcmd "\\(^\\|[[{;]\\)\\s-*")
      (toendcmd "\\([^\\;\n]\\|\\\\.\\)*\\)[;\n]")
      (andword "\\s-+[^] \t\n;}]+\\)")
      (reqsubcmd "\\s-+[a-z]+\\>\\)")
      (optsubcmd "\\(\\s-+[a-z]+\\>\\)?\\)")
      ;; Core Tcl commands -------------------------------------------
      (core-commands '(foreach while for if case switch eval uplevel
			              return catch expr bgerror unknown break
				             exit continue error subst))
      (namespace-subcmds '(code children current delete eval export
				forget import inscope origin parent
				qualifiers tail which))
      (variable-commands '(upvar global))
      (set-commands '(set variable))
      ;; Tcl commands that are (individually) less common ------------
      (extra-tcl-cmds '(cd append close concat eof exec fblocked
			      fconfigure fcopy fileevent flush format
			         gets glob incr join lappend lindex linsert
				    list llength lrange lreplace lsearch lsort
				       open pid puts pwd read regexp regsub scan
				          seek socket split tell time vwait))
      (xtcl-withsubs '(array binary clock file history info package
			          string trace))
      (xtcl-optsubs '(after))
      ;; Tk commands -------------------------------------------------
      (tk-commands '(frame bindtags toplevel focus raise message label
			      "\\(radio\\|check\\|menu\\)?button" listbox
			         scrollbar destroy entry scale canvas text
				    lower bell tkerror "tk_\\S-+" menu raise))
      (tk-subcmdcmds '(wm winfo pack place grid bind option send font
			    selection "image\\(\\s-+create\\)" clipboard
			      tk tkwait event grab)))
  ;; -----------------------------------------------------------------
  (setq tcl-font-lock-keywords
	(list (cons (concat startcmd "\\(#\\([^\\\n]\\|\\\\.\\|\\\\\n\\)*\\)")
		        '(2 tcl-font-lock-comment-face t))
	            '("\\<\\(then\\|else\\(if\\)?\\)\\>"
		      . tcl-font-lock-struct-face)
		          (cons (concat startcmd (tcl-font-lock-join core-commands))
				    '(2 tcl-font-lock-struct-face))
			        (cons (concat startcmd "\\(namespace\\>\\(\\s-+"
					          (tcl-font-lock-join namespace-subcmds) "\\)?\\)")
				          '(2 tcl-font-lock-struct-face))
				      (cons (concat startcmd "\\(interp" andword)
					        '(2 tcl-font-lock-struct-face))
				            (cons (concat startcmd (tcl-font-lock-join extra-tcl-cmds))
						      '(2 tcl-font-lock-command-face))
					          (cons (concat startcmd "\\(" (tcl-font-lock-join xtcl-withsubs)
								    reqsubcmd)
							    '(2 tcl-font-lock-command-face))
						        (cons (concat startcmd "\\(" (tcl-font-lock-join xtcl-optsubs)
								          optsubcmd)
							          '(2 tcl-font-lock-command-face))
							      (cons (concat startcmd "\\(update\\>" optsubcmd)
								        '(2 tcl-font-lock-command-face))

							            (cons (concat startcmd "\\(\\(load\\|source\\)\\>" toendcmd)
									      '(2 tcl-font-lock-source-face))
								          (cons (concat startcmd "\\(proc" andword)
										    '(2 tcl-font-lock-fundef-face))
									        (cons (concat startcmd "\\("
											          (tcl-font-lock-join variable-commands) toendcmd)
										          '(2 tcl-font-lock-vardef-face))
										      (cons (concat startcmd "\\("
												        (tcl-font-lock-join set-commands) andword)
											        '(2 tcl-font-lock-vardef-face))
										            (cons (concat startcmd "\\(unset\\>" toendcmd)
												      '(2 tcl-font-lock-delete-face))
											          (cons (concat startcmd "\\(rename\\)\\>")
													    '(2 tcl-font-lock-delete-face))

												        '("<\\S-+>" . tcl-font-lock-binding-face)
													      (cons (concat startcmd (tcl-font-lock-join tk-commands))
														        '(2 tcl-font-lock-tkword-face))
													            (cons (concat startcmd "\\(" (tcl-font-lock-join tk-subcmdcmds)
																      optsubcmd)
															      '(2 tcl-font-lock-tkword-face)))))

(let ((type (tcl-font-emacs-type "Unable to determine emacs version")))
  (cond
   ;; GNU emacs 19 & 20 provide tcl-mode.el
   ((or (eq type 'emacs19) (eq type 'emacs20))
    (require 'tcl-mode))
   ;; GNU emacs 21+ and XEmacs provide tcl.el
   (t
    (require 'tcl))))
(defun tcl-font-lock-install ()
  "Installs Donal's TCL highlighting based on font-lock"
  (interactive)
  (setq font-lock-defaults
	'(tcl-font-lock-keywords
	    ;; Ignore the syntax table for highlighting comments - we'll
	    ;; do that ourselves so that we can make sure it happens in
	    ;; the correct places only (I think :^)
	    nil nil
	      ((?$ . "w") (?: . "w") (?# . "_"))
	        tcl-beginning-of-proc
		  (font-lock-string-face . tcl-font-lock-string-face)))
  (font-lock-mode 1))
(if (not (eq (tcl-font-emacs-type) 'emacs21))
    (add-hook 'tcl-mode-hook 'tcl-font-lock-install))
(provide 'tcl-font)
