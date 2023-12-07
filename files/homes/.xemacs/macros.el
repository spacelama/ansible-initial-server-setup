(;--------------------------- power-macros ---------------------------
 ;-                  Sh-C-f8 - specific for c-mode                   -
 ;--------------------------------------------------------------------
	pm-def-macro
	'add-debug-statement-to-function
	'c-mode [(shift control f8)]
	"add a debug statement to a function name"
	"C-s redraw SPC = SPC true C-a C-x r SPC 1 M-x search-backwards-regexp RET ^[^ ]* [^ ]*(.*) RET <C-right> <right> C-SPC <C-right> M-w C-x r j 1 TAB fprintf(strd 2*<backspace> derr, SPC \"redraw=true: SPC C-y \\n\"); RET <down>"
 ;--------------------------------------------------------------------
)

(;--------------------------- power-macros ---------------------------
 ;-                    f8 - specific for f90-mode                    -
 ;--------------------------------------------------------------------
	pm-def-macro
	'pm-mac-tabdown
	'f90-mode [f8]
	"tabs the current line, and modifies it (non destructively) to make sure keywords are capatalised"
	"C-a SPC <backspace> TAB <down>"
 ;--------------------------------------------------------------------
)

(;--------------------------- power-macros ---------------------------
 ;-                    S-f8 - specific for c-mode                    -
 ;--------------------------------------------------------------------
	pm-def-macro
	'add-function-name-to-close-brace
	'c-mode [(super f8)]
	""
	"M-C-s ^ } M-x search-backwards-regexp RET ^[^ ]* [^ ]*(.*) RET <C-right> <right> C-SPC <C-right> M-w 2*<M-C-s> C-e SPC // SPC C-y ()"
 ;--------------------------------------------------------------------
)

(;--------------------------- power-macros ---------------------------
 ;-                    Sh-f8 - global definition                     -
 ;--------------------------------------------------------------------
	pm-def-macro
	'pm-mac-change-fortran-continuation
	'global [(shift f8)]
	"switches the continuation line from next line to current line"
	"C-s & <left> C-d <up> C-e SPC & TAB"
 ;--------------------------------------------------------------------
)

