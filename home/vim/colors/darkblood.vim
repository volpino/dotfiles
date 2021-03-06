" Author: cHoBi

hi clear
if exists("syntax on")
	syntax reset
endif
let g:colors_name = "darkblood"

" General colors
hi Normal        ctermfg=grey
hi Directory     term=bold  ctermfg=red
hi ErrorMsg	     term=standout  ctermfg=white ctermbg=red
hi NonText	     term=bold  ctermfg=darkgray
hi SpecialKey    term=bold  ctermfg=darkgray
hi LineNr        term=standout  ctermfg=darkgrey
hi IncSearch     term=reverse  cterm=reverse
hi Search        term=reverse  ctermfg=black ctermbg=yellow
hi Visual        term=bold,reverse  cterm=bold,reverse ctermfg=gray ctermbg=black
hi VisualNOS     term=bold,underline  cterm=bold,underline
hi MoreMsg       term=bold ctermfg=darkgreen 
hi ModeMsg       term=bold cterm=bold gui=bold
hi Question      term=standout ctermfg=darkgreen
hi WarningMsg    term=standout ctermfg=red
hi WildMenu      term=standout ctermfg=black ctermbg=yellow
hi Folded        term=standout ctermfg=blue ctermbg=white 
hi FoldColumn    term=standout ctermfg=blue ctermbg=white
hi DiffAdd       term=bold ctermbg=blue
hi DiffChange    term=bold ctermbg=darkmagenta
hi DiffDelete    term=bold cterm=bold ctermfg=lightblue ctermbg=cyan
hi DiffText      term=reverse cterm=bold ctermbg=red
hi StatusLine    term=reverse cterm=reverse
hi StatusLineNC  term=reverse cterm=reverse
hi VertSplit     term=reverse cterm=reverse
hi Title         term=bold ctermfg=darkmagenta

" Syntax colors
hi Comment     term=bold ctermfg=darkgrey
hi PreProc     term=underline ctermfg=darkgreen
hi Constant    term=underline ctermfg=darkred
hi Type        term=underline ctermfg=darkred
hi Statement   term=bold ctermfg=darkyellow
hi Identifier  term=underline ctermfg=darkgreen
hi Ignore      term=bold ctermfg=darkgray
hi Special     term=underline ctermfg=brown
hi Error       term=reverse ctermfg=gray
hi Todo        term=reverse ctermfg=black ctermbg=darkred
hi Underlined  term=underline cterm=underline ctermfg=darkred
hi Number      term=underline ctermfg=darkred

" Syntax links
hi link String		Constant
hi link Character	Constant
hi link Number		Constant
hi link Boolean		Constant
hi link Float		Number
hi link Function	Identifier
hi link Number		Constant
hi link Conditional	Statement
hi link Repeat		Statement
hi link Label		Statement
hi link Keyword		Statement
hi link Exception	Statement
hi link Operator	Statement
hi link Include		PreProc
hi link Define		PreProc
hi link Macro		PreProc
hi link PreCondit	PreProc
hi link StorageClass	Type
hi link Structure	Type
hi link Typedef		Type
hi link Tag		Special
hi link SpecialChar	Special
hi link Delimiter	Normal
hi link SpecialComment	Special
hi link Debug		Special


