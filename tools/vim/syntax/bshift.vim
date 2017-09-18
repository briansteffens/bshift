" Vim syntax file
" Language:     bshift
" Maintainer:   Brian Steffens <briansteffens@gmail.com>
" Last Change:  2017 Sept 18

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

let s:ft = matchstr(&ft, '^\([^.]\)\+')


" A bunch of useful C keywords
syn keyword cStatement    return break continue defer import unqualified
syn keyword cStatement    export
syn keyword cConditional  if else
syn keyword cRepeat       while
syn keyword cTodo         TODO FIXME
syn keyword cOperator     sizeof syscall variadic
syn keyword cType         auto void bool u8 u64
syn keyword cStructure    struct construct destruct
syn keyword cStorageClass extern
syn keyword cConstant     true false


" cCommentGroup allows adding matches for special things in comments
syn cluster     cCommentGroup   contains=cTodo,cBadContinuation


" String and Character constants
" Highlight special characters (those which have a backslash) differently
syn match       cSpecial        display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"


" Highlight % items in strings.
syn match   cFormat         display "%\(\d\+\$\)\=[-+' #0*]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hlLjzt]\|ll\|hh\)\=\([aAbdiuoxXDOUfFeEgGcCsSpn]\|\[\^\=.[^]]*\]\)" contained


syn region    cString         start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial,cFormat,@Spell extend


syn region      cCppSkip        contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=cSpaceError,cCppSkip

syn cluster     cStringGroup    contains=cCppString,cCppSkip

" Character literals
syn match       cCharacter      "L\='[^\\]'"
syn match       cCharacter      "L'[^']*'" contains=cSpecial


"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match       cNumbers        display transparent "\<\d\|\.\d" contains=cNumber,cFloat,cOctalError,cOctal
" Same, but without octal error (for comments)
syn match       cNumbersCom     display contained transparent "\<\d\|\.\d" contains=cNumber,cFloat,cOctal
syn match       cNumber         display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"


syn case match


syn region    cCommentL       start="//" skip="\\$" end="$" keepend contains=@cCommentGroup,cSpaceError,@Spell


syn region  cComment        matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cSpaceError,@Spell fold extend


" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link cFormat             cSpecial
hi def link cCppString          cString
hi def link cCommentL           cComment
hi def link cCommentStart       cComment
hi def link cLabel              Label
hi def link cUserLabel          Label
hi def link cConditional        Conditional
hi def link cRepeat             Repeat
hi def link cCharacter          Character
hi def link cSpecialCharacter   cSpecial
hi def link cNumber             Number
hi def link cOctal              Number
hi def link cOctalZero          PreProc  " link this to Error if you want
hi def link cFloat              Float
hi def link cOctalError         cError
hi def link cParenError         cError
hi def link cErrInParen         cError
hi def link cErrInBracket       cError
hi def link cCommentError       cError
hi def link cCommentStartError  cError
hi def link cSpaceError         cError
hi def link cSpecialError       cError
hi def link cCurlyError         cError
hi def link cOperator           Operator
hi def link cStructure          Structure
hi def link cStorageClass       StorageClass
hi def link cInclude            Include
hi def link cPreProc            PreProc
hi def link cDefine             Macro
hi def link cIncluded           cString
hi def link cError              Error
hi def link cStatement          Statement
hi def link cCppInWrapper       cCppOutWrapper
hi def link cCppOutWrapper      cPreCondit
hi def link cPreConditMatch     cPreCondit
hi def link cPreCondit          PreCondit
hi def link cType               Type
hi def link cConstant           Constant
hi def link cCommentString      cString
hi def link cComment2String     cString
hi def link cCommentSkip        cComment
hi def link cString             String
hi def link cComment            Comment
hi def link cSpecial            SpecialChar
hi def link cTodo               Todo
hi def link cBadContinuation    Error
hi def link cCppOutSkip         cCppOutIf2
hi def link cCppInElse2         cCppOutIf2
hi def link cCppOutIf2          cCppOut
hi def link cCppOut             Comment

let b:current_syntax = "bshift"

unlet s:ft

let &cpo = s:cpo_save
unlet s:cpo_save
