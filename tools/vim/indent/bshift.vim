" Vim indent file
" Language:    bshift
" Maintainer:  Brian Steffens <briansteffens@gmail.com>
" Last Change: 2017 Sept 17

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
   finish
endif
let b:did_indent = 1

" Use built-in C indenting
setlocal cindent

let b:undo_indent = "setl cin<"
