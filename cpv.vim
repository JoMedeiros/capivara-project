" Vim syntax file
" Language: Capivara Lang
" Maintainer: Josivan Medeiros
" Latest Revision: May 2019

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword syntaxElementKeyword begin end nextgroup=syntaxElement2

" Matches
syn match syntaxElementMatch 'regexp' contains=syntaxElement1 nextgroup=syntaxElement2 skipwhite

" Regions
syn region syntaxElementRegion start='x' end='y'
