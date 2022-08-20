" Vim syntax file
" Language: SWL
" Put this file in .vim/syntax/swl.vim
" and add in your .vimrc file the next line:
" autocmd BufRead,BufNewFile *.swl set filetype=swl

if exists("b:current_syntax")
    finish
endif

syntax keyword swlKeyword if else while for break func
syntax keyword swlKeyword alias intrinsic as arr struct ref deref
syntax keyword swlKeyword include <- from for typeclass instance

syntax keyword swlType char bool int float void string
syntax keyword swlType true false

syntax match swlComment /\/\/.*$/
syntax region swlComment start=/\/\*/ end=/\*\//

syntax region swlString start=/"/ end=/"/

highlight link swlKeyword Keyword
highlight link swlType Type
highlight link swlComment Comment
highlight link swlString String


let b:current_syntax = "swl"
