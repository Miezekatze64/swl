" The vim syntax file for swl

if exists("b:current_syntax")
    finish
endif

let b:current_syntax = "swl"

syntax keyword swlKeyword if else while for break func
syntax keyword swlKeyword alias intrinsic as arr struct ref deref
syntax keyword swlKeyword include <- from for typeclass instance
highlight link swlKeyword Keyword

syntax keyword swlType char bool int float void
syntax keyword swlType true false
highlight link swlType Function

syntax match swlComment /\/\/.*$/
syntax region swlComment start=/\/\*/ end=/\*\//
highlight link swlComment Comment

syntax region swlString start=/"/ end=/"/
highlight link swlString String
