set background=dark

if exists("syntax_on")
    syntax reset
endif

let colors_name = "bigbug"

let s:bb = {}

let s:bb.mono0  = ['#000000', 0]
let s:bb.mono1  = ['#080808', 232]
let s:bb.mono2  = ['#121212', 233]
let s:bb.mono3  = ['#1c1c1c', 234]
let s:bb.mono4  = ['#262626', 235]
let s:bb.mono5  = ['#303030', 236]
let s:bb.mono6  = ['#3a3a3a', 237]
let s:bb.mono7  = ['#444444', 238]
let s:bb.mono8  = ['#4e4e4e', 239]
let s:bb.mono9  = ['#585858', 240]
let s:bb.mono10 = ['#606060', 241]
let s:bb.mono11 = ['#666666', 242]
let s:bb.mono12 = ['#767676', 243]
let s:bb.mono13 = ['#808080', 244]
let s:bb.mono14 = ['#8a8a8a', 245]
let s:bb.mono15 = ['#949494', 246]
let s:bb.mono16 = ['#9e9e9e', 247]
let s:bb.mono17 = ['#a8a8a8', 248]
let s:bb.mono18 = ['#b2b2b2', 249]
let s:bb.mono19 = ['#bcbcbc', 250]
let s:bb.mono20 = ['#c6c6c6', 251]
let s:bb.mono21 = ['#d0d0d0', 252]
let s:bb.mono22 = ['#dadada', 253]
let s:bb.mono23 = ['#e4e4e4', 254]
let s:bb.mono24 = ['#eeeeee', 255]
let s:bb.mono25 = ['#ffffff', 15]

let s:bb.yellow = ['ffff00', 226]

function! HL(group, fg, ...)
    " Arguments: group, guifg, guibg, gui, guisp

    let histring = 'hi ' . a:group . ' '

    if strlen(a:fg)
        if a:fg == 'fg'
            let histring .= 'guifg=fg ctermfg=fg '
        else
            let c = get(s:bb, a:fg)
            let histring .= 'guifg=#' . c[0] . ' ctermfg=' . c[1] . ' '
        endif
    endif

    if a:0 >= 1 && strlen(a:1)
        if a:1 == 'bg'
            let histring .= 'guibg=bg ctermbg=bg '
        else
            let c = get(s:bb, a:1)
            let histring .= 'guibg=#' . c[0] . ' ctermbg=' . c[1] . ' '
        endif
    endif

    if a:0 >= 2 && strlen(a:2)
        let histring .= 'gui=' . a:2 . ' cterm=' . a:2 . ' '
    endif

    if a:0 >= 3 && strlen(a:3)
        let c = get(s:bb, a:3)
        let histring .= 'guisp=#' . c[0] . ' '
    endif

    execute histring
endfunction

call HL('Normal', 'mono25', 'mono2')

call HL('Folded', 'mono15', 'mono4', 'none')

call HL('VertSplit', 'mono20', 'mono20', 'none')

call HL('CursorLine',   '', 'mono4', 'none')
call HL('CursorColumn', '', 'mono4')
call HL('ColorColumn',  '', 'mono4')

call HL('LineNr', 'mono15', 'mono3')
