#Author: Malsententia <Malsententia@gmail.com>
#Purpose: Colorized man pages, with random themes based upon the
#         arguments passed to man. IE: man man will always have 
#         the same color scheme, but man 7 man will have a
#         different one.
#Note: Colorization will differ between bash and zsh, since their 
#      RNGs are different
#Requires: bash or zsh(may support others), and perl
#Setup: Name it .mancolor.sh, source it in your bashrc/zshrc,
#       and chmod +x it(it also gets run as a script)
#       If you name it something else, change this:
export MANCOLORPATH=$HOME/repos/dotfiles/zsh/mancolor.sh
#If you want it to use just the category(0-9) as the seed change this
# to 1. Man page of the same category will have the same schemes.
export USECATEGORY=0

#Tips welcome: 1MAnCLrnkbrNZSPaQ7PfUSQZYjez17PaG5

#The author disclaims copyright to this source code.  In place of
#a legal notice, here is a blessing:
#   May you do good and not evil.
#   May you find forgiveness for yourself and forgive others.
#   May you share freely, never taking more than you give.

#For those not in the know, the 256 color pallete consists of
#the basic 16 colors at the end, the 25 colorblack-white gradient at
#the end, with 216 colors in between. The numbers of the colors is
#base 6, RGB. ie, 000 is black, 555 is white, 500 is red etc
# for yellow, b6 color would be used like: b6color 5 5 0
[[ $- == *i* ]] && b6color(){
    echo -n $((6#$1$2$3+16))
}&& RND(){
    #BASH doesn't carry the RANDOM seed to subshells,
    #so this is a shitty workaround, since I wanted this to work
    #outside of zsh
    RANDOM=$(</tmp/mancolorseed)
    echo -n $(($(</tmp/mancolorseed)+1))>/tmp/mancolorseed
    echo -en $RANDOM
}&& man() {
    local ARGS="$*" #Bash didn't like ${@[*]} below, so stick it in a variable
    #Set seed using the first four bytes of the md5 of the args
    echo -n "$((16#$(echo "${ARGS[*]}"| md5sum | head -c 4)/2))">/tmp/mancolorseed
    if ((USECATEGORY==1)); then
        local cat="$(MANPAGER=cat /bin/man -w "$@" 2>/dev/null | grep -Pohm 1 '\d(?=\w*?.gz)')"
        [[ -n "$cat" ]]&& echo -n "$cat">/tmp/mancolorseed
    fi
    local mbColor=$(b6color 5 $(($(RND)%3)) 0)
    local mdColor
    if (($(RND)%2)); then
        mdColor=$(b6color $(($(RND)%5)) 5 0)
    else
        mdColor=$(b6color $(($(RND)%3+2)) 0 5)
    fi
    local secColor=$(b6color $(($(RND)%6)) 2 5)
    local soColor
    case $(($(RND)%6)) in
        0)
            soColor=$(b6color 5 $(($(RND)%6)) 0)
            ;;
        1)
            soColor=$(b6color $(($(RND)%6)) 5 0)
            ;;
        2)
            soColor=$(b6color 0 5 $(($(RND)%6)))
            ;;
        3)
            soColor=$(b6color 0 $(($(RND)%4+2)) 5)
            ;;
        4)
            soColor=$(b6color $(($(RND)%4+2)) 0 5)
            ;;
        5)
            soColor=$(b6color 5 0 $(($(RND)%6)))
            ;;
    esac
    local default
    #If you find the main text colors are too colored, and you want them
    #closer to white, change the "%3+3"s to "%2+4"s, and the single 3s to 4s
    case $(($(RND)%6)) in
        0)
            default=$(b6color 5 $(($(RND)%3+3)) 3)
            ;;
        1)
            default=$(b6color $(($(RND)%3+3)) 5 3)
            ;;
        2)
            default=$(b6color 3 5 $(($(RND)%3+3)))
            ;;
        3)
            default=$(b6color 3 $(($(RND)%3+3)) 5)
            ;;
        4)
            default=$(b6color $(($(RND)%3+3)) 3 5)
            ;;
        5)
            default=$(b6color 5 3 $(($(RND)%3+3)))
            ;;
    esac
    local usColor
    if (($(RND)%2)); then
        usColor=$(b6color 0 $(($(RND)%4+2)) 5)
    else
        usColor=$(b6color 5 0 $(($(RND)%6)))
    fi
    local topColor
    if (($(RND)%2)); then
        topColor=$(b6color 5 $(($(RND)%6)) 0)
    else
        topColor=$(b6color 5 0 $(($(RND)%3)))
    fi
    env \
    normColor=$'\e'"[0;38;5;${default}m" \
    sectionColor=$'\e'"[1;38;5;${secColor}m" \
    footColor=$'\e'"[1;48;5;235;38;5;${topColor}m" \
    headColor=$'\e'"[1;38;5;234;48;5;${topColor}m" \
    MANPAGER="$MANCOLORPATH" \
    LESS_TERMCAP_mb=$'\e'"[1;5;48;5;${mbColor}m" \
    LESS_TERMCAP_md=$'\e'"[1;38;5;${mdColor}m" \
    LESS_TERMCAP_me=$'\e'"[0;38;5;${default}m" \
    LESS_TERMCAP_se=$'\e'"[0;38;5;${default}m" \
    LESS_TERMCAP_so=$'\e'"[1;38;5;232;48;5;${soColor}m" \
    LESS_TERMCAP_ue=$'\e'"[0;38;5;${default}m" \
    LESS_TERMCAP_us=$'\e'"[1;4;38;5;${usColor}m" \
    man "$@"
} || perl -pe '
$.==1 && s/^(.*)$/'${headColor}'\1'${normColor}'/;
eof && s/^(.*)$/'${footColor}'\1'${normColor}'/;
s/^/'${normColor}'/;
/^[^\t A-Z]+[A-Z0-9\x08]+[^a-z]+(?!   )$/ && s/\x08.//ga && s/^([^\t A-Z]+)([A-Z0-9\x08]+[^a-z]+)(?!   )$/\1'${sectionColor}'\2/g' | \
/bin/less "$@"
