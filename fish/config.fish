# Remove the welcome message
set fish_greeting

# Prompt settings
set -g ___fish_git_prompt_color (set_color yellow)
set -g __fish_git_prompt_showdirtystate true
set -g __fish_git_prompt_showupstream auto
set -g fish_prompt_pwd_dir_length 3

# Key bindings
bind \cl forward-char

# Use vim as EDITOR
set -xg EDITOR vim

# Path manipulation
set PATH $HOME/.local/bin $HOME/.cargo/bin $PATH

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/eivind/source/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<


function mv_todo_oldbarred
    if test (basename $PWD) != "staging"
        echo "Wrong directory"
        return 1
    end
    for x in *
        if test -f todo/$x
            echo $x
            mv todo/$x oldbarred/$x
        end
    end
end

function mv_marked_todo
    if test (basename $PWD) != "staging"
        echo "Wrong directory"
        return 1
    end
    for x in (cat ~/marked.txt | sort | uniq)
        echo $x
        mv $x todo/
    end
end
