function venv
    if test (count $argv) = 1
        set -f name $argv[1]
    else
        set -f name base
    end
    source "$HOME/venvs/"$name"/bin/activate.fish"
end
