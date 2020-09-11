function fish_prompt
    set -g last_status $status

    # Username
    set_color -o green
    echo -n -s $USER @ (hostname)

    # Working directory
    set_color -o cyan
    echo -n -s "  " (prompt_pwd)

    # Git information
    echo -n -s " " (fish_git_prompt)

    # Conda environment
    if set -q CONDA_DEFAULT_ENV
        set_color -o cyan
        echo -n -s "  " "(c:" $CONDA_DEFAULT_ENV ")"
    end

    # Newline
    echo ""

    # Prompt
    set_color -o blue
    echo -n -s "-> % "

    set_color normal
end
