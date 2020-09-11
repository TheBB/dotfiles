function fish_right_prompt
    if test $last_status -ne 0
        set_color red
        echo $last_status
        set_color normal
    end
end
