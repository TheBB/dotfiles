function cleardir
    if test (basename $PWD) != "build"
        echo "Wrong directory"
        return 1
    end
    rm -rf *
end
