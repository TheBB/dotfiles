function petsc
    set -xg PETSC_ARCH $argv[1]
    set -xg PETSC_DIR ~/source/petsc
    set -xga LD_LIBRARY_PATH $PETSC_DIR/$PETSC_ARCH/lib
end
