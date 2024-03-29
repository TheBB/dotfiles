# Remove the welcome message
set fish_greeting

# Prompt settings
set -g ___fish_git_prompt_color (set_color yellow)
set -g __fish_git_prompt_showdirtystate true
set -g __fish_git_prompt_showupstream auto
set -g fish_prompt_pwd_dir_length 3

# Key bindings
bind \cl forward-char
bind \cr expand_glob

# Use vim as EDITOR
set -xg EDITOR vim

# None of that annoying blue-on-gray in 'ls'
set LS_COLORS 'rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=30;41:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:'

# Path manipulation
set PATH $HOME/.local/bin $HOME/.cargo/bin $PATH

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/eivind/source/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<


alias pic="feh -Z."


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
    echo '' > ~/marked.txt
end

function cleardir
    if test (basename $PWD) != "build"
        echo "Wrong directory"
        return 1
    end
    rm -rf *
end

function petsc
    set -xg PETSC_ARCH $argv[1]
    set -xg PETSC_DIR ~/source/petsc
    set -xga LD_LIBRARY_PATH $PETSC_DIR/$PETSC_ARCH/lib
end

function backup
    rsync -avxx -e "ssh -F $HOME/.ssh/config" $HOME rsync@mnemosyne::NetBackup/home-(date '+%Y-%m-%d') 2>&1 | tee $HOME/backup.log
end

function wacom
    xsetwacom --set "Wacom Bamboo Connect Pen stylus" MapToOutput HEAD-1
    xsetwacom --set "Wacom Bamboo Connect Pen stylus" Rotate half
end

function blue
    switch $argv[1]
        case spinup
            set -l name $argv[2]
            set -l size $argv[3]
            az group create \
                --name AutoGrp-$name \
                --location westeurope
            az vm create \
                --resource-group AutoGrp-$name \
                --name AutoVm-$name \
                --image UbuntuLTS \
                --ssh-key-values ~/.ssh/id_rsa.pub \
                --size $size

            set -l ip ( \
                az vm list-ip-addresses --name AutoVm-$name | \
                jq -r '.[0].virtualMachine.network.publicIpAddresses[0].ipAddress' \
            )
            ssh-keyscan -H $ip >> ./.ssh/known_hosts
            if grep "Host $name" ~/.ssh/config
                cat ~/.ssh/config | \
                    tr '\n' '#' | \
                    sed -e "s/\(Host $name#\s*HostName\) [^#]*\(#\s*User\) [^#]*/\1 $ip\2 $USER/" | \
                    tr '#' '\n' > ~/.ssh/temp
                mv ~/.ssh/temp ~/.ssh/config
            else
                echo "Host $name" >> ~/.ssh/config
                echo "    HostName $ip" >> ~/.ssh/config
                echo "    User $USER" >> ~/.ssh/config
            end

        case ifem
            set -l name $argv[2]

            ssh $name '\
                curl -fsSL https://get.docker.com -o get-docker.sh
                sudo sh get-docker.sh
                rm get-docker.sh
                sudo usermod -aG docker $USER
                echo "localhost slots=$(nproc)" > hostfile
                echo >> .bashrc \'ifem () { docker run --cap-add SYS_PTRACE -v$(pwd):/workdir --workdir /workdir thebb/ifem bash -c "$*"; }\'
            '

            ssh $name 'docker pull thebb/ifem'

        case spindown
            set -l name $argv[2]
            set -l ip ( \
                az vm list-ip-addresses --name AutoVm-$name | \
                jq -r '.[0].virtualMachine.network.publicIpAddresses[0].ipAddress' \
            )

            yes | ssh-keygen -R $ip
            az group delete --yes --name AutoGrp-$name
    end
end
