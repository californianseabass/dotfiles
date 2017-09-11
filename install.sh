# bash_profile=$HOME/.bash_profile
gitignore_global=$HOME/.gitignore_global
tmux_conf=$HOME/.tmux.conf
tmuxinator=$HOME/.tmuxinator

while getopts "r" opt
do
    case "$opt" in
        r)
            echo "choosing to reinstall";
            # rm $bash_profile;
            rm $tmux_conf;
            rm -rf $tmuxinator;
            rm gitignore_global;
            ;;
        *)
            echo "Wrong command line argument. Exiting...";
            exit 1
            ;;
    esac
done

ln -s $PWD/gitignore_global $gitignore_global
ln -s $PWD/tmux_conf $tmux_conf
# ln -s $PWD/bash_profile $bash_profile
ln -s $PWD/tmuxinator/ $tmuxinator
