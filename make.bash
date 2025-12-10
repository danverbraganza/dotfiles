#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
#
# Credit to Michael Smalley for the idea.
# http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/
#
############################

########## Variables

olddir=~/dotfiles_old             # old dotfiles backup directory
files=".bashrc .bash_profile .emacs .gitignore .agignore .config/awesome/rc.lua .config/git/config_shared .config/ghostty/config git-prompt.sh .inputrc .eslintrc .jjconfig.toml .wezterm.lua" # list of files/folders to symlink in homedir

##########

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir $olddir
echo "...done"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks
for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mkdir -p ~/"$(dirname $file)"
    mv ~/$file ~/dotfiles_old/
    echo "Creating symlink to $file in home directory."
    ln -s ~/dotfiles/$file ~/$file
done

ln -s ~/dotfiles/site-lisp /usr/local/share/emacs/site-lisp
mkdir -p ~/.config/git
ln -s ~/dotfiles/.gitignore ~/.config/git/ignore

# If ~/.gitconfig does not end with an include statement for the shared config, add it.
if ! grep -q "\[include\]" ~/.gitconfig; then
	echo "Adding include statement to ~/.gitconfig"
	echo "" >> ~/.gitconfig
	echo "[include]" >> ~/.gitconfig
	echo "    path = ~/.config/git/config_shared" >> ~/.gitconfig
fi
