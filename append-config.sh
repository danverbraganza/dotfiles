#! /usr/bin/env bash

set -uex pipefail

# If ~/.gitconfig does not end with an include statement for the shared config, add it.
if ! grep -q "\[include\]" ~/.gitconfig; then
	echo "Adding include statement to ~/.gitconfig"
	echo "" >> ~/.gitconfig
	echo "[include]" >> ~/.gitconfig
	echo "    path = ~/.config/git/config_shared" >> ~/.gitconfig
fi
