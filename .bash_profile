export BASH_SILENCE_DEPRECATION_WARNING=1

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi
. "$HOME/.cargo/env"

. "$HOME/.local/bin/env"
