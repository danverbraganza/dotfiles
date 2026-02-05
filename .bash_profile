export BASH_SILENCE_DEPRECATION_WARNING=1

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi
. "$HOME/.cargo/env"

if [ -f "$HOME/.local/bin/env" ]; then
    . "$HOME/.local/bin/env"
fi

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
