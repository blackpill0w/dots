#
# ~/.bash_profile
#

export EDITOR=micro
export XCURSOR_SIZE=16
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin:/usr/local/bin/:~/.local/bin:~/.config/emacs/bin:~/.cargo/bin:~/.cabal/bin"
export QT_QPA_PLATFORMTHEME=gnome
export BAT_THEME=gruvbox-dark

[[ -f ~/.bashrc ]] && . ~/.bashrc

# opam configuration
test -r /home/blackpill0w/.opam/opam-init/init.sh && . /home/blackpill0w/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
