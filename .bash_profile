#
# ~/.bash_profile
#

export EDITOR=nvim
export XCURSOR_SIZE=16
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib64
export JAVA_HOME=/usr/lib/jvm/default
export PATH="$PATH:/sbin:/usr/sbin:/usr/local/sbin:/usr/local/bin/:~/.local/bin:~/.config/emacs/bin:~/.cargo/bin"
export QT_QPA_PLATFORMTHEME=gnome
export BAT_THEME=gruvbox-dark

test -r /home/blackpill0w/.opam/opam-init/init.sh && . /home/blackpill0w/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

[[ -f ~/.bashrc ]] && . ~/.bashrc
