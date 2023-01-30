#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
export EDITOR=micro
export XCURSOR_SIZE=16
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib64

test -r /home/blackpill0w/.opam/opam-init/init.sh && . /home/blackpill0w/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
