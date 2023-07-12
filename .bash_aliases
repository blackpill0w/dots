alias cl='printf "\E[H\E[3J" && clear'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'
# grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
# ls
alias ls='ls --color=auto'
alias lla='ls -lAh'
alias ll='ls -lh'
alias la='ls -Ah'
alias l='ls -CFh'
# protonvpn
alias vpn='protonvpn-cli d; protonvpn-cli c'
alias vpnf='protonvpn-cli d; protonvpn-cli c -f'
alias vpnd='protonvpn-cli d'

alias grub-update='sudo grub-mkconfig -o /boot/grub/grub.cfg'
alias paru='paru --skipreview'

alias temacs='emacs -nw -q --load ~/.config/doom/terminal_config.el'
