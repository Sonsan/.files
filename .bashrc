#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PATH=$PATH:/usr/bin/python

alias ls='ls --color=auto'
alias gotop='gotop -b -c monokai'
alias restart='shutdown -r now'
alias mutt='neomutt'


echo -e "\e[1m\e[34mHappy \e[92mLeet \e[34mDay\e[0m"
export PS1="\[\e[1m\]\u\[\e[m\]@\[\e[35m\]\h\e[m\]\[\e[32m\][\w]\[\e[m\]\\$"

#PS1='[\u@\h \W]\$ '
