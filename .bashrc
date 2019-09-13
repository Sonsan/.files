#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PATH=$PATH:/usr/bin/python

alias ls='ls --color=auto'
alias gotop='gotop -b -c monokai'
alias restart='shutdown -r now'

# get current branch in git repo
function git_branch() {
	BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
	if [ ! "${BRANCH}" == "" ]
	then
		echo -e "\e[93m{\e[0m\e[41m${BRANCH}\e[0m\e[93m}\e[0m"
	else
		echo ""
	fi
}

echo -e "\e[1m\e[34mHappy \e[92mLeet \e[34mDay\e[0m"
export PS1="\[\e[1m\]\u\[\e[m\]@\[\e[35m\]\h\e[m\]\[\e[32m\][\w]\[\e[m\]\`git_branch\`\\$ "

#PS1='[\u@\h \W]\$ '
