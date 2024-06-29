#
# mk/subclean.mk
#
# Created Sun Aug  3 19:46:31 2014 PST
#

RM_OPTS=$(shell if [ `uname` != "OpenBSD" ]; then echo '-vf'; else echo '-f'; fi)

clean:
	@rm ${RM_OPTS} *~
	@rm ${RM_OPTS} *.elc
	@rm ${RM_OPTS} *.bak
	@rm ${RM_OPTS} *-baseline*
	@rm ${RM_OPTS} *-merge*
	@rm ${RM_OPTS} *-original*

