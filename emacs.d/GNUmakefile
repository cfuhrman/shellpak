#
# emacs.d/GNUmakefile
#
# Created Mon Aug  4 02:23:07 2014 UTC
#

SUBDIRS+=lisp
SUBDIRS+=thirdparty

# Files
ORG_FILES=EMACS-README.org

CLEANDIRS=$(SUBDIRS:%=clean-%)

RM_OPTS=$(shell if [ `uname` != "OpenBSD" ]; then echo '-vf'; else echo '-f'; fi)

clean: ${CLEANDIRS}
	@rm ${RM_OPTS} *~
	@rm ${RM_OPTS} *.bak
	@rm ${RM_OPTS} *-baseline*
	@rm ${RM_OPTS} *-merge*
	@rm ${RM_OPTS} *-original*
	@rm ${RM_OPTS} *.elc

${CLEANDIRS} :
	${MAKE} -C $(@:clean-%=%) clean

include ../mk/documentation.mk

# emacs.d/GNUmakefile ends here
