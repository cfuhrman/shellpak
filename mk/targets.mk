#
# mk/targets.mk
#

# Variables
# --------------------------------------------------------------------

EMACS_BIN=emacs
EMACS_VER=$(shell emacs --version | head -1 | awk  '{ print $$3 }' | awk -F\. '{ print $$1 }')

# Emacs Evaluation strings
EMACS_CMD_PDF=org-export-as-pdf
EMACS_CMD_HTM=org-export-as-html
EMACS_CMD_TXT=$(shell if [ ${EMACS_VER} -ge 24 ]; then echo 'org-export-as-utf8'; else echo 'org-export-as-ascii'; fi)

# Target Files
HTM_FILES=$(ORG_FILES:.org=.html)
PDF_FILES=$(ORG_FILES:.org=.pdf)
TXT_FILES=$(ORG_FILES:.org=.txt)

# Targets
# --------------------------------------------------------------------

#
# Catch all target
#

all : html pdf txt

clean :
	@rm -vf ${HTM_FILES}
	@rm -vf ${PDF_FILES}
	@rm -vf $(ORG_FILES:.org=.tex)
	@rm -vf ${TXT_FILES}
	@rm -vf *~
	@rm -vf *.toc
	@rm -vf *.out
	@rm -vf *.aux
	@rm -vf *.log
	@rm -vf *-baseline
	@rm -vf *-merge
	@rm -vf *-original

#
# Determines if emacs supports org-mode
#

emacs-org:
	$(shell if [ ${EMACS_VER} -ge 23 ]; then true ; else echo "Emacs version (v${EMACS_VER}) is too old.  Consider upgrading"; false; fi)

#
# HTML Targets
#

html : ${HTM_FILES} emacs-org

%.html : %.org
	${EMACS_BIN} $< --batch -f ${EMACS_CMD_HTM}

#
# PDF Targets
#

pdf : ${PDF_FILES} emacs-org

%.pdf : %.org
	${EMACS_BIN} $< --batch -f ${EMACS_CMD_PDF}

#
# Text/ASCII Targets
#

txt : ${TXT_FILES} emacs-org

%.txt : %.org
	${EMACS_BIN} $< --batch -f ${EMACS_CMD_TXT}
