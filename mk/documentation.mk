#
# mk/documentation.mk
#

# Variables
# --------------------------------------------------------------------

DBLATEX_BIN=dblatex
EMACS_BIN=emacs
EMACS_VER=$(shell emacs --version | head -1 | awk  '{ print $$3 }' | awk -F\. '{ print $$1 }')

# Emacs Evaluation strings
EMACS_CMD_PDF=org-export-as-pdf
EMACS_CMD_HTM=org-export-as-html
EMACS_CMD_TXT=$(shell if [ ${EMACS_VER} -ge 24 ]; then echo 'org-export-as-utf8'; else echo 'org-export-as-ascii'; fi)
EMACS_CMD_XML=org-export-as-docbook

# Target Files
HTM_FILES=$(ORG_FILES:.org=.html)
PDF_FILES=$(ORG_FILES:.org=.pdf)
TXT_FILES=$(ORG_FILES:.org=.txt)
XML_FILES=$(ORG_FILES:.org=.xml)
DBK_FILES=$(ORG_FILES:.org=-doc.pdf)

# Targets
# --------------------------------------------------------------------

#
# Catch all target
#

all : html pdf txt

clean :
	@rm -vf ${HTM_FILES}
	@rm -vf ${PDF_FILES}
	@rm -vf ${TXT_FILES}
	@rm -vf ${XML_FILES}
	@rm -vf ${DBK_FILES}
	@rm -vf $(ORG_FILES:.org=.tex)
	@rm -vf $(ORG_FILES:.org=.toc)
	@rm -vf $(ORG_FILES:.org=.out)
	@rm -vf $(ORG_FILES:.org=.aux)
	@rm -vf $(ORG_FILES:.org=.xml)
	@rm -vf *.log
	@rm -vf *.bak
	@rm -vf *-baseline
	@rm -vf *-merge
	@rm -vf *-original
	@rm -vf *~

# Check dblatex

${DBLATEX_BIN} :
	$(shell if type dblatex >/dev/null; then true ; else echo "This target requires dblatex"; false; fi)

#
# Determines if emacs supports org-mode
#

emacs-org:
	$(shell if [ ${EMACS_VER} -ge 23 ]; then true ; else echo "Emacs version (v${EMACS_VER}) is too old.  Consider upgrading"; false; fi)

#
# HTML Targets
#

html : emacs-org ${HTM_FILES}

%.html : %.org
	${EMACS_BIN} $< --batch -f ${EMACS_CMD_HTM}

#
# PDF Targets
#

pdf : emacs-org ${PDF_FILES}

%.pdf : %.org
	${EMACS_BIN} $< --batch -f ${EMACS_CMD_PDF}

#
# Text/ASCII Targets
#

txt : emacs-org ${TXT_FILES}

%.txt : %.org
	${EMACS_BIN} $< --batch -f ${EMACS_CMD_TXT}

#
# Docbook XML Targets
#

xml : emacs-org ${XML_FILES}

%.xml : %.org
	${EMACS_BIN} $< --batch -f ${EMACS_CMD_XML}

docbook: dblatex ${DBK_FILES}

%-doc.pdf : %.xml
	${DBLATEX_BIN} -o $@ $<

# Ende
