#
# mk/documentation.mk
#

# Variables
# --------------------------------------------------------------------

DBLATEX_BIN=dblatex
EMACS_BIN=emacs
EMACS_VER=$(shell emacs --version | head -1 | awk  '{ print $$3 }' | awk -F\. '{ print $$1 }')
EMACS_WRAPPER_EL=../emacs.d/cmf-org-export.el
TEXI2PDF_BIN=texi2pdf

# Emacs Evaluation strings
EMACS_CMD_PDF=cmf-export-to-pdf
EMACS_CMD_HTM=cmf-export-to-html
EMACS_CMD_TXT=cmf-export-to-txt
EMACS_CMD_MD=cmf-export-to-markdown
EMACS_CMD_XML=cmf-export-to-docbook
EMACS_CMD_TXI=cmf-export-to-texinfo

# Target Files
HTM_FILES=$(ORG_FILES:.org=.html)
PDF_FILES=$(ORG_FILES:.org=.pdf)
TXT_FILES=$(ORG_FILES:.org=.txt)
XML_FILES=$(ORG_FILES:.org=.xml)
MD_FILES=$(ORG_FILES:.org=.md)
DBK_FILES=$(ORG_FILES:.org=-doc.pdf)
TXI_FILES=$(ORG_FILES:.org=.texi)
TXP_FILES=$(ORG_FILES:.org=-texi.pdf)

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
	@rm -vf ${MD_FILES}
	@rm -vf ${DBK_FILES}
	@rm -vf ${TXI_FILES}
	@rm -vf ${TXP_FILES}
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

${TEXI2PDF_BIN} :
	$(shell if type texi2pdf >/dev/null; then true ; else echo "This target requires texi2pdf"; false; fi)

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
	${EMACS_BIN} -l ${EMACS_WRAPPER_EL} $< --batch -f ${EMACS_CMD_HTM}

#
# Markdown Targets
#

markdown : emacs-org ${MD_FILES}

%.md : %.org
	${EMACS_BIN} -l ${EMACS_WRAPPER_EL} $< --batch -f ${EMACS_CMD_MD}

#
# PDF Targets
#

pdf : emacs-org ${PDF_FILES}

%.pdf : %.org
	${EMACS_BIN} -l ${EMACS_WRAPPER_EL} $< --batch -f ${EMACS_CMD_PDF}

#
# Text/ASCII Targets
#

txt : emacs-org ${TXT_FILES}

%.txt : %.org
	${EMACS_BIN} -l ${EMACS_WRAPPER_EL} $< --batch -f ${EMACS_CMD_TXT}

#
# Docbook XML Targets
#

xml : emacs-org ${XML_FILES}

%.xml : %.org
	${EMACS_BIN} -l ${EMACS_WRAPPER_EL} $< --batch -f ${EMACS_CMD_XML}

docbook: xml ${DBLATEX_BIN} ${DBK_FILES}

%-doc.pdf : %.xml
	${DBLATEX_BIN} -o $@ $<

#
# TexInfo Targets
#

texi : emacs-org ${TXI_FILES}

%.texi : %.org
	${EMACS_BIN} -l ${EMACS_WRAPPER_EL} $< --batch -f ${EMACS_CMD_TXI}

texinfo: texi ${TXP_FILES}

%-texi.pdf : %.texi ${TEXI2PDF_BIN}
	${TEXI2PDF_BIN} -o $@ $<

# Ende
