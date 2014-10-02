#
# mk/subclean.mk
#

clean:
	@rm -vf *~
	@rm -vf *.bak
	@rm -vf *-baseline
	@rm -vf *-merge
	@rm -vf *-original

