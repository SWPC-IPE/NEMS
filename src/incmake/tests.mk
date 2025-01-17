SHELL=/bin/sh

ifeq (,$(wildcard /BIN))
  CASELESS_FILESYSTEM=NO
else
  CASELESS_FILESYSTEM=YES
endif

.PHONY: TEST

TEST: $(TARGET)
$(TARGET):
	if [ -f "$(CONFDIR)/modules.nems.lua" ] ; then \
	  $(MODULE_LOGIC) ; \
	fi ; \
	( \
	echo "# Do not edit this file.  It is automatically generated from tests.mk." ; \
	echo "CASELESS_FILESYSTEM=$(CASELESS_FILESYSTEM)" ; \
	if [ "Q$$ESMFMKFILE" != Q ] ; then egrep 'VERSION|^ESMF_[A-Z]*DIR' < "$$ESMFMKFILE" ; fi ; \
	) > "$@"
