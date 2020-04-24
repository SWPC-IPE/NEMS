# Location of the ESMF makefile fragment for this component:
datapoll_mk = $(DATAPOLL_BINDIR)/datapoll.mk
all_component_mk_files+=$(datapoll_mk)

# Location of source code and installation
DATAPOLL_SRCDIR?=$(ROOTDIR)/DATAPOLL
DATAPOLL_BINDIR?=$(ROOTDIR)/DATAPOLL/DATAPOLL_INSTALL

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(DATAPOLL_SRCDIR),DATAPOLL source directory)

ifneq (,$(findstring DEBUG=Y,$(DATAPOLL_MAKEOPT)))
  override DATAPOLL_CONFOPT += --enable-debug
endif

########################################################################

# Rule for building this component:

build_DATAPOLL: $(datapoll_mk)

$(datapoll_mk): configure
	set -e
	+$(MODULE_LOGIC) ; cd $(DATAPOLL_SRCDIR)                   ; \
	test -r Makefile || exec ./configure                     \
	  --prefix=$(DATAPOLL_BINDIR)                                \
	  --datarootdir=$(DATAPOLL_BINDIR) --libdir=$(DATAPOLL_BINDIR)   \
          $(DATAPOLL_CONFOPT)
	+$(MODULE_LOGIC) ; cd $(DATAPOLL_SRCDIR) ; exec $(MAKE)
	+$(MODULE_LOGIC) ; cd $(DATAPOLL_SRCDIR) ; exec $(MAKE) install
	test -d "$(DATAPOLL_BINDIR)"

########################################################################

# Rule for cleaning the SRCDIR and BINDIR:

clean_DATAPOLL:
	+cd $(DATAPOLL_SRCDIR) ; test -f Makefile && exec $(MAKE) -k distclean || echo "Nothing to clean up"

distclean_DATAPOLL: clean_DATAPOLL
	rm -rf $(DATAPOLL_BINDIR) $(datapoll_mk)
