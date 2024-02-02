########################################################################
#
# Main driver for CISL machine support
#
########################################################################

ifneq (,$(wildcard /nasa))
  NEMS_COMPILER?=intel
  $(call add_build_env,pleiades.$(NEMS_COMPILER),env/nasa/pleiades.$(NEMS_COMPILER).mk)
endif
