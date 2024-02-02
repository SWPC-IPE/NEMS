########################################################################
#
# Main driver for CISL machine support
#
########################################################################

ifneq (,$(wildcard /glade))
  NEMS_COMPILER?=intel
  $(call add_build_env,derecho.$(NEMS_COMPILER),env/cisl/derecho.$(NEMS_COMPILER).mk)
endif
