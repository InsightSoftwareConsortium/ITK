ifndef vnl_client_mk
vnl_client_mk := 1
#
# fsm@robots.ox.ac.uk
#

USE_NETLIB := 1

include $(IUELOCALROOT)/vcl/client.mk

ald_libdeps += vnl:
ald_libdeps += vnl-algo:vnl,netlib

endif
