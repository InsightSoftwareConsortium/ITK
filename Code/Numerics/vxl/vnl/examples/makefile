USES += vxl
USE_NETLIB := 1

IULIBS := -lvnl-algo -lvbl -lvnl -lnetlib

MINI_PROG_SOURCES += time_fastops.cxx
MINI_PROG_SOURCES += vnl_amoeba.cxx
#MINI_PROG_SOURCES += vnl-calc.cxx
MINI_PROG_SOURCES += vnl_complex_svd.cxx
MINI_PROG_SOURCES += vnl_planefit.cxx
MINI_PROG_SOURCES += vnl_polyroots.cxx
MINI_PROG_SOURCES += vnl_rosenbrock.cxx
MINI_PROG_SOURCES += vnl_svd.cxx
MINI_PROG_SOURCES += vnl_lsqr_test.cxx

################################
include ${IUEROOT}/config/top.mk
################################

#all: $(RELOBJDIR)/time_fastops.40

$(RELOBJDIR)/time_fastops.40: time_fastops.cxx time_fastops.mk
	$(SHELL) time_fastops.mk

speeds: $(RELOBJDIR)/time_fastops_O_M1

$(RELOBJDIR)/time_fastops_O_M1: time_fastops.cxx
	$(COMPILE.c++) -O2 -DOPTIMIZED -DMETHOD=1 $<
	$(MAKE_EXE)
	$@
