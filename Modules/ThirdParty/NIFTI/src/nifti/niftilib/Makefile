include ../Makefile

PROJNAME = niftiio

TAR     ?= /bin/tar
RM      ?= /bin/rm

INCFLAGS = $(ZLIB_INC) $(ZNZ_INC)
LIBS = $(ZLIB_LIBS) $(ZNZ_LIBS) $(NIFTI_LIBS)

SRCS=nifti1_io.c
OBJS=nifti1_io.o
MOBJS=mjtest.o

SCRIPTS =

depend:
	$(RM) -f depend.mk
	$(MAKE) depend.mk

depend.mk:
	$(CC) $(DEPENDFLAGS) $(INCFLAGS) $(SRCS) >> depend.mk

lib: libniftiio.a

nifti1_io.o: nifti1_io.c nifti1_io.h
	$(CC) -c $(CFLAGS) $(USEZLIB) $(INCFLAGS) $<

libniftiio.a: $(OBJS)
	$(AR) -r libniftiio.a $(OBJS)
	$(RANLIB) $@

nifti_tool: nifti_tool.c nifti_tool.h
	$(CC) $(CFLAGS) -Wall -o nifti_tool nifti_tool.c $(NIFTI_INCS) $(ZNZ_INCS) $(NIFTI_LIBS) $(ZNZ_LIBS)

nifti1_test: nifti1_test.c
	$(CC) $(CFLAGS) -o nifti1_test nifti1_test.c $(NIFTI_INCS) $(ZNZ_INCS) $(NIFTI_LIBS) $(ZNZ_LIBS)

# get the regression testing data tree
regress_data: nifti_regress_test/nifti_regress_data

# remove any result directories
regress_clean:
	$(RM) -fr nifti_regress_test/results*

# remove any result directories, and remove the data tree
regress_clean_all: regress_clean
	$(RM) -fr nifti_regress_test/nifti_regress_data

# download and unpack the regression data tree
nifti_regress_test/nifti_regress_data:
	wget http://nifti.nimh.nih.gov/pub/dist/data/nifti_regress_data.tgz
	( cd nifti_regress_test; $(TAR) -xzf ../nifti_regress_data.tgz; )
	$(RM) nifti_regress_data.tgz

include depend.mk
