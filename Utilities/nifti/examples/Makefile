CC	=	gcc
CFLAGS	=	-ansi -pedantic

FSLIO_INCS	=	-I../include
NIFTI_INCS	=	-I../include
ZNZ_INCS	=	-I/usr/include

FSLIO_LIBS	=	-L../lib -lfslio
NIFTI_LIBS	=	-L../lib -lniftiio
ZNZ_LIBS	=	-L/usr/lib -L../lib -lznz -lm -lz



## SGI 32bit
ifeq ($(ARCH),SGI)
ZNZ_INCS	=	-I/usr/freeware/include
ZNZ_LIBS	=	-L/usr/freeware/lib32 -L../lib -lznz -lm -lz
endif


## Redhat Fedora Linux
##ZNZ_INCS	=	-I/usr/include
##ZNZ_LIBS	=	-L/usr/lib -L../lib -lznz -lm -lz




all:	fsl_api_driver

clean:
	rm -f fsl_api_driver

fsl_api_driver:	fsl_api_driver.c ../lib/libfslio.a
	$(CC) $(CFLAGS) -o fsl_api_driver fsl_api_driver.c $(FSLIO_INCS) $(NIFTI_INCS) $(ZNZ_INCS) $(FSLIO_LIBS) $(NIFTI_LIBS) $(ZNZ_LIBS)

help:
	@echo "all:      make the fsl_api_driver program"
	@echo "clean:    rm the fsl_api_driver program"
