
# note the TARFILE_NAME embeds the release version number
TARFILE_NAME	= nifti2clib-0.0.1

USEZLIB         = -DHAVE_ZLIB

## Compiler  defines
CC		= gcc
IFLAGS          = -I. -I../niftilib -I../znzlib
CFLAGS          = -Wall -std=gnu99 -pedantic $(USEZLIB) $(IFLAGS)

LLIBS 		= -lz -lm

MISC_OBJS	= nifticdf.o znzlib.o
OBJS	   	= nifti2_io.o $(MISC_OBJS)

TOOLS 	   	= nifti_tool nifti1_tool nifti2_tool
EXAMPLES   	= clib_02_nifti2


# --------------------------------------------------
# default compile for C files
%.o : %.c %.h
	$(CC) -c $(CFLAGS) $< -o $@

# --------------------------------------------------
# main targets (primary is nifti_tool, for now)

nifti_tool: nifti_tool.o nifti_tool.h nifti2objs
	$(CC) -o $@ $(CFLAGS) $< $(OBJS) $(LLIBS)

all: $(TOOLS) $(EXAMPLES)

clean:
	$(RM) *.o $(TOOLS) $(EXAMPLES)

nifti2objs: $(OBJS)

clib_02_nifti2: clib_02_nifti2.o nifti2objs
	$(CC) -o $@ $(CFLAGS) $< $(OBJS) $(LLIBS)

# --------------------------------------------------
# targets from source residing elsewhere
nifticdf.o:
	$(CC) -c $(CFLAGS) ../nifticdf/nifticdf.c -o $@

znzlib.o:
	$(CC) -c $(CFLAGS) ../znzlib/znzlib.c -o $@

# maybe we want to build NIFTI-1 nifti_tool
nifti1_tool: $(MISC_OBJS)
	$(CC) -o $@ -I../utils $(CFLAGS) \
	../niftilib/nifti_tool.c ../niftilib/nifti1_io.c $(MISC_OBJS) $(LLIBS)

# make non-clashing version
nifti2_tool: nifti_tool
	cp -p nifti_tool nifti2_tool
