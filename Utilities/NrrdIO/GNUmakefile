
### For the time being, this will have to do as a makefile for NrrdIO

### These have to be set to reflect the current platform:
###
### -DTEEM_DIO=0, -DTEEM_DIO=1: This platform can (1) or cannot (0) do
### DirectIO, which is the fast way to do multi-gigabyte I/O.
### Currently, only available on SGIs.
###
### -DTEEM_32BIT=0, -DTEEM_32BIT=1: This platform is a 32-bit (1) or a
### 64-bit machine (0)
###
### -DTEEM_ENDIAN=4321, -DTEEM_ENDIAN=1234: The platform is big-endian
### (4321) or little-endian (1234)
###
### -DTEEM_QNANHIBIT=1, -DTEEM_QNANHIBIT=0: The 23nd bit of a 32-bit
### quiet-NaN is either 1 (1) or 0 (0).  This is needed as part of
### handling IEEE floating point special values.  This quantity is
### independent of endianness.
###
### -DTEEM_BIGBITFIELD=0, -DTEEM_BIGBITFIELD=1: This platform can (1)
### or cannot (0) handle bit-fields larger than 32 bits.  This is also
### needed for doing IEEE floating point special values.
###
PLATFORM_DEFS = \
  -DTEEM_DIO=0 \
  -DTEEM_32BIT=1 \
  -DTEEM_ENDIAN=4321 \
  -DTEEM_QNANHIBIT=0 \
  -DTEEM_BIGBITFIELD=1

### Any architecture-specific flags to cc
###
CCFLAGS =

### This also has to be set per-architecture- whether or not we need to
### run ranlib on libraries created via ar
###
RANLIB = ranlib

### Assuming NrrdIO will be built with zlib enabled (due to "-DTEEM_ZLIB=1"
### on the source compilation, below), these (may) need to be set to help 
### find the zlib includes and libraries
###
ZLIB_IPATH =
ZLIB_LPATH =

### We'll build the static libNrrdIO library, and one test program
###
ALL = libNrrdIO.a sampleIO
all: $(ALL)

### The libNrrdIO library is built from the objects from the source files
### named in NrrdIO_Srcs.txt
###
libNrrdIO.a : $(patsubst %.c,%.o,$(shell cat NrrdIO_Srcs.txt))
	ar ru $@ $^
	$(if $(RANLIB),$(RANLIB) $@,)

### Compiling the source files will also have some platform-specific stuff
###
%.o : %.c
	cc -O2 $(CCFLAGS) $(PLATFORM_DEFS) \
          -DTEEM_ZLIB=1 $(ZLIB_IPATH) -c $^ -o $@

### this creates the sampleIO program
###
sampleIO : sampleIO.c
	cc -O2 $(CCFLAGS) $(PLATFORM_DEFS) -DTEEM_ZLIB=1 $(ZLIB_IPATH) \
	$^ -o $@ -L. -lNrrdIO $(ZLIB_LPATH) -lz -lm

### how to clean up
###
clean :
	rm -f *.o $(ALL)
