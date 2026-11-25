include ../Makefile

PROJNAME = znzlib

INCFLAGS = $(ZLIB_INC)

SRCS=znzlib.c
OBJS=znzlib.o

depend:
	$(RM) -f depend.mk
	$(MAKE) depend.mk

depend.mk:
	$(CC) $(DEPENDFLAGS) $(INCFLAGS) $(SRCS) >> depend.mk

lib: libznz.a

znzlib.o: znzlib.c znzlib.h
	$(CC) -c $(CFLAGS) $(USEZLIB) $(INCFLAGS) $<

libznz.a: $(OBJS)
	$(AR) -r libznz.a $(OBJS)
	$(RANLIB) $@

include depend.mk
