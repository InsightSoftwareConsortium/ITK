CC = gcc
CXX = g++
EXPAT = expat
DEBUG = -g
INCLUDES = -I$(EXPAT)/xmlparse -I$(EXPAT)/xmltok

PARSER_OBJS= xmlSourceParser.o \
             xmlConfigurationParser.o \
             internalRep.o \
             configRep.o \
             $(EXPAT)/xmlparse/hashtable.o \
             $(EXPAT)/xmlparse/xmlparse.o \
             $(EXPAT)/xmltok/xmlrole.o \
             $(EXPAT)/xmltok/xmltok.o

GENERATOR_OBJS = $(PARSER_OBJS) \
                 generateWrappers.o \
                 generateTcl.o \
                 displayTree.o

EXECUTABLES = generateWrappers

all: $(EXECUTABLES)

-include generateWrappers.d
-include internalRep.d
-include configRep.d
-include xmlSourceParser.d
-include xmlConfigurationParser.d
-include displayTree.d
-include generateTcl.d

%.d: %.cxx
	$(CXX) $(INCLUDES) -MM $< >$@

%.o: %.cxx
	$(CXX) $(DEBUG) $(INCLUDES) -c $< -o $@

%.o: %.c
	$(CC) $(DEBUG) $(INCLUDES) -c $< -o $@

generateWrappers: $(GENERATOR_OBJS)
	$(CXX) -o generateWrappers $(GENERATOR_OBJS)

clean:
	rm -f *.o *.d

cleanall:
	rm -f $(EXECUTABLES) *.o *.d

