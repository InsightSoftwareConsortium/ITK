CC = gcc
CXX = g++
EXPAT = expat
DEBUG = -g
INCLUDES = -I$(EXPAT)/xmlparse -I$(EXPAT)/xmltok

PARSER_OBJS= parseSourceXML.o \
             parseConfigXML.o \
             parseUtils.o \
             internalRep.o \
             $(EXPAT)/xmlparse/hashtable.o \
             $(EXPAT)/xmlparse/xmlparse.o \
             $(EXPAT)/xmltok/xmlrole.o \
             $(EXPAT)/xmltok/xmltok.o

GENERATOR_OBJS = $(PARSER_OBJS) \
                 generateWrappers.o \
                 generateTcl.o \
                 displayTree.o

all: generateWrappers

-include generateWrappers.d
-include internalRep.d
-include parseSourceXML.d
-include parseConfigXML.d
-include parseUtils.d
-include displayTree.d
-include generateTcl.d

%.d: %.cxx
	$(CXX) $(INCLUDES) -M $< >$@

%.o: %.cxx
	$(CXX) $(DEBUG) $(INCLUDES) -c $<

generateWrappers: $(GENERATOR_OBJS)
	$(CXX) -o generateWrappers $(GENERATOR_OBJS)

clean:
	rm -f *.o *.d

cleanall:
	rm -f generateWrappers *.o *.d

test:   generateWrappers
	./generateWrappers Test/test.xml -display

tcltest:   generateWrappers
	./generateWrappers Test/test.xml -tcl

