# Makefile for IRIX and Mipspro 7.3. Thanks to Jan Springer 
# for providing this makefile.

IF(CMAKE_COMPILER_IS_GNUCXX)
   SET(C_LOCALE_SRCS c_locale_glibc/c_locale_glibc.c)
   ADD_DEFINITIONS(-D__STL_USE_NEW_IOSTREAMS)
ELSE(CMAKE_COMPILER_IS_GNUCXX)
   SET(C_LOCALE_SRCS c_locale_stub.cxx)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

SET(IOSTREAM_SRCS 
	iostream.cxx    codecvt.cxx locale.cxx		
	streambuf.cxx   collate.cxx message_facets.cxx	
        strstream.cxx   ctype.cxx   monetary.cxx	
        time_facets.cxx fstream.cxx numeric_facets.cxx	
        ios.cxx         sstream.cxx stdio_streambuf.cxx	
)

SET(COMPLEX_SRCS
	complex.cxx    complex_exp.cxx 			
	complex_io.cxx complex_trig.cxx			
)

ADD_LIBRARY(Cio STATIC ${C_LOCALE_SRCS} ${IOSTREAM_SRCS} ${COMPLEX_SRCS})

INSTALL_TARGETS(/lib/InsightToolkit Cio)

INSTALL_FILES(/include/InsightToolkit/Utilities/stdlib "(\\.h)$")
INSTALL_FILES(/include/InsightToolkit/Utilities/stdlib
              "^(algorithm|bitset|complex|deque|fstream|functional|hash_map)$")
INSTALL_FILES(/include/InsightToolkit/Utilities/stdlib
              "^(hash_set|iomanip|ios|iosfwd|iostream|istream|iterator)$")
INSTALL_FILES(/include/InsightToolkit/Utilities/stdlib
              "^(limits|list|locale|map|memory|numeric|ostream)$")
INSTALL_FILES(/include/InsightToolkit/Utilities/stdlib
              "^(pthread_alloc|queue|rope|set|slist|sstream|stack|stdexcept)$")
INSTALL_FILES(/include/InsightToolkit/Utilities/stdlib
              "^(stdio_streambuf|streambuf|string|strstream|utility)$")
INSTALL_FILES(/include/InsightToolkit/Utilities/stdlib
              "^(valarray|vector)$")

# Subdirectory has an install target.
SUBDIRS(c_locale_glibc)
