# Makefile for IRIX and Mipspro 7.3. Thanks to Jan Springer 
# for providing this makefile.

IF(CMAKE_COMPILER_IS_GNUCXX)
   SOURCE_FILES(C_LOCALE_SRCS c_locale_glibc/c_locale_glibc.c)
   ADD_DEFINITIONS(-D__STL_USE_NEW_IOSTREAMS)
ELSE(CMAKE_COMPILER_IS_GNUCXX)
   SOURCE_FILES(C_LOCALE_SRCS c_locale_stub.cxx)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

SOURCE_FILES(IOSTREAM_SRCS 
	iostream.cxx    codecvt.cxx locale.cxx		
	streambuf.cxx   collate.cxx message_facets.cxx	
        strstream.cxx   ctype.cxx   monetary.cxx	
        time_facets.cxx fstream.cxx numeric_facets.cxx	
        ios.cxx         sstream.cxx stdio_streambuf.cxx	
)

SOURCE_FILES(COMPLEX_SRCS
	complex.cxx    complex_exp.cxx 			
	complex_io.cxx complex_trig.cxx			
)

ADD_LIBRARY(Cio STATIC C_LOCALE_SRCS IOSTREAM_SRCS COMPLEX_SRCS)

