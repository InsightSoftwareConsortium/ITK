# Microsoft Developer Studio Generated NMAKE File
!IF "$(CFG)" == ""
CFG=libCio - Win32 Debug
!MESSAGE No configuration specified. Defaulting to libCio - Win32 Debug.
!ENDIF 


#
#  NOTE: When building an application using libCio, specify the
#  /X flag to turn off the standard includes. Add a -I for
#  the libCio include directory, and also add a -I for
#  the VC98 include directory (typically this is
#  "/program files/microsoft visual studio/vc98/include")
#


!IF "$(CFG)" != "libCio - Win32 Release" && "$(CFG)" != "libCio - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libCio.mak" CFG="libCio - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libCio - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libCio - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libCio - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\libCio.lib"


CLEAN :
	-@erase "$(INTDIR)\c_locale_stub.obj"
	-@erase "$(INTDIR)\codecvt.obj"
	-@erase "$(INTDIR)\collate.obj"
	-@erase "$(INTDIR)\ctype.obj"
	-@erase "$(INTDIR)\fstream.obj"
	-@erase "$(INTDIR)\ios.obj"
	-@erase "$(INTDIR)\iostream.obj"
	-@erase "$(INTDIR)\locale.obj"
	-@erase "$(INTDIR)\message_facets.obj"
	-@erase "$(INTDIR)\monetary.obj"
	-@erase "$(INTDIR)\numeric_facets.obj"
	-@erase "$(INTDIR)\range_errors.obj"
	-@erase "$(INTDIR)\sstream.obj"
	-@erase "$(INTDIR)\stdio_streambuf.obj"
	-@erase "$(INTDIR)\streambuf.obj"
	-@erase "$(INTDIR)\strstream.obj"
	-@erase "$(INTDIR)\time_facets.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\libCio.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /w /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "__STL_USE_NEW_IOSTREAMS" /D "_WINDOWS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD -I../include /GZ /GR /c 

BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libCio.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libCio.lib" 
LIB32_OBJS= \
	"$(INTDIR)\time_facets.obj" \
	"$(INTDIR)\codecvt.obj" \
	"$(INTDIR)\collate.obj" \
	"$(INTDIR)\ctype.obj" \
	"$(INTDIR)\fstream.obj" \
	"$(INTDIR)\ios.obj" \
	"$(INTDIR)\iostream.obj" \
	"$(INTDIR)\locale.obj" \
	"$(INTDIR)\message_facets.obj" \
	"$(INTDIR)\monetary.obj" \
	"$(INTDIR)\numeric_facets.obj" \
	"$(INTDIR)\range_errors.obj" \
	"$(INTDIR)\sstream.obj" \
	"$(INTDIR)\stdio_streambuf.obj" \
	"$(INTDIR)\streambuf.obj" \
	"$(INTDIR)\strstream.obj" \
	"$(INTDIR)\c_locale_stub.obj"

"$(OUTDIR)\libCio.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libCio - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\libCio.lib"


CLEAN :
	-@erase "$(INTDIR)\c_locale_stub.obj"
	-@erase "$(INTDIR)\codecvt.obj"
	-@erase "$(INTDIR)\collate.obj"
	-@erase "$(INTDIR)\ctype.obj"
	-@erase "$(INTDIR)\fstream.obj"
	-@erase "$(INTDIR)\ios.obj"
	-@erase "$(INTDIR)\iostream.obj"
	-@erase "$(INTDIR)\locale.obj"
	-@erase "$(INTDIR)\message_facets.obj"
	-@erase "$(INTDIR)\monetary.obj"
	-@erase "$(INTDIR)\numeric_facets.obj"
	-@erase "$(INTDIR)\range_errors.obj"
	-@erase "$(INTDIR)\sstream.obj"
	-@erase "$(INTDIR)\stdio_streambuf.obj"
	-@erase "$(INTDIR)\streambuf.obj"
	-@erase "$(INTDIR)\strstream.obj"
	-@erase "$(INTDIR)\time_facets.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\libCio.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /w /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "__STL_USE_NEW_IOSTREAMS" /D "_WINDOWS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD -I../include /GZ /GR /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libCio.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\libCio.lib" 
LIB32_OBJS= \
	"$(INTDIR)\time_facets.obj" \
	"$(INTDIR)\codecvt.obj" \
	"$(INTDIR)\collate.obj" \
	"$(INTDIR)\ctype.obj" \
	"$(INTDIR)\fstream.obj" \
	"$(INTDIR)\ios.obj" \
	"$(INTDIR)\iostream.obj" \
	"$(INTDIR)\locale.obj" \
	"$(INTDIR)\message_facets.obj" \
	"$(INTDIR)\monetary.obj" \
	"$(INTDIR)\numeric_facets.obj" \
	"$(INTDIR)\range_errors.obj" \
	"$(INTDIR)\sstream.obj" \
	"$(INTDIR)\stdio_streambuf.obj" \
	"$(INTDIR)\streambuf.obj" \
	"$(INTDIR)\strstream.obj" \
	"$(INTDIR)\c_locale_stub.obj"

"$(OUTDIR)\libCio.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("libCio.dep")
!INCLUDE "libCio.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libCio - Win32 Release" || "$(CFG)" == "libCio - Win32 Debug"

SOURCE=.\c_locale_stub.cxx

"$(INTDIR)\c_locale_stub.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\codecvt.cxx

"$(INTDIR)\codecvt.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\collate.cxx

"$(INTDIR)\collate.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\ctype.cxx

"$(INTDIR)\ctype.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\fstream.cxx

"$(INTDIR)\fstream.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\ios.cxx

"$(INTDIR)\ios.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\iostream.cxx

"$(INTDIR)\iostream.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\locale.cxx

"$(INTDIR)\locale.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\message_facets.cxx

"$(INTDIR)\message_facets.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\monetary.cxx

"$(INTDIR)\monetary.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\numeric_facets.cxx

"$(INTDIR)\numeric_facets.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\range_errors.cxx

"$(INTDIR)\range_errors.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\sstream.cxx

"$(INTDIR)\sstream.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\stdio_streambuf.cxx

"$(INTDIR)\stdio_streambuf.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\streambuf.cxx

"$(INTDIR)\streambuf.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\strstream.cxx

"$(INTDIR)\strstream.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\time_facets.cxx

"$(INTDIR)\time_facets.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

