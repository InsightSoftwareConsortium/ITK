# Microsoft Developer Studio Project File - Name="vcl" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=vcl - Win32 DebugSTLPort
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "vcl.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "vcl.mak" CFG="vcl - Win32 DebugSTLPort"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "vcl - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "vcl - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "vcl - Win32 StaticDebug" (based on "Win32 (x86) Static Library")
!MESSAGE "vcl - Win32 StaticRelease" (based on "Win32 (x86) Static Library")
!MESSAGE "vcl - Win32 DebugSTLPort" (based on "Win32 (x86) Static Library")
!MESSAGE "vcl - Win32 ReleaseSTLPort" (based on "Win32 (x86) Static Library")
!MESSAGE "vcl - Win32 ReleaseWithDBInfo" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
RSC=rc.exe

!IF  "$(CFG)" == "vcl - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)/vcl" /I "$(IUEROOT)\vcl" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /Zl /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\Release\vcl.lib"

!ELSEIF  "$(CFG)" == "vcl - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "vcl___Win32_Debug"
# PROP BASE Intermediate_Dir "vcl___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /Zl /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\Debug\vcl.lib"

!ELSEIF  "$(CFG)" == "vcl - Win32 StaticDebug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "vcl___Win32_StaticDebug"
# PROP BASE Intermediate_Dir "vcl___Win32_StaticDebug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "StaticDebug"
# PROP Intermediate_Dir "StaticDebug"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /Zl /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo /out:"..\Debug\vcl.lib"
# ADD LIB32 /nologo /out:"..\StaticDebug\vcl.lib"

!ELSEIF  "$(CFG)" == "vcl - Win32 StaticRelease"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "vcl___Win32_StaticRelease"
# PROP BASE Intermediate_Dir "vcl___Win32_StaticRelease"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "StaticRelease"
# PROP Intermediate_Dir "StaticRelease"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)/vcl" /I "$(IUEROOT)\vcl" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /FD /c
# SUBTRACT BASE CPP /YX
# ADD CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)/vcl" /I "$(IUEROOT)\vcl" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /Zl /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo /out:"..\Release\vcl.lib"
# ADD LIB32 /nologo /out:"..\StaticRelease\vcl.lib"

!ELSEIF  "$(CFG)" == "vcl - Win32 DebugSTLPort"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "vcl___Win32_DebugSTLPort"
# PROP BASE Intermediate_Dir "vcl___Win32_DebugSTLPort"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug-STLPort"
# PROP Intermediate_Dir "Debug-STLPort"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /Ob2 /I "$(STLPORT)\stlport" /I "$(IUEROOT)\vcl\config.stlport.win32-VC60" /I "$(IUEROOT)\vcl" /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /Zl /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo /out:"..\Debug\vcl.lib"
# ADD LIB32 /nologo /out:"..\Debug-STLPort\vcl.lib"

!ELSEIF  "$(CFG)" == "vcl - Win32 ReleaseSTLPort"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "vcl___Win32_ReleaseSTLPort"
# PROP BASE Intermediate_Dir "vcl___Win32_ReleaseSTLPort"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release-STLPort"
# PROP Intermediate_Dir "Release-STLPort"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)/vcl" /I "$(IUEROOT)\vcl" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /FD /c
# SUBTRACT BASE CPP /YX
# ADD CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(STLPORT)\stlport" /I "$(IUEROOT)\vcl\config.stlport.win32-VC60" /I "$(IUEROOT)\vcl" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /Zl /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo /out:"..\Release\vcl.lib"
# ADD LIB32 /nologo /out:"..\Release-STLPort\vcl.lib"

!ELSEIF  "$(CFG)" == "vcl - Win32 ReleaseWithDBInfo"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "vcl___Win32_ReleaseWithDBInfo"
# PROP BASE Intermediate_Dir "vcl___Win32_ReleaseWithDBInfo"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "ReleaseWithDBInfo"
# PROP Intermediate_Dir "ReleaseWithDBInfo"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)/vcl" /I "$(IUEROOT)\vcl" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /FD /c
# SUBTRACT BASE CPP /YX
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)/vcl" /I "$(IUEROOT)\vcl" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /Zl /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo /out:"..\Release\vcl.lib"
# ADD LIB32 /nologo /out:"..\Release\vcl.lib"

!ENDIF 

# Begin Target

# Name "vcl - Win32 Release"
# Name "vcl - Win32 Debug"
# Name "vcl - Win32 StaticDebug"
# Name "vcl - Win32 StaticRelease"
# Name "vcl - Win32 DebugSTLPort"
# Name "vcl - Win32 ReleaseSTLPort"
# Name "vcl - Win32 ReleaseWithDBInfo"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=".\Templates\complex-instances.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\fstream-instances.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\stream-instances.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\string-instances.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_algorithm+bool-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_algorithm+double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_algorithm+float-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_algorithm+int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_algorithm+unsigned-.cxx"
# End Source File
# Begin Source File

SOURCE=.\emulation\vcl_alloc.cxx
# End Source File
# Begin Source File

SOURCE=.\vcl_cassert.cxx
# End Source File
# Begin Source File

SOURCE=.\vcl_cmath.cxx
# End Source File
# Begin Source File

SOURCE=.\vcl_cstdlib.cxx
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_deque+int-.cxx"
# End Source File
# Begin Source File

SOURCE=.\emulation\vcl_hashtable.cxx
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_list+float-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_list+float~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_list+int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_list+uint-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_list+ulong-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_list+vcl_pair+void~.int--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_list+vcl_string-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_list+void~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+double.int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+int.double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+int.int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+int.uint-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+int.void~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+long.void~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+string.int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+string.string-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+unsigned_int.double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+unsigned_int.int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+unsigned_int.unsigned_int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+unsigned_int.void~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+void~.int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_map+void~.void~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_ostream_iterator+int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_pair+float.float-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_pair+void~.int-.cxx"
# End Source File
# Begin Source File

SOURCE=.\emulation\vcl_rbtree_instances.cxx
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_set+int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_set+unsigned-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_set+vcl_string-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_set+void~-.cxx"
# End Source File
# Begin Source File

SOURCE=.\emulation\vcl_straits.cxx
# End Source File
# Begin Source File

SOURCE=.\emulation\vcl_string_instances.cxx
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+bool-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+bool~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+char-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+char~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+complex+double--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+complex+double-~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+complex+float--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+complex+float-~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+const_char~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+double_const~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+double~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+float-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+float~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+int-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+int~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+long-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+long~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+schar-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+schar~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+string-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+uchar-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+uchar~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+uint-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+uint~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+ulong-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+ulong~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_map+int.int--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_map+uint.uint--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_pair+double.double--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_pair+double.int--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_pair+float.float--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_pair+int.int--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_pair+int.int-~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_pair+int.string--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_pair+string.string--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_pair+uint.uint--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_vector+double--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_vector+int--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_vector+unsigned--.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+vcl_vector+unsigned-~-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vcl_vector+void~-.cxx"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\vcl_algorithm.h
# End Source File
# Begin Source File

SOURCE=.\vcl_alloc.h
# End Source File
# Begin Source File

SOURCE=.\vcl_cassert.h
# End Source File
# Begin Source File

SOURCE=.\vcl_cctype.h
# End Source File
# Begin Source File

SOURCE=.\vcl_climits.h
# End Source File
# Begin Source File

SOURCE=.\vcl_cmath.h
# End Source File
# Begin Source File

SOURCE=.\vcl_compiler.h
# End Source File
# Begin Source File

SOURCE=.\vcl_complex.h
# End Source File
# Begin Source File

SOURCE=.\vcl_complex_fwd.h
# End Source File
# Begin Source File

SOURCE=.\vcl_config.h
# End Source File
# Begin Source File

SOURCE=.\vcl_cstdarg.h
# End Source File
# Begin Source File

SOURCE=.\vcl_cstddef.h
# End Source File
# Begin Source File

SOURCE=.\vcl_cstdio.h
# End Source File
# Begin Source File

SOURCE=.\win32\vcl_cstdio.h
# End Source File
# Begin Source File

SOURCE=.\vcl_cstdlib.h
# End Source File
# Begin Source File

SOURCE=.\vcl_cstring.h
# End Source File
# Begin Source File

SOURCE=.\vcl_ctime.h
# End Source File
# Begin Source File

SOURCE=.\vcl_deque.h
# End Source File
# Begin Source File

SOURCE=.\vcl_deprecated.h
# End Source File
# Begin Source File

SOURCE=.\vcl_exception.h
# End Source File
# Begin Source File

SOURCE=.\vcl_fstream.h
# End Source File
# Begin Source File

SOURCE=.\vcl_functional.h
# End Source File
# Begin Source File

SOURCE=.\vcl_iomanip.h
# End Source File
# Begin Source File

SOURCE=.\vcl_ios.h
# End Source File
# Begin Source File

SOURCE=.\vcl_iosfwd.h
# End Source File
# Begin Source File

SOURCE=.\vcl_iostream.h
# End Source File
# Begin Source File

SOURCE=.\vcl_istream.h
# End Source File
# Begin Source File

SOURCE=.\vcl_iterator.h
# End Source File
# Begin Source File

SOURCE=.\vcl_limits.h
# End Source File
# Begin Source File

SOURCE=.\vcl_list.h
# End Source File
# Begin Source File

SOURCE=.\vcl_locale.h
# End Source File
# Begin Source File

SOURCE=.\vcl_map.h
# End Source File
# Begin Source File

SOURCE=.\vcl_memory.h
# End Source File
# Begin Source File

SOURCE=.\vcl_multimap.h
# End Source File
# Begin Source File

SOURCE=.\vcl_multiset.h
# End Source File
# Begin Source File

SOURCE=.\vcl_new.h
# End Source File
# Begin Source File

SOURCE=.\vcl_numeric.h
# End Source File
# Begin Source File

SOURCE=.\vcl_pair.h
# End Source File
# Begin Source File

SOURCE=.\vcl_queue.h
# End Source File
# Begin Source File

SOURCE=.\vcl_set.h
# End Source File
# Begin Source File

SOURCE=.\vcl_sstream.h
# End Source File
# Begin Source File

SOURCE=.\vcl_stack.h
# End Source File
# Begin Source File

SOURCE=.\vcl_stdexcept.h
# End Source File
# Begin Source File

SOURCE=.\vcl_stlfwd.h
# End Source File
# Begin Source File

SOURCE=.\vcl_streambuf.h
# End Source File
# Begin Source File

SOURCE=.\vcl_string.h
# End Source File
# Begin Source File

SOURCE=.\vcl_strstream.h
# End Source File
# Begin Source File

SOURCE=.\vcl_typeinfo.h
# End Source File
# Begin Source File

SOURCE=.\vcl_utility.h
# End Source File
# Begin Source File

SOURCE=.\vcl_vector.h
# End Source File
# End Group
# End Target
# End Project
