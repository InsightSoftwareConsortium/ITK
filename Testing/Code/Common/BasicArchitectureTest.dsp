# Microsoft Developer Studio Project File - Name="basicArchitectureTest" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=basicArchitectureTest - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "basicArchitectureTest.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "basicArchitectureTest.mak" CFG="basicArchitectureTest - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "basicArchitectureTest - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "basicArchitectureTest - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "basicArchitectureTest - Win32 Release"

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
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c /I ../../../Code/Common
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "basicArchitectureTest - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MD /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c  /I ../../../Code/Common
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "basicArchitectureTest - Win32 Release"
# Name "basicArchitectureTest - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=itkBasicArchitectureTest.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkDataObject.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkImageBase.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkIndent.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkObject.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkOutputWindow.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkProcessObject.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkTimeStamp.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkVersion.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkWin32OutputWindow.cxx
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkWriter.cxx
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\..\..\Code\Common\itkDataObject.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkImageBase.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkImageIterator.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkImageSource.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkImageToImageFilter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkImageWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkIndent.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkIndex.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkMeshBase.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkObject.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkOutputWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkProcessObject.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkRandomImageSource.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkScalar.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkSetGet.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkShrinkImage.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkSmartPointer.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkTimeStamp.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkVector.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkVersion.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkVTKImageReader.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkVTKImageWriter.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkWin32Header.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkWin32OutputWindow.h
# End Source File
# Begin Source File

SOURCE=..\..\..\Code\Common\itkWriter.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
