# Microsoft Developer Studio Project File - Name="vnl_algo" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=vnl_algo - Win32 StaticDebug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "vnl_algo.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "vnl_algo.mak" CFG="vnl_algo - Win32 StaticDebug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "vnl_algo - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "vnl_algo - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "vnl_algo - Win32 StaticDebug" (based on "Win32 (x86) Static Library")
!MESSAGE "vnl_algo - Win32 StaticRelease" (based on "Win32 (x86) Static Library")
!MESSAGE "vnl_algo - Win32 ReleaseWithDBInfo" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
RSC=rc.exe

!IF  "$(CFG)" == "vnl_algo - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)" /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /I "$(IUEROOT)/vxl" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /Zl
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\Release\vnl_algo.lib"

!ELSEIF  "$(CFG)" == "vnl_algo - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /I "$(IUEROOT)/vxl" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /FD /GZ /c /Zl
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\Debug\vnl_algo.lib"

!ELSEIF  "$(CFG)" == "vnl_algo - Win32 StaticDebug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "vnl_algo___Win32_StaticDebug"
# PROP BASE Intermediate_Dir "vnl_algo___Win32_StaticDebug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 2
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "StaticDebug"
# PROP Intermediate_Dir "StaticDebug"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /I "$(IUEROOT)/vxl" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /Ob2 /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /I "$(IUEROOT)/vxl" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /D "_AFXDLL" /FD /GZ /c /Zl
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo /out:"..\..\Debug\vnl_algo.lib"
# ADD LIB32 /nologo /out:"..\..\StaticDebug\vnl_algo.lib"

!ELSEIF  "$(CFG)" == "vnl_algo - Win32 StaticRelease"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "vnl_algo___Win32_StaticRelease"
# PROP BASE Intermediate_Dir "vnl_algo___Win32_StaticRelease"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "StaticRelease"
# PROP Intermediate_Dir "StaticRelease"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)" /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /I "$(IUEROOT)/vxl" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)" /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /I "$(IUEROOT)/vxl" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /Zl
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo /out:"..\..\Release\vnl_algo.lib"
# ADD LIB32 /nologo /out:"..\..\StaticRelease\vnl_algo.lib"

!ELSEIF  "$(CFG)" == "vnl_algo - Win32 ReleaseWithDBInfo"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "vnl_algo___Win32_ReleaseWithDBInfo"
# PROP BASE Intermediate_Dir "vnl_algo___Win32_ReleaseWithDBInfo"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "ReleaseWithDBInfo"
# PROP Intermediate_Dir "ReleaseWithDBInfo"
# PROP Target_Dir ""
MTL=midl.exe
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(IUEROOT)" /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /I "$(IUEROOT)/vxl" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /Ob2 /I "$(IUEROOT)" /I "$(IUEROOT)\vcl\config.win32-VC60" /I "$(IUEROOT)\vcl" /I "$(IUEROOT)/vxl" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /Zl
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=xilink6.exe -lib
# ADD BASE LIB32 /nologo /out:"..\..\Release\vnl_algo.lib"
# ADD LIB32 /nologo /out:"..\..\Release\vnl_algo.lib"

!ENDIF 

# Begin Target

# Name "vnl_algo - Win32 Release"
# Name "vnl_algo - Win32 Debug"
# Name "vnl_algo - Win32 StaticDebug"
# Name "vnl_algo - Win32 StaticRelease"
# Name "vnl_algo - Win32 ReleaseWithDBInfo"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=".\Templates\vcl_vector+vnl_amoeba_SimplexCorner-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_adjugate+double-.cxx"
# End Source File
# Begin Source File

SOURCE=.\vnl_amoeba.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_chi_squared.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_cholesky.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_complex_eigensystem.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_conjugate_gradient.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_cpoly_roots.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_determinant.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_discrete_diff.cxx
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_fft1d+double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_fft1d+float-.cxx"
# End Source File
# Begin Source File

SOURCE=.\Templates\vnl_fft2d.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_fftxd_prime_factors.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_gaussian_kernel_1d.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_generalized_eigensystem.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_lbfgs.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_levenberg_marquardt.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_lsqr.cxx
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_matrix_inverse+double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_orthogonal_complement+double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_orthogonal_complement+double_complex-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_qr+double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_qr+double_complex-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_qr+float-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_qr+float_complex-.cxx"
# End Source File
# Begin Source File

SOURCE=.\vnl_real_eigensystem.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_rnpoly_solve.cxx
# End Source File
# Begin Source File

SOURCE=.\vnl_rpoly_roots.cxx
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_scatter_3x3+double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_scatter_3x3+float-.cxx"
# End Source File
# Begin Source File

SOURCE=.\vnl_sparse_symmetric_eigensystem.cxx
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_svd+double-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_svd+double_complex-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_svd+float-.cxx"
# End Source File
# Begin Source File

SOURCE=".\Templates\vnl_svd+float_complex-.cxx"
# End Source File
# Begin Source File

SOURCE=.\vnl_symmetric_eigensystem.cxx
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\dll.h
# End Source File
# Begin Source File

SOURCE=.\vnl_affine_approx.h
# End Source File
# Begin Source File

SOURCE=.\vnl_algo_fwd.h
# End Source File
# Begin Source File

SOURCE=.\vnl_amoeba.h
# End Source File
# Begin Source File

SOURCE=.\vnl_chi_squared.h
# End Source File
# Begin Source File

SOURCE=.\vnl_cholesky.h
# End Source File
# Begin Source File

SOURCE=.\vnl_complex_eigensystem.h
# End Source File
# Begin Source File

SOURCE=.\vnl_conjugate_gradient.h
# End Source File
# Begin Source File

SOURCE=.\vnl_cpoly_roots.h
# End Source File
# Begin Source File

SOURCE=.\vnl_determinant.h
# End Source File
# Begin Source File

SOURCE=.\vnl_discrete_diff.h
# End Source File
# Begin Source File

SOURCE=.\vnl_fft1d.h
# End Source File
# Begin Source File

SOURCE=.\vnl_fft2d.h
# End Source File
# Begin Source File

SOURCE=.\vnl_fftxd_prime_factors.h
# End Source File
# Begin Source File

SOURCE=.\vnl_gaussian_kernel_1d.h
# End Source File
# Begin Source File

SOURCE=.\vnl_generalized_eigensystem.h
# End Source File
# Begin Source File

SOURCE=.\vnl_lbfgs.h
# End Source File
# Begin Source File

SOURCE=.\vnl_levenberg_marquardt.h
# End Source File
# Begin Source File

SOURCE=.\vnl_matrix_inverse.h
# End Source File
# Begin Source File

SOURCE=.\vnl_netlib.h
# End Source File
# Begin Source File

SOURCE=.\vnl_qr.h
# End Source File
# Begin Source File

SOURCE=.\vnl_real_eigensystem.h
# End Source File
# Begin Source File

SOURCE=.\vnl_rnpoly_solve.h
# End Source File
# Begin Source File

SOURCE=.\vnl_rpoly_roots.h
# End Source File
# Begin Source File

SOURCE=.\vnl_sparse_symmetric_eigensystem.h
# End Source File
# Begin Source File

SOURCE=.\vnl_svd.h
# End Source File
# Begin Source File

SOURCE=.\vnl_symmetric_eigensystem.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\vnl_matrix_inverse.txx
# End Source File
# End Target
# End Project
