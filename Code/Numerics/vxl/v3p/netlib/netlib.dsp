# Microsoft Developer Studio Project File - Name="netlib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=netlib - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "netlib.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "netlib.mak" CFG="netlib - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "netlib - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "netlib - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "netlib - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /FD /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "netlib - Win32 Debug"

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
MTL=midl.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /FD /GZ /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "netlib - Win32 Release"
# Name "netlib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\c_abs.c
# End Source File
# Begin Source File

SOURCE=.\c_div.c
# End Source File
# Begin Source File

SOURCE=.\cabs.c
# End Source File
# Begin Source File

SOURCE=.\camsun.c
# End Source File
# Begin Source File

SOURCE=.\caxpy.c
# End Source File
# Begin Source File

SOURCE=.\cdotc.c
# End Source File
# Begin Source File

SOURCE=.\cg.c
# End Source File
# Begin Source File

SOURCE=.\cscal.c
# End Source File
# Begin Source File

SOURCE=.\csrot.c
# End Source File
# Begin Source File

SOURCE=.\csvdc.c
# End Source File
# Begin Source File

SOURCE=.\cswap.c
# End Source File
# Begin Source File

SOURCE=.\d_cnjg.c
# End Source File
# Begin Source File

SOURCE=.\d_imag.c
# End Source File
# Begin Source File

SOURCE=.\d_lg10.c
# End Source File
# Begin Source File

SOURCE=.\d_sign.c
# End Source File
# Begin Source File

SOURCE=.\dasum.c
# End Source File
# Begin Source File

SOURCE=.\daxpy.c
# End Source File
# Begin Source File

SOURCE=.\dcopy.c
# End Source File
# Begin Source File

SOURCE=.\ddot.c
# End Source File
# Begin Source File

SOURCE=.\dgemv.c
# End Source File
# Begin Source File

SOURCE=.\dgeqpf.c
# End Source File
# Begin Source File

SOURCE=.\dgeqr2.c
# End Source File
# Begin Source File

SOURCE=.\dger.c
# End Source File
# Begin Source File

SOURCE=.\dgerq2.c
# End Source File
# Begin Source File

SOURCE=.\dggsvd.c
# End Source File
# Begin Source File

SOURCE=.\dggsvp.c
# End Source File
# Begin Source File

SOURCE=.\dgpfa.c
# End Source File
# Begin Source File

SOURCE=.\dgpfa2f.c
# End Source File
# Begin Source File

SOURCE=.\dgpfa3f.c
# End Source File
# Begin Source File

SOURCE=.\dgpfa5f.c
# End Source File
# Begin Source File

SOURCE=.\dlabad.c
# End Source File
# Begin Source File

SOURCE=.\dlacpy.c
# End Source File
# Begin Source File

SOURCE=.\dladiv.c
# End Source File
# Begin Source File

SOURCE=.\dlags2.c
# End Source File
# Begin Source File

SOURCE=.\dlamch.c
# End Source File
# Begin Source File

SOURCE=.\dlange.c
# End Source File
# Begin Source File

SOURCE=.\dlapll.c
# End Source File
# Begin Source File

SOURCE=.\dlapmt.c
# End Source File
# Begin Source File

SOURCE=.\dlapy2.c
# End Source File
# Begin Source File

SOURCE=.\dlapy3.c
# End Source File
# Begin Source File

SOURCE=.\dlarf.c
# End Source File
# Begin Source File

SOURCE=.\dlarfg.c
# End Source File
# Begin Source File

SOURCE=.\dlartg.c
# End Source File
# Begin Source File

SOURCE=.\dlas2.c
# End Source File
# Begin Source File

SOURCE=.\dlaset.c
# End Source File
# Begin Source File

SOURCE=.\dlassq.c
# End Source File
# Begin Source File

SOURCE=.\dlasv2.c
# End Source File
# Begin Source File

SOURCE=.\dnlaso.c
# End Source File
# Begin Source File

SOURCE=.\dnrm2.c
# End Source File
# Begin Source File

SOURCE=.\dorg2r.c
# End Source File
# Begin Source File

SOURCE=.\dorm2r.c
# End Source File
# Begin Source File

SOURCE=.\dormr2.c
# End Source File
# Begin Source File

SOURCE=.\dpoco.c
# End Source File
# Begin Source File

SOURCE=.\dpodi.c
# End Source File
# Begin Source File

SOURCE=.\dpofa.c
# End Source File
# Begin Source File

SOURCE=.\dposl.c
# End Source File
# Begin Source File

SOURCE=.\dqrdc.c
# End Source File
# Begin Source File

SOURCE=.\dqrsl.c
# End Source File
# Begin Source File

SOURCE=.\drot.c
# End Source File
# Begin Source File

SOURCE=.\drotg.c
# End Source File
# Begin Source File

SOURCE=.\dscal.c
# End Source File
# Begin Source File

SOURCE=.\dsetgpfa.c
# End Source File
# Begin Source File

SOURCE=.\dsvdc.c
# End Source File
# Begin Source File

SOURCE=.\dswap.c
# End Source File
# Begin Source File

SOURCE=.\dtgsja.c
# End Source File
# Begin Source File

SOURCE=.\dtrans.c
# End Source File
# Begin Source File

SOURCE=.\dzasum.c
# End Source File
# Begin Source File

SOURCE=.\eisrsg.c
# End Source File
# Begin Source File

SOURCE=.\exit.c
# End Source File
# Begin Source File

SOURCE=.\F77_aloc.c
# End Source File
# Begin Source File

SOURCE=".\full-dpoco.c"
# End Source File
# Begin Source File

SOURCE=.\gpfa.c
# End Source File
# Begin Source File

SOURCE=.\gpfa2f.c
# End Source File
# Begin Source File

SOURCE=.\gpfa3f.c
# End Source File
# Begin Source File

SOURCE=.\gpfa5f.c
# End Source File
# Begin Source File

SOURCE=.\idamax.c
# End Source File
# Begin Source File

SOURCE=.\ilaenv.c
# End Source File
# Begin Source File

SOURCE=.\isamax.c
# End Source File
# Begin Source File

SOURCE=.\izamax.c
# End Source File
# Begin Source File

SOURCE=.\izmax1.c
# End Source File
# Begin Source File

SOURCE=".\lbfgs-example.c"
# End Source File
# Begin Source File

SOURCE=".\lbfgs-lb1.c"
# End Source File
# Begin Source File

SOURCE=.\lbfgs.c
# End Source File
# Begin Source File

SOURCE=.\lmder.c
# End Source File
# Begin Source File

SOURCE=.\lmder1.c
# End Source File
# Begin Source File

SOURCE=.\lmdif.c
# End Source File
# Begin Source File

SOURCE=.\lsame.c
# End Source File
# Begin Source File

SOURCE=.\machineparams.c
# End Source File
# Begin Source File

SOURCE=.\main.c
# End Source File
# Begin Source File

SOURCE=.\pow_dd.c
# End Source File
# Begin Source File

SOURCE=.\pow_di.c
# End Source File
# Begin Source File

SOURCE=.\pow_ii.c
# End Source File
# Begin Source File

SOURCE=.\pow_ri.c
# End Source File
# Begin Source File

SOURCE=.\pythag.c
# End Source File
# Begin Source File

SOURCE=.\r_cnjg.c
# End Source File
# Begin Source File

SOURCE=.\r_imag.c
# End Source File
# Begin Source File

SOURCE=.\r_sign.c
# End Source File
# Begin Source File

SOURCE=.\rg.c
# End Source File
# Begin Source File

SOURCE=.\rpoly.c
# End Source File
# Begin Source File

SOURCE=.\rs.c
# End Source File
# Begin Source File

SOURCE=.\rsg.c
# End Source File
# Begin Source File

SOURCE=.\s_cat.c
# End Source File
# Begin Source File

SOURCE=.\s_cmp.c
# End Source File
# Begin Source File

SOURCE=.\s_copy.c
# End Source File
# Begin Source File

SOURCE=.\sasum.c
# End Source File
# Begin Source File

SOURCE=.\saxpy.c
# End Source File
# Begin Source File

SOURCE=.\scnrm2.c
# End Source File
# Begin Source File

SOURCE=.\scopy.c
# End Source File
# Begin Source File

SOURCE=.\sdot.c
# End Source File
# Begin Source File

SOURCE=.\setgpfa.c
# End Source File
# Begin Source File

SOURCE=.\sgemv.c
# End Source File
# Begin Source File

SOURCE=.\sgeqpf.c
# End Source File
# Begin Source File

SOURCE=.\sgeqr2.c
# End Source File
# Begin Source File

SOURCE=.\sger.c
# End Source File
# Begin Source File

SOURCE=.\sgerq2.c
# End Source File
# Begin Source File

SOURCE=.\sggsvd.c
# End Source File
# Begin Source File

SOURCE=.\sggsvp.c
# End Source File
# Begin Source File

SOURCE=.\sig_die.c
# End Source File
# Begin Source File

SOURCE=.\slabax.c
# End Source File
# Begin Source File

SOURCE=.\slabcm.c
# End Source File
# Begin Source File

SOURCE=.\slabfc.c
# End Source File
# Begin Source File

SOURCE=.\slacpy.c
# End Source File
# Begin Source File

SOURCE=.\slaeig.c
# End Source File
# Begin Source File

SOURCE=.\slager.c
# End Source File
# Begin Source File

SOURCE=.\slags2.c
# End Source File
# Begin Source File

SOURCE=.\slamch.c
# End Source File
# Begin Source File

SOURCE=.\slange.c
# End Source File
# Begin Source File

SOURCE=.\slapll.c
# End Source File
# Begin Source File

SOURCE=.\slapmt.c
# End Source File
# Begin Source File

SOURCE=.\slapy2.c
# End Source File
# Begin Source File

SOURCE=.\slaran.c
# End Source File
# Begin Source File

SOURCE=.\slarf.c
# End Source File
# Begin Source File

SOURCE=.\slarfg.c
# End Source File
# Begin Source File

SOURCE=.\slartg.c
# End Source File
# Begin Source File

SOURCE=.\slas2.c
# End Source File
# Begin Source File

SOURCE=.\slaset.c
# End Source File
# Begin Source File

SOURCE=.\slassq.c
# End Source File
# Begin Source File

SOURCE=.\slasv2.c
# End Source File
# Begin Source File

SOURCE=.\smvpc.c
# End Source File
# Begin Source File

SOURCE=.\snlaso.c
# End Source File
# Begin Source File

SOURCE=.\snppla.c
# End Source File
# Begin Source File

SOURCE=.\snrm2.c
# End Source File
# Begin Source File

SOURCE=.\sorg2r.c
# End Source File
# Begin Source File

SOURCE=.\sorm2r.c
# End Source File
# Begin Source File

SOURCE=.\sormr2.c
# End Source File
# Begin Source File

SOURCE=.\sortqr.c
# End Source File
# Begin Source File

SOURCE=.\srot.c
# End Source File
# Begin Source File

SOURCE=.\srotg.c
# End Source File
# Begin Source File

SOURCE=.\sscal.c
# End Source File
# Begin Source File

SOURCE=.\ssvdc.c
# End Source File
# Begin Source File

SOURCE=.\sswap.c
# End Source File
# Begin Source File

SOURCE=.\stgsja.c
# End Source File
# Begin Source File

SOURCE=.\svsort.c
# End Source File
# Begin Source File

SOURCE=.\tql1.c
# End Source File
# Begin Source File

SOURCE=.\tql2.c
# End Source File
# Begin Source File

SOURCE=.\trans.c
# End Source File
# Begin Source File

SOURCE=.\tred1.c
# End Source File
# Begin Source File

SOURCE=.\tred2.c
# End Source File
# Begin Source File

SOURCE=.\xerbla.c
# End Source File
# Begin Source File

SOURCE=.\z_abs.c
# End Source File
# Begin Source File

SOURCE=.\z_div.c
# End Source File
# Begin Source File

SOURCE=.\z_sqrt.c
# End Source File
# Begin Source File

SOURCE=.\zcopy.c
# End Source File
# Begin Source File

SOURCE=.\zdotu.c
# End Source File
# Begin Source File

SOURCE=.\zdscal.c
# End Source File
# Begin Source File

SOURCE=.\zgebak.c
# End Source File
# Begin Source File

SOURCE=.\zgebal.c
# End Source File
# Begin Source File

SOURCE=.\zgeev.c
# End Source File
# Begin Source File

SOURCE=.\zgehd2.c
# End Source File
# Begin Source File

SOURCE=.\zgehrd.c
# End Source File
# Begin Source File

SOURCE=.\zgemm.c
# End Source File
# Begin Source File

SOURCE=.\zgemv.c
# End Source File
# Begin Source File

SOURCE=.\zgerc.c
# End Source File
# Begin Source File

SOURCE=.\zhseqr.c
# End Source File
# Begin Source File

SOURCE=.\zlacgv.c
# End Source File
# Begin Source File

SOURCE=.\zlacpy.c
# End Source File
# Begin Source File

SOURCE=.\zladiv.c
# End Source File
# Begin Source File

SOURCE=.\zlahqr.c
# End Source File
# Begin Source File

SOURCE=.\zlahrd.c
# End Source File
# Begin Source File

SOURCE=.\zlange.c
# End Source File
# Begin Source File

SOURCE=.\zlanhs.c
# End Source File
# Begin Source File

SOURCE=.\zlarf.c
# End Source File
# Begin Source File

SOURCE=.\zlarfb.c
# End Source File
# Begin Source File

SOURCE=.\zlarfg.c
# End Source File
# Begin Source File

SOURCE=.\zlarft.c
# End Source File
# Begin Source File

SOURCE=.\zlarfx.c
# End Source File
# Begin Source File

SOURCE=.\zlascl.c
# End Source File
# Begin Source File

SOURCE=.\zlaset.c
# End Source File
# Begin Source File

SOURCE=.\zlassq.c
# End Source File
# Begin Source File

SOURCE=.\zlatrs.c
# End Source File
# Begin Source File

SOURCE=.\zsvdc.c
# End Source File
# Begin Source File

SOURCE=.\ztrevc.c
# End Source File
# Begin Source File

SOURCE=.\ztrmm.c
# End Source File
# Begin Source File

SOURCE=.\ztrmv.c
# End Source File
# Begin Source File

SOURCE=.\ztrsv.c
# End Source File
# Begin Source File

SOURCE=.\zung2r.c
# End Source File
# Begin Source File

SOURCE=.\zunghr.c
# End Source File
# Begin Source File

SOURCE=.\zungqr.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\f2c.h
# End Source File
# Begin Source File

SOURCE=.\netlib.h
# End Source File
# End Group
# End Target
# End Project
