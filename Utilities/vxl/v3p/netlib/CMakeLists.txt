# ./v3p/netlib/CMakeLists.txt

PROJECT( netlib C )

SET( netlib_sources
  # The "Triangle" program of Jonathan Richard Shewchuk
  triangle.c triangle.h
  )

ADD_LIBRARY( itknetlib ${netlib_sources} )

IF(UNIX)
  TARGET_LINK_LIBRARIES( itknetlib m )
ENDIF(UNIX)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(itknetlib PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT VXL_INSTALL_NO_LIBRARIES)
  INSTALL_TARGETS(
    ${VXL_INSTALL_LIB_DIR}
    RUNTIME_DIRECTORY ${VXL_INSTALL_BIN_DIR}
    itknetlib)
ENDIF(NOT VXL_INSTALL_NO_LIBRARIES)

# Allow sources in subdirectories to see the include files.
INCLUDE_DIRECTORIES(${netlib_SOURCE_DIR})

# Inform header files they are being included from a source inside the
# v3p_netlib library.
ADD_DEFINITIONS(-DV3P_NETLIB_SRC)

# List sources for each library component.
SET(V3P_NETLIB_base_SOURCES
  v3p_f2c.h
  v3p_f2c_mangle.h
  v3p_f2c_original.h
  v3p_f2c_unmangle.h
  v3p_netlib.h
  v3p_netlib_mangle.h
  v3p_netlib_prototypes.h
  v3p_netlib_unmangle.h
  v3p_netlib_init.c
  )
SET(V3P_NETLIB_libf2c_SOURCES
  libf2c/f77_aloc.c
  libf2c/exit_.c
  libf2c/c_abs.c
  libf2c/c_div.c
  libf2c/c_sqrt.c
  libf2c/cabs.c
  libf2c/d_cnjg.c
  libf2c/d_imag.c
  libf2c/d_lg10.c
  libf2c/d_sign.c
  libf2c/i_dnnt.c
  libf2c/pow_dd.c
  libf2c/pow_di.c
  libf2c/pow_ii.c
  libf2c/pow_ri.c
  libf2c/r_cnjg.c
  libf2c/r_imag.c
  libf2c/r_sign.c
  libf2c/s_cat.c
  libf2c/s_cmp.c
  libf2c/s_copy.c
  libf2c/sig_die.c
  libf2c/z_abs.c
  libf2c/z_div.c
  libf2c/z_sqrt.c
  )
SET(V3P_NETLIB_blas_SOURCES
  blas/caxpy.c blas/caxpy.h
  blas/ccopy.c blas/ccopy.h
  blas/cdotc.c blas/cdotc.h
  blas/cscal.c blas/cscal.h
  blas/csrot.c blas/csrot.h
  blas/cswap.c blas/cswap.h
  blas/dasum.c blas/dasum.h
  blas/daxpy.c blas/daxpy.h
  blas/dcabs1.c blas/dcabs1.h
  blas/dcopy.c blas/dcopy.h
  blas/ddot.c blas/ddot.h
  blas/dgemm.c blas/dgemm.h
  blas/dgemv.c blas/dgemv.h
  blas/dger.c blas/dger.h
  blas/dlamch.c blas/dlamch.h
  blas/dnrm2.c blas/dnrm2.h
  blas/drot.c blas/drot.h
  blas/drotg.c blas/drotg.h
  blas/dscal.c blas/dscal.h
  blas/dswap.c blas/dswap.h
  blas/dtrmm.c blas/dtrmm.h
  blas/dtrmv.c blas/dtrmv.h
  blas/dtrsv.c blas/dtrsv.h
  blas/dzasum.c blas/dzasum.h
  blas/dznrm2.c blas/dznrm2.h
  blas/idamax.c blas/idamax.h
  blas/isamax.c blas/isamax.h
  blas/izamax.c blas/izamax.h
  blas/saxpy.c blas/saxpy.h
  blas/scnrm2.c blas/scnrm2.h
  blas/scopy.c blas/scopy.h
  blas/sdot.c blas/sdot.h
  blas/sgemv.c blas/sgemv.h
  blas/sger.c blas/sger.h
  blas/slamch.c blas/slamch.h
  blas/snrm2.c blas/snrm2.h
  blas/srot.c blas/srot.h
  blas/srotg.c blas/srotg.h
  blas/sscal.c blas/sscal.h
  blas/sswap.c blas/sswap.h
  blas/xerbla.c blas/xerbla.h
  blas/zaxpy.c blas/zaxpy.h
  blas/zcopy.c blas/zcopy.h
  blas/zdotc.c blas/zdotc.h
  blas/zdotu.c blas/zdotu.h
  blas/zdrot.c blas/zdrot.h
  blas/zdscal.c blas/zdscal.h
  blas/zgemm.c blas/zgemm.h
  blas/zgemv.c blas/zgemv.h
  blas/zgerc.c blas/zgerc.h
  blas/zscal.c blas/zscal.h
  blas/zswap.c blas/zswap.h
  blas/ztrmm.c blas/ztrmm.h
  blas/ztrmv.c blas/ztrmv.h
  blas/ztrsv.c blas/ztrsv.h
  )
SET(V3P_NETLIB_linpack_SOURCES
  linpack/sqrdc.c linpack/sqrdc.h
  linpack/dqrdc.c linpack/dqrdc.h
  linpack/cqrdc.c linpack/cqrdc.h
  linpack/zqrdc.c linpack/zqrdc.h
  linpack/sqrsl.c linpack/sqrsl.h
  linpack/dqrsl.c linpack/dqrsl.h
  linpack/cqrsl.c linpack/cqrsl.h
  linpack/zqrsl.c linpack/zqrsl.h

  linpack/csvdc.c linpack/csvdc.h
  linpack/dsvdc.c linpack/dsvdc.h
  linpack/ssvdc.c linpack/ssvdc.h
  linpack/zsvdc.c linpack/zsvdc.h

  linpack/dpoco.c linpack/dpoco.h
  linpack/dpodi.c linpack/dpodi.h
  linpack/dpofa.c linpack/dpofa.h
  linpack/dposl.c linpack/dposl.h
  )
SET(V3P_NETLIB_temperton_SOURCES
  temperton/dgpfa.c temperton/dgpfa.h
  temperton/dgpfa2f.c temperton/dgpfa2f.h
  temperton/dgpfa3f.c temperton/dgpfa3f.h
  temperton/dgpfa5f.c temperton/dgpfa5f.h
  temperton/gpfa.c temperton/gpfa.h
  temperton/gpfa2f.c temperton/gpfa2f.h
  temperton/gpfa3f.c temperton/gpfa3f.h
  temperton/gpfa5f.c temperton/gpfa5f.h
  temperton/setdgpfa.c temperton/setdgpfa.h
  temperton/setgpfa.c temperton/setgpfa.h
  )
SET(V3P_NETLIB_eispack_SOURCES
  eispack/balanc.c eispack/balanc.h
  eispack/balbak.c eispack/balbak.h
  eispack/cdiv.c eispack/cdiv.h
  eispack/elmhes.c eispack/elmhes.h
  eispack/eltran.c eispack/eltran.h
  eispack/epslon.c eispack/epslon.h
  eispack/hqr.c eispack/hqr.h
  eispack/hqr2.c eispack/hqr2.h
  eispack/otqlrat.c eispack/otqlrat.h
  eispack/pythag.c eispack/pythag.h
  eispack/rebak.c eispack/rebak.h
  eispack/reduc.c eispack/reduc.h
  eispack/rg.c eispack/rg.h
  eispack/rs.c eispack/rs.h
  eispack/rsg.c eispack/rsg.h
  eispack/tql1.c eispack/tql1.h
  eispack/tql2.c eispack/tql2.h
  eispack/tred1.c eispack/tred1.h
  eispack/tred2.c eispack/tred2.h
  )
SET(V3P_NETLIB_laso_SOURCES
  laso/dlabax.c laso/dlabax.h
  laso/dlabcm.c laso/dlabcm.h
  laso/dlabfc.c laso/dlabfc.h
  laso/dlaeig.c laso/dlaeig.h
  laso/dlager.c laso/dlager.h
  laso/dlaran.c laso/dlaran.h
  laso/dmvpc.c laso/dmvpc.h
  laso/dnlaso.c laso/dnlaso.h
  laso/dnppla.c laso/dnppla.h
  laso/dortqr.c laso/dortqr.h
  laso/dvsort.c laso/dvsort.h
  laso/urand.c laso/urand.h
  )
SET(V3P_NETLIB_lapack_SOURCES
  lapack/complex16/zgebak.c lapack/complex16/zgebak.h
  lapack/complex16/zgebal.c lapack/complex16/zgebal.h
  lapack/complex16/zgeev.c lapack/complex16/zgeev.h
  lapack/complex16/zgehd2.c lapack/complex16/zgehd2.h
  lapack/complex16/zgehrd.c lapack/complex16/zgehrd.h
  lapack/complex16/zhseqr.c lapack/complex16/zhseqr.h
  lapack/complex16/zlacgv.c lapack/complex16/zlacgv.h
  lapack/complex16/zlacpy.c lapack/complex16/zlacpy.h
  lapack/complex16/zladiv.c lapack/complex16/zladiv.h
  lapack/complex16/zlahqr.c lapack/complex16/zlahqr.h
  lapack/complex16/zlahrd.c lapack/complex16/zlahrd.h
  lapack/complex16/zlange.c lapack/complex16/zlange.h
  lapack/complex16/zlanhs.c lapack/complex16/zlanhs.h
  lapack/complex16/zlarf.c lapack/complex16/zlarf.h
  lapack/complex16/zlarfb.c lapack/complex16/zlarfb.h
  lapack/complex16/zlarfg.c lapack/complex16/zlarfg.h
  lapack/complex16/zlarft.c lapack/complex16/zlarft.h
  lapack/complex16/zlarfx.c lapack/complex16/zlarfx.h
  lapack/complex16/zlascl.c lapack/complex16/zlascl.h
  lapack/complex16/zlaset.c lapack/complex16/zlaset.h
  lapack/complex16/zlassq.c lapack/complex16/zlassq.h
  lapack/complex16/zlatrs.c lapack/complex16/zlatrs.h
  lapack/complex16/ztrevc.c lapack/complex16/ztrevc.h
  lapack/complex16/zung2r.c lapack/complex16/zung2r.h
  lapack/complex16/zunghr.c lapack/complex16/zunghr.h
  lapack/complex16/zungqr.c lapack/complex16/zungqr.h
  lapack/double/dgecon.c lapack/double/dgecon.h
  lapack/double/dgeqr2.c lapack/double/dgeqr2.h
  lapack/double/dgeqrf.c lapack/double/dgeqrf.h
  lapack/double/dgerq2.c lapack/double/dgerq2.h
  lapack/double/dgesc2.c lapack/double/dgesc2.h
  lapack/double/dgetc2.c lapack/double/dgetc2.h
  lapack/double/dggbak.c lapack/double/dggbak.h
  lapack/double/dggbal.c lapack/double/dggbal.h
  lapack/double/dgges.c lapack/double/dgges.h
  lapack/double/dgghrd.c lapack/double/dgghrd.h
  lapack/double/dhgeqz.c lapack/double/dhgeqz.h
  lapack/double/dlabad.c lapack/double/dlabad.h
  lapack/double/dlacon.c lapack/double/dlacon.h
  lapack/double/dlacpy.c lapack/double/dlacpy.h
  lapack/double/dladiv.c lapack/double/dladiv.h
  lapack/double/dlag2.c lapack/double/dlag2.h
  lapack/double/dlagv2.c lapack/double/dlagv2.h
  lapack/double/dlange.c lapack/double/dlange.h
  lapack/double/dlanhs.c lapack/double/dlanhs.h
  lapack/double/dlapy2.c lapack/double/dlapy2.h
  lapack/double/dlapy3.c lapack/double/dlapy3.h
  lapack/double/dlarf.c lapack/double/dlarf.h
  lapack/double/dlarfb.c lapack/double/dlarfb.h
  lapack/double/dlarfg.c lapack/double/dlarfg.h
  lapack/double/dlarft.c lapack/double/dlarft.h
  lapack/double/dlartg.c lapack/double/dlartg.h
  lapack/double/dlascl.c lapack/double/dlascl.h
  lapack/double/dlaset.c lapack/double/dlaset.h
  lapack/double/dlassq.c lapack/double/dlassq.h
  lapack/double/dlasv2.c lapack/double/dlasv2.h
  lapack/double/dlaswp.c lapack/double/dlaswp.h
  lapack/double/dlatdf.c lapack/double/dlatdf.h
  lapack/double/dlatrs.c lapack/double/dlatrs.h
  lapack/double/dorg2r.c lapack/double/dorg2r.h
  lapack/double/dorgqr.c lapack/double/dorgqr.h
  lapack/double/dorgr2.c lapack/double/dorgr2.h
  lapack/double/dorm2r.c lapack/double/dorm2r.h
  lapack/double/dormqr.c lapack/double/dormqr.h
  lapack/double/dormr2.c lapack/double/dormr2.h
  lapack/double/drscl.c lapack/double/drscl.h
  lapack/double/dtgex2.c lapack/double/dtgex2.h
  lapack/double/dtgexc.c lapack/double/dtgexc.h
  lapack/double/dtgsen.c lapack/double/dtgsen.h
  lapack/double/dtgsy2.c lapack/double/dtgsy2.h
  lapack/double/dtgsyl.c lapack/double/dtgsyl.h
  lapack/single/sgeqpf.c lapack/single/sgeqpf.h
  lapack/single/sgeqr2.c lapack/single/sgeqr2.h
  lapack/single/sgerq2.c lapack/single/sgerq2.h
  lapack/single/sggsvd.c lapack/single/sggsvd.h
  lapack/single/sggsvp.c lapack/single/sggsvp.h
  lapack/single/slacpy.c lapack/single/slacpy.h
  lapack/single/slags2.c lapack/single/slags2.h
  lapack/single/slange.c lapack/single/slange.h
  lapack/single/slapll.c lapack/single/slapll.h
  lapack/single/slapmt.c lapack/single/slapmt.h
  lapack/single/slapy2.c lapack/single/slapy2.h
  lapack/single/slarf.c lapack/single/slarf.h
  lapack/single/slarfg.c lapack/single/slarfg.h
  lapack/single/slartg.c lapack/single/slartg.h
  lapack/single/slas2.c lapack/single/slas2.h
  lapack/single/slaset.c lapack/single/slaset.h
  lapack/single/slassq.c lapack/single/slassq.h
  lapack/single/slasv2.c lapack/single/slasv2.h
  lapack/single/sorg2r.c lapack/single/sorg2r.h
  lapack/single/sorm2r.c lapack/single/sorm2r.h
  lapack/single/sormr2.c lapack/single/sormr2.h
  lapack/single/stgsja.c lapack/single/stgsja.h
  lapack/util/ieeeck.c lapack/util/ieeeck.h
  lapack/util/ilaenv.c lapack/util/ilaenv.h
  lapack/util/lsame.c lapack/util/lsame.h
  )
SET(V3P_NETLIB_napack_SOURCES
  napack/cg.c napack/cg.h
  )
SET(V3P_NETLIB_minpack_SOURCES
  minpack/dpmpar.c minpack/dpmpar.h
  minpack/enorm.c minpack/enorm.h
  minpack/fdjac2.c minpack/fdjac2.h
  minpack/lmder.c minpack/lmder.h
  minpack/lmder1.c minpack/lmder1.h
  minpack/lmdif.c minpack/lmdif.h
  minpack/lmpar.c minpack/lmpar.h
  minpack/qrfac.c minpack/qrfac.h
  minpack/qrsolv.c minpack/qrsolv.h
  )
SET(V3P_NETLIB_opt_SOURCES
  opt/lbfgs.c opt/lbfgs.h
  )
SET(V3P_NETLIB_linalg_SOURCES
  linalg/lsqr.c linalg/lsqr.h
  )
SET(V3P_NETLIB_toms_SOURCES
  toms/rpoly.c toms/rpoly.h
  )
SET(V3P_NETLIB_datapac_SOURCES
  datapac/camsun.c datapac/camsun.h
  )
SET(V3P_NETLIB_mathews_SOURCES
  mathews/simpson.c mathews/simpson.h
  mathews/trapezod.c mathews/trapezod.h
  mathews/adaquad.c mathews/adaquad.h
  )

# Collect full source list.
SET(v3p_netlib_sources
  ${V3P_NETLIB_base_SOURCES}
  ${V3P_NETLIB_libf2c_SOURCES}
  ${V3P_NETLIB_blas_SOURCES}
  ${V3P_NETLIB_linpack_SOURCES}
  ${V3P_NETLIB_temperton_SOURCES}
  ${V3P_NETLIB_eispack_SOURCES}
  ${V3P_NETLIB_laso_SOURCES}
  ${V3P_NETLIB_lapack_SOURCES}
  ${V3P_NETLIB_napack_SOURCES}
  ${V3P_NETLIB_minpack_SOURCES}
  ${V3P_NETLIB_opt_SOURCES}
  ${V3P_NETLIB_linalg_SOURCES}
  ${V3P_NETLIB_toms_SOURCES}
  ${V3P_NETLIB_datapac_SOURCES}
  ${V3P_NETLIB_mathews_SOURCES}
  )

# Adjust the compiler flags to avoid problems with f2c-generated code.
IF(CMAKE_COMPILER_IS_GNUCC)
  # Hide warnings in f2c-generated code.
  SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w")

  # CMake versions before 2.4 did not put the source-file-specific flags
  # after the directory-level flags.  Remove optimization flags from the
  # CMAKE_C_FLAGS* variables so they can be overridden.
  IF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" LESS 2.4)
    FOREACH(c "" "_DEBUG" "_RELEASE" "_MINSIZEREL" "_RELWITHDEBINFO")
      STRING(REGEX REPLACE
        "-O[^ ]*( |$)" "" CMAKE_C_FLAGS${c} "${CMAKE_C_FLAGS${c}}")
    ENDFOREACH(c)
  ENDIF("${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}" LESS 2.4)

  # Disable buggy optimization on some source files.
  #SET_SOURCE_FILES_PROPERTIES(
  #  temperton/gpfa5f.c
  #  temperton/dgpfa5f.c
  #  laso/dnlaso.c
  #  PROPERTIES COMPILE_FLAGS -O0
  #  )

  # Adjust optimization of floating point computation for some sources.
  # See comments in the sources for details.
  SET_SOURCE_FILES_PROPERTIES(
    linpack/ssvdc.c
    linpack/dsvdc.c
    PROPERTIES COMPILE_FLAGS -ffloat-store
    )

  # Disable global optimizations for some source files.
  # See comments in the sources for details.
  SET_SOURCE_FILES_PROPERTIES(
    blas/slamch.c
    blas/dlamch.c
    lapack/complex16/zlarfx.c
    PROPERTIES COMPILE_FLAGS -fno-gcse
    )
ENDIF(CMAKE_COMPILER_IS_GNUCC)

# Create a netlib library with mangled symbols.
ADD_LIBRARY(itkv3p_netlib ${v3p_netlib_sources})
IF(UNIX)
  TARGET_LINK_LIBRARIES( itkv3p_netlib m )
ENDIF(UNIX)
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(itkv3p_netlib PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT VXL_INSTALL_NO_LIBRARIES)
  INSTALL_TARGETS(
    ${VXL_INSTALL_LIB_DIR}
    RUNTIME_DIRECTORY ${VXL_INSTALL_BIN_DIR}
    itkv3p_netlib)
ENDIF(NOT VXL_INSTALL_NO_LIBRARIES)
