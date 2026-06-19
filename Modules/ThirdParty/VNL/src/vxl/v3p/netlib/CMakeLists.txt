# v3p/netlib/CMakeLists.txt

project( netlib C )



# Inform header files they are being included from a source inside the
# v3p_netlib library. Directory-scoped so the netlib sources and their
# tests see the internal f2c view; consumers get the public C++ view.
add_compile_definitions(V3P_NETLIB_SRC)

# List sources for each library component.
set(V3P_NETLIB_base_SOURCES
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
set(V3P_NETLIB_libf2c_SOURCES
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
     libf2c/pow_zi.c
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
set(V3P_NETLIB_blas_SOURCES
     blas/lsame.c blas/lsame.h
     blas/caxpy.c blas/caxpy.h
     blas/cdotc.c blas/cdotc.h
     blas/cscal.c blas/cscal.h
     blas/csrot.c blas/csrot.h
     blas/cswap.c blas/cswap.h
     blas/daxpy.c blas/daxpy.h
     blas/dcabs1.c blas/dcabs1.h
     blas/dcopy.c blas/dcopy.h
     blas/ddot.c blas/ddot.h
     blas/dlamch.c blas/dlamch.h
     blas/dnrm2.c blas/dnrm2.h
     blas/drot.c blas/drot.h
     blas/drotg.c blas/drotg.h
     blas/dscal.c blas/dscal.h
     blas/dswap.c blas/dswap.h
     blas/dznrm2.c blas/dznrm2.h
     blas/saxpy.c blas/saxpy.h
     blas/scnrm2.c blas/scnrm2.h
     blas/sdot.c blas/sdot.h
     blas/slamch.c blas/slamch.h
     blas/snrm2.c blas/snrm2.h
     blas/srot.c blas/srot.h
     blas/srotg.c blas/srotg.h
     blas/sscal.c blas/sscal.h
     blas/sswap.c blas/sswap.h
     blas/xerbla.c blas/xerbla.h
     blas/zaxpy.c blas/zaxpy.h
     blas/zdotc.c blas/zdotc.h
     blas/zdrot.c blas/zdrot.h
     blas/zscal.c blas/zscal.h
     blas/zswap.c blas/zswap.h
   )
set(V3P_NETLIB_linpack_SOURCES
     linpack/csvdc.c linpack/csvdc.h
     linpack/dsvdc.c linpack/dsvdc.h
     linpack/ssvdc.c linpack/ssvdc.h
     linpack/zsvdc.c linpack/zsvdc.h
   )
set(V3P_NETLIB_eispack_SOURCES
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
# eispack (rs/rg/rsg) backs only the deprecated vnl_real_eigensystem,
# vnl_symmetric_eigensystem, vnl_generalized_eigensystem and vnl_scatter_3x3.
# Drop the dead engine once those APIs are removed.
if(ITK_FUTURE_LEGACY_REMOVE)
  set(V3P_NETLIB_eispack_SOURCES "")
endif()
set(V3P_NETLIB_napack_SOURCES
     napack/cg.c napack/cg.h
   )
# The netlib MINPACK lmdif/lmder port has been removed: ITK's
# vnl_levenberg_marquardt is now backed by Eigen's MINPACK-port
# Levenberg-Marquardt (unsupported/Eigen/NonLinearOptimization), which was its
# only consumer.
set(V3P_NETLIB_opt_SOURCES
     opt/lbfgs.c opt/lbfgs.h
     opt/lbfgsb.c opt/lbfgsb.h
   )
set(V3P_NETLIB_linalg_SOURCES
     linalg/lsqrBase.cxx linalg/lsqrBase.h
     linalg/lsqrDense.cxx linalg/lsqrDense.h
   )
set(V3P_NETLIB_sparse_SOURCES
     sparse/spAllocate.c
     sparse/spBuild.c
     sparse/spFactor.c
     sparse/spSolve.c
     sparse/spUtils.c
     sparse/spOutput.c
     sparse/spConfig.h
     sparse/spDefs.h
     sparse/spMatrix.h
   )

# Collect full source list.
set(v3p_netlib_sources
     ${V3P_NETLIB_base_SOURCES}
     ${V3P_NETLIB_libf2c_SOURCES}
     ${V3P_NETLIB_blas_SOURCES}
     ${V3P_NETLIB_linpack_SOURCES}
     ${V3P_NETLIB_eispack_SOURCES}
     ${V3P_NETLIB_napack_SOURCES}
     ${V3P_NETLIB_opt_SOURCES}
     ${V3P_NETLIB_linalg_SOURCES}
     ${V3P_NETLIB_sparse_SOURCES}
   )

# Adjust the compiler flags to avoid problems with f2c-generated code.
if(CMAKE_COMPILER_IS_GNUCC)
  # Adjust optimization of floating point computation for some sources.
  # See comments in the sources for details.
  set_source_files_properties(
     linpack/ssvdc.c
     linpack/dsvdc.c
     PROPERTIES COMPILE_FLAGS -ffloat-store
    )

  # Disable global optimizations for some source files.
  # See comments in the sources for details.
  set_source_files_properties(
     PROPERTIES COMPILE_FLAGS -fno-gcse
    )

  # Adjust optimization of floating point computation and disable global
  # optimizations for some sources.
  # See comments in the sources for details.
  set_source_files_properties(
     blas/slamch.c
     blas/dlamch.c
     PROPERTIES COMPILE_FLAGS "-fno-gcse -ffloat-store"
    )
endif()

# Create a netlib library with mangled symbols.
vxl_add_library(
    LIBRARY_NAME itkv3p_netlib
    LIBRARY_SOURCES ${v3p_netlib_sources}
    DISABLE_MSVC_MP
)

target_link_libraries(itkv3p_netlib PRIVATE ${VXL_PLATFORM_MATH_LIBRARIES})
# The linalg/ subdirectory holds the lsqr C++ headers that consumers
# (e.g. vnl_lsqr) include directly; expose it so they need no manual path.
target_include_directories(itkv3p_netlib PUBLIC
    "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/linalg>")

if(BUILD_TESTING)
  add_subdirectory(tests)
endif()
