project(ITKVNL)
set(ITKVNL_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_VXL)
  find_package(VXL 4.0.0 REQUIRED)
  set(
    ITKVNL_SYSTEM_INCLUDE_DIRS
    ${VXL_NETLIB_INCLUDE_DIR}
    ${VXL_VCL_INCLUDE_DIRS}
    ${VXL_VCL_INCLUDE_DIR}
    ${VXL_CORE_INCLUDE_DIRS}
    ${VXL_CORE_INCLUDE_DIR}
  )
  set(
    ITKVNL_LIBRARIES
    vnl_algo
    vnl
    v3p_netlib
    netlib
  )
  set(ITKVNL_SYSTEM_LIBRARY_DIRS "${VXL_LIBRARY_DIR}")
  set(ITKVNL_NO_SRC 1)
else()
  # The ITK-stripped vxl is numerics-only and hardcodes its former configure
  # options (bounds-checking off, thread-safe on, core-numerics on, no failing
  # tests, no introspection), so there are no VXL_* options left to force here;
  # only the library properties are passed through from ITK.
  set(VXL_LIBRARY_PROPERTIES ${ITK_LIBRARY_PROPERTIES})

  set(
    ITKVNL_INCLUDE_DIRS
    ${ITKVNL_SOURCE_DIR}/src/vxl/v3p/netlib
    ${ITKVNL_SOURCE_DIR}/src/vxl/vcl
    ${ITKVNL_SOURCE_DIR}/src/vxl/core
    ${ITKVNL_BINARY_DIR}/src/vxl/v3p/netlib
    ${ITKVNL_BINARY_DIR}/src/vxl/vcl
    ${ITKVNL_BINARY_DIR}/src/vxl/core
  )
  set(
    ITKVNL_LIBRARIES
    itkvnl_algo
    itkvnl
    itkv3p_netlib
  )

  if(ITK_TEMPLATE_VISIBILITY_DEFAULT)
    add_definitions(
      "-DVNL_TEMPLATE_EXPORT=__attribute__((visibility(\"default\")))"
    )
  endif()
endif()

itk_module_impl()

mark_as_advanced(FORCE TESTLIB_VCL_WHERE_ROOT_DIR_H)
