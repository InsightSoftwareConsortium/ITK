INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH}
                    ${CABLE_INCLUDE_DIR}
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/doc
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl
                    ${ITK_SOURCE_DIR}/Wrapping/Tcl)

SUBDIRS(Numerics Common BasicFilters IO Algorithms Executable)

INCLUDE(${ITK_SOURCE_DIR}/Wrapping/Tcl/itkConfigureTclPackages.cmake)
