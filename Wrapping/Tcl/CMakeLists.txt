INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH}
                    ${CABLE_INCLUDE_DIR})

SUBDIRS(Common)

INCLUDE(${ITK_SOURCE_DIR}/Wrapping/Tcl/itkConfigureTclPackages.cmake)
