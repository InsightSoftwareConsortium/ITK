# v3p/CMakeLists.txt

project( vthreep )

set( VTHREEP_INCLUDE_DIR ${vthreep_SOURCE_DIR} CACHE STATIC "v3p include files" )
set( VTHREEP_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p CACHE STATIC "v3p include files" )

add_subdirectory(netlib)
