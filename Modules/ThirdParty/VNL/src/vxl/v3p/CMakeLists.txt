# v3p/CMakeLists.txt

project( vthreep )

set( VTHREEP_INCLUDE_DIR ${vthreep_SOURCE_DIR} CACHE STATIC "v3p include files" )
set( VTHREEP_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p CACHE STATIC "v3p include files" )

if(BUILD_CORE_NUMERICS)
  add_subdirectory(netlib)
endif()

CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_CLIPPER "Use V3P instead of any native Clipper library?" ON
                          "BUILD_CORE_GEOMETRY" OFF)
mark_as_advanced( VXL_FORCE_V3P_CLIPPER )
if (VXL_FORCE_V3P_CLIPPER)
  add_subdirectory(clipper)
endif ()

include(${VXL_CMAKE_DIR}/FindZLIB.cmake)
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_ZLIB "Use V3P instead of any native ZLIB library?" OFF
                        "BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_ZLIB )
if (VXL_FORCE_V3P_ZLIB OR ( NOT VXL_USING_NATIVE_ZLIB ) AND IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/zlib)
  add_subdirectory(zlib)
endif()


if(BUILD_CORE_IMAGING)
  option( VXL_FORCE_V3P_BZLIB2 "Use V3P instead of any native BZip2 library?" OFF)
  mark_as_advanced( VXL_FORCE_V3P_BZLIB2 )
  add_subdirectory(bzlib)

  option( VXL_FORCE_V3P_JPEG "Use V3P instead of any native JPEG library?" OFF)
  mark_as_advanced( VXL_FORCE_V3P_JPEG )
  add_subdirectory(jpeg)

  option( VXL_FORCE_V3P_J2K "Use V3P instead of any native JPEG2000 library? (WIN32 only)" OFF)
  mark_as_advanced( VXL_FORCE_V3P_J2K )
  add_subdirectory(j2k)

  option( VXL_FORCE_V3P_TIFF "Use V3P instead of any native TIFF library?" OFF)
  mark_as_advanced( VXL_FORCE_V3P_TIFF )
  add_subdirectory(tiff)

  option( VXL_FORCE_V3P_GEOTIFF "Use V3P instead of any native GEOTIFF library?" OFF)
  mark_as_advanced( VXL_FORCE_V3P_GEOTIFF )
  add_subdirectory(geotiff)

  option( VXL_FORCE_V3P_PNG "Use V3P instead of any native PNG library?" OFF)
  mark_as_advanced( VXL_FORCE_V3P_PNG )
  add_subdirectory(png)

  option( VXL_FORCE_V3P_DCMTK "Use V3P instead of any native DCMTK library?" OFF)
  mark_as_advanced( VXL_FORCE_V3P_DCMTK )
  add_subdirectory(dcmtk)

  option( VXL_FORCE_V3P_OPENJPEG2 "Use V3P instead of any native OpenJPEG v2 library?" OFF)
  mark_as_advanced( VXL_FORCE_V3P_OPENJPEG2 )
  add_subdirectory(openjpeg2)

  add_subdirectory(rply)

endif()
