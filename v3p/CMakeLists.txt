# v3p/CMakeLists.txt

project( vthreep )

set( VTHREEP_INCLUDE_DIR ${vthreep_SOURCE_DIR} CACHE STATIC "v3p include files" )
set( VTHREEP_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p CACHE STATIC "v3p include files" )

if(VXL_BUILD_CORE_NUMERICS)
  add_subdirectory(netlib)
endif()

CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_CLIPPER "Use V3P instead of any native Clipper library?" ON
    "VXL_BUILD_CORE_GEOMETRY" OFF)
mark_as_advanced( VXL_FORCE_V3P_CLIPPER )
if (VXL_FORCE_V3P_CLIPPER)
  add_subdirectory(clipper)
endif ()

CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_ZLIB "Use V3P instead of any native ZLIB library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_ZLIB )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_BZLIB2 "Use V3P instead of any native BZip2 library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_BZLIB2 )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_JPEG "Use V3P instead of any native JPEG library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_JPEG )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_J2K "Use V3P instead of any native JPEG2000 library? (WIN32 only)" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_J2K )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_TIFF "Use V3P instead of any native TIFF library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_TIFF )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_GEOTIFF "Use V3P instead of any native GEOTIFF library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_GEOTIFF )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_PNG "Use V3P instead of any native PNG library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_PNG )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_DCMTK "Use V3P instead of any native DCMTK library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_DCMTK )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_OPENJPEG2 "Use V3P instead of any native OpenJPEG v2 library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_OPENJPEG2 )
CMAKE_DEPENDENT_OPTION( VXL_FORCE_V3P_RPLY "Use V3P instead of any native RPLY library?" OFF
    "VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced( VXL_FORCE_V3P_RPLY )

if(VXL_BUILD_CORE_IMAGING)
  include(${VXL_CMAKE_DIR}/FindZLIB.cmake)
  if (VXL_FORCE_V3P_ZLIB OR ( NOT VXL_USING_NATIVE_ZLIB ) AND IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/zlib)
    add_subdirectory(zlib)
  endif()
  add_subdirectory(bzlib)
  add_subdirectory(jpeg)
  add_subdirectory(j2k)
  add_subdirectory(tiff)
  add_subdirectory(geotiff)
  add_subdirectory(png)
  add_subdirectory(dcmtk)
  add_subdirectory(openjpeg2)
  add_subdirectory(rply)
endif()
