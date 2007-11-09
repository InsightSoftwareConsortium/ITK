# v3p/CMakeLists.txt

PROJECT( vthreep )

SET( VTHREEP_INCLUDE_DIR ${vthreep_SOURCE_DIR} CACHE STATIC "v3p include files" )
SET( VTHREEP_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p CACHE STATIC "v3p include files" )

# The borland compiler produces lots of warnings like
# Warning W8004 C:\cygwin\tmp\vxl_nightly_borland\vxl_src\v3p\tiff\tif_fax3.c 256:
#      'a0' is assigned a value that is never used in function Fax3Decode1D
# There is no point in us fixing them locally. IMS
IF (BORLAND)
  SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-8004")
  SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w-8004")
ENDIF (BORLAND)


IF (BUILD_CORE_NUMERICS)
  SUBDIRS(netlib)
ENDIF (BUILD_CORE_NUMERICS)

IF (BUILD_VGUI)
  SUBDIRS(Qv)
ENDIF (BUILD_VGUI)

IF (BUILD_CORE_IMAGING)
  OPTION( VXL_FORCE_V3P_ZLIB "Use V3P instead of any native ZLIB library?" NO)
  MARK_AS_ADVANCED( VXL_FORCE_V3P_ZLIB )
  SUBDIRS(zlib)

  OPTION( VXL_FORCE_V3P_JPEG "Use V3P instead of any native JPEG library?" NO)
  MARK_AS_ADVANCED( VXL_FORCE_V3P_JPEG )
  SUBDIRS(jpeg)

  OPTION( VXL_FORCE_V3P_TIFF "Use V3P instead of any native TIFF library?" NO)
  MARK_AS_ADVANCED( VXL_FORCE_V3P_TIFF )
  SUBDIRS(tiff)

  OPTION( VXL_FORCE_V3P_GEOTIFF "Use V3P instead of any native GEOTIFF library?" NO)
  MARK_AS_ADVANCED( VXL_FORCE_V3P_GEOTIFF )
  SUBDIRS(geotiff)

  OPTION( VXL_FORCE_V3P_PNG "Use V3P instead of any native PNG library?" NO)
  MARK_AS_ADVANCED( VXL_FORCE_V3P_PNG )
  SUBDIRS(png)
  
  OPTION( VXL_FORCE_V3P_MPEG2 "Use V3P instead of any native MPEG2 library?" NO)
  MARK_AS_ADVANCED( VXL_FORCE_V3P_MPEG2 )
  SUBDIRS(mpeg2)

  OPTION( VXL_FORCE_V3P_DCMTK "Use V3P instead of any native DCMTK library?" NO)
  MARK_AS_ADVANCED( VXL_FORCE_V3P_DCMTK )
  SUBDIRS(dcmtk)
	
  SUBDIRS(rply)

ENDIF (BUILD_CORE_IMAGING)
