# vxl/CMakeLists.txt

PROJECT( vxlcore )

doxygen_add_book(core "VXL overview documentation")

doxygen_add_package(core "Core Vision Libraries")
doxygen_add_package(core-L2 "Level-2 Vision Libraries")
doxygen_add_package(core-L2E "Level-2 Vision Libraries (Experimental)")
doxygen_add_package(core-test "Test Framework")

SET(global_sources
 vxl_copyright.h
 vxl_version.h
 vxl_config.h.in
)

INSTALL_NOBASE_HEADER_FILES(/include/vxl/core ${global_sources})

# common test executable
SUBDIRS(testlib)

# Tests that check and output the vxl configuration
IF( BUILD_TESTING )
  SUBDIRS( tests )
ENDIF( BUILD_TESTING )


IF (BUILD_CORE_UTILITIES)
  # POSIX layer
  SUBDIRS(vpl)

  # utilities
  SUBDIRS(vul)

  # "basics" (templates)
  SUBDIRS(vbl)
ENDIF (BUILD_CORE_UTILITIES)


# numerics
IF (BUILD_CORE_NUMERICS)
  SUBDIRS(vnl)
ENDIF (BUILD_CORE_NUMERICS)


# geometry (including computational geometry)
IF (BUILD_CORE_GEOMETRY)
  SUBDIRS(vgl)
ENDIF (BUILD_CORE_GEOMETRY)


IF (BUILD_CORE_IMAGING)
  # image
  SUBDIRS(vil)

  IF (BUILD_CORE_NUMERICS)
    SUBDIRS (vil/algo)
  ENDIF (BUILD_CORE_NUMERICS)

  # Old image library (deprecated)
  SUBDIRS(vil1)
ENDIF (BUILD_CORE_IMAGING)

IF (BUILD_CORE_SERIALISATION)
  # Fast Binary IO
  SUBDIRS(vsl)

  IF (BUILD_CORE_UTILITIES)
    SUBDIRS(vul/io vbl/io)
  ENDIF (BUILD_CORE_UTILITIES)

  IF (BUILD_CORE_NUMERICS)
    SUBDIRS(vnl/io vnl/xio)
  ENDIF (BUILD_CORE_NUMERICS)

  IF (BUILD_CORE_GEOMETRY)
    SUBDIRS(vgl/io vgl/xio)
  ENDIF (BUILD_CORE_GEOMETRY)

  IF (BUILD_CORE_IMAGING)
    SUBDIRS(vil/io)
    # I/O for the ld image library (deprecated):
    SUBDIRS(vil1/io)
  ENDIF (BUILD_CORE_IMAGING)
ENDIF (BUILD_CORE_SERIALISATION)


# coordinate systems
IF (BUILD_CORE_GEOMETRY AND BUILD_CORE_NUMERICS)
  SUBDIRS(vcsl)
ENDIF (BUILD_CORE_GEOMETRY AND BUILD_CORE_NUMERICS)

# probability distributions
IF (BUILD_CORE_PROBABILITY)
  SUBDIRS( vpdl )
ENDIF (BUILD_CORE_PROBABILITY)

IF (BUILD_CORE_ALL)
# multi-platform GUI toolkit
  SUBDIRS(vgui)

  IF(BUILD_EXAMPLES)
    SUBDIRS(examples)
  ENDIF(BUILD_EXAMPLES)
ENDIF (BUILD_CORE_ALL)

# video
SET (CORE_VIDEO_FOUND NO CACHE INTERNAL "VXL core video libraries built")
IF (BUILD_CORE_UTILITIES AND BUILD_CORE_IMAGING)
  OPTION (BUILD_CORE_VIDEO "Build VXL's core video libraries" YES)
  IF (BUILD_CORE_VIDEO)
    SUBDIRS(vidl)
    # deprecated: old video library:
    IF (BUILD_UNMAINTAINED_LIBRARIES OR BUILD_FOR_VXL_DASHBOARD)
      SUBDIRS(vidl1)
      SUBDIRS(vidl_vil1)
    ENDIF (BUILD_UNMAINTAINED_LIBRARIES OR BUILD_FOR_VXL_DASHBOARD)
    SET (CORE_VIDEO_FOUND YES CACHE INTERNAL "VXL core video libraries built")
  ENDIF (BUILD_CORE_VIDEO)
ENDIF (BUILD_CORE_UTILITIES AND BUILD_CORE_IMAGING)
