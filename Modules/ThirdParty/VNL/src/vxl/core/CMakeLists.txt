# vxl/CMakeLists.txt

project( vxlcore )

doxygen_add_book(core "VXL overview documentation")

doxygen_add_package(core "Core Vision Libraries")
doxygen_add_package(core-L2 "Level-2 Vision Libraries")
doxygen_add_package(core-L2E "Level-2 Vision Libraries (Experimental)")
doxygen_add_package(core-test "Test Framework")


set(global_sources
 vxl_copyright.h
 vxl_version.h
)
# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(prefix "${VXL_INSTALL_INCLUDE_DIR}/core")
else()
  set(prefix "${VXL_INSTALL_INCLUDE_DIR}")
endif()
INSTALL_NOBASE_HEADER_FILES(${prefix} ${global_sources})

#
# Do platform-specific configuration.
# configure files
#
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vxl_config.h.in ${PROJECT_BINARY_DIR}/vxl_config.h ${prefix})
include_directories(${PROJECT_BINARY_DIR})

if(VXL_BUILD_CORE_UTILITIES)
  # POSIX layer
  add_subdirectory(vpl)

  # utilities
  add_subdirectory(vul)

  # "basics" (templates)
  add_subdirectory(vbl)
endif()


# numerics
if(VXL_BUILD_CORE_NUMERICS)
  add_subdirectory(vnl)
endif()


# geometry (including computational geometry)
if(VXL_BUILD_CORE_GEOMETRY)
  add_subdirectory(vgl)
endif()


if(VXL_BUILD_CORE_IMAGING)
  # image
  add_subdirectory(vil)

  if(VXL_BUILD_CORE_NUMERICS)
    add_subdirectory (vil/algo)
  endif()
endif()


##- if(NOT VXL_BUILD_NONDEPRECATED_ONLY)  ## NOTE THIS CAN NOT BE DEPRECATED
#                                     # more discussion needed.
#                                     # deprecating vil1 involves MANY
#                                     # other deprecations that need further
#                                     # review
# Old image library (deprecated)
if(VXL_BUILD_CORE_IMAGING ) ## BRL still depends on vil1 AND NOT VXL_BUILD_NONDEPRECATED_ONLY)
  add_subdirectory(vil1)
endif()


if(VXL_BUILD_CORE_SERIALISATION)
  # Fast Binary IO
  add_subdirectory(vsl)

  if(VXL_BUILD_CORE_UTILITIES)
    add_subdirectory(vul/io)
    add_subdirectory(vbl/io)
  endif()

  if(VXL_BUILD_CORE_NUMERICS)
    add_subdirectory(vnl/io)
    add_subdirectory(vnl/xio)
  endif()

  if(VXL_BUILD_CORE_GEOMETRY)
    add_subdirectory(vgl/io)
    add_subdirectory(vgl/xio)
  endif()

  if(VXL_BUILD_CORE_IMAGING)
    add_subdirectory(vil/io)
  endif()
endif()


# coordinate systems
if(VXL_BUILD_CORE_GEOMETRY AND VXL_BUILD_CORE_NUMERICS)
  add_subdirectory(vcsl)
  add_subdirectory(vpgl)
endif()


# probability distributions
if(VXL_BUILD_CORE_PROBABILITY)
  add_subdirectory(vpdl)
endif()


# multi-platform GUI toolkit
# Default the dashboard builds to "YES" so that
# we have some clients that try to compile vgui
CMAKE_DEPENDENT_OPTION( VXL_BUILD_VGUI "Build VGUI" OFF
                         "VXL_BUILD_CORE_GEOMETRY;VXL_BUILD_CORE_NUMERICS;VXL_BUILD_CORE_UTILITIES;VXL_BUILD_CORE_SERIALISATION;VXL_BUILD_CORE_IMAGING" OFF )
mark_as_advanced(VXL_BUILD_VGUI)
if(VXL_BUILD_VGUI)
  add_subdirectory(vgui)
endif()


# video
CMAKE_DEPENDENT_OPTION (VXL_BUILD_CORE_VIDEO "Build VXL's core video libraries" OFF
                        "VXL_BUILD_CORE_UTILITIES;VXL_BUILD_CORE_IMAGING" OFF)
mark_as_advanced(VXL_BUILD_CORE_VIDEO)
if (VXL_BUILD_CORE_VIDEO)
  add_subdirectory(vidl)
  set(CORE_VIDEO_FOUND ON CACHE INTERNAL "VXL core video libraries built")
else()
  set(CORE_VIDEO_FOUND OFF CACHE INTERNAL "VXL core video libraries built")
endif ()

# common test executable
add_subdirectory(testlib)

# Tests that check and output the vxl configuration
# NOTE: some external projects remove the tests directory (aka ITK)
if( BUILD_TESTING AND EXISTS "tests")
  add_subdirectory(tests)
endif()
if( VXL_BUILD_EXAMPLES )
  add_subdirectory(examples)
endif()
