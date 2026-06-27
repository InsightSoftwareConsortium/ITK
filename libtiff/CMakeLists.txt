# CMake build for libtiff
#
# Copyright © 2015 Open Microscopy Environment / University of Dundee
# Written by Roger Leigh <rleigh@codelibre.net>
#
# Permission to use, copy, modify, distribute, and sell this software and
# its documentation for any purpose is hereby granted without fee, provided
# that (i) the above copyright notices and this permission notice appear in
# all copies of the software and related documentation, and (ii) the names of
# Sam Leffler and Silicon Graphics may not be used in any advertising or
# publicity relating to the software without the specific, prior written
# permission of Sam Leffler and Silicon Graphics.
#
# THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
# EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
#
# IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
# ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
# OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
# LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
# OF THIS SOFTWARE.

# Generate headers
# and update configuration settings and package / version information.
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/tif_config.h.cmake.in
               ${CMAKE_CURRENT_BINARY_DIR}/tif_config.h
               @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/tiffconf.h.cmake.in
               ${CMAKE_CURRENT_BINARY_DIR}/tiffconf.h
               @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/tiffvers.h.cmake.in
               ${CMAKE_CURRENT_BINARY_DIR}/tiffvers.h
               @ONLY)

set(tiff_public_HEADERS
        tiff.h
        tiffio.h
        ${CMAKE_CURRENT_BINARY_DIR}/tiffvers.h
        ${CMAKE_CURRENT_BINARY_DIR}/tiffconf.h)

set(tiff_private_HEADERS
        t4.h
        tif_dir.h
        tif_predict.h
        tiffiop.h
        uvcode.h
        ${CMAKE_CURRENT_BINARY_DIR}/tif_config.h)


add_library(tiff ${TIFF_BUILD_LIB_VALUE} libtiff.def)
add_library(TIFF::tiff ALIAS tiff)

target_sources(tiff PRIVATE
        ${tiff_public_HEADERS}
        ${tiff_private_HEADERS}
        tif_aux.c
        tif_close.c
        tif_codec.c
        tif_color.c
        tif_compress.c
        tif_dir.c
        tif_dirinfo.c
        tif_dirread.c
        tif_dirwrite.c
        tif_dumpmode.c
        tif_error.c
        tif_extension.c
        tif_fax3.c
        tif_fax3sm.c
        tif_flush.c
        tif_getimage.c
        tif_hash_set.c
        tif_jbig.c
        tif_jpeg.c
        tif_jpeg_12.c
        tif_lerc.c
        tif_luv.c
        tif_lzma.c
        tif_lzw.c
        tif_next.c
        tif_ojpeg.c
        tif_open.c
        tif_packbits.c
        tif_pixarlog.c
        tif_predict.c
        tif_print.c
        tif_read.c
        tif_strip.c
        tif_swab.c
        tif_thunder.c
        tif_tile.c
        tif_version.c
        tif_warning.c
        tif_webp.c
        tif_write.c
        tif_zip.c
        tif_zstd.c)

if (CMAKE_VERSION VERSION_GREATER_EQUAL 3.16)
  set_property(SOURCE tif_jpeg.c tif_jpeg12.c PROPERTY SKIP_UNITY_BUILD_INCLUSION ON)
endif ()

# For all files (but tif_open.c, tif_unix.c and tif_win32.c), forbid the use
# of _TIFFmalloc/_TIFFfree and require the use of their "Ext" versions
target_compile_definitions(tiff PRIVATE TIFF_DO_NOT_USE_NON_EXT_ALLOC_FUNCTIONS)
set_property(SOURCE tif_open.c APPEND PROPERTY COMPILE_DEFINITIONS ALLOW_TIFF_NON_EXT_ALLOC_FUNCTIONS)

if(USE_WIN32_FILEIO)
  target_sources(tiff PRIVATE tif_win32.c)
  set_property(SOURCE tif_win32.c APPEND PROPERTY COMPILE_DEFINITIONS ALLOW_TIFF_NON_EXT_ALLOC_FUNCTIONS)
else()
  target_sources(tiff PRIVATE tif_unix.c)
  set_property(SOURCE tif_unix.c APPEND PROPERTY COMPILE_DEFINITIONS ALLOW_TIFF_NON_EXT_ALLOC_FUNCTIONS)
endif()

# Apply C++ compatibility mode if enabled
tiff_target_compile_as_cxx(tiff)

target_include_directories(tiff
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    PRIVATE
        ${TIFF_INCLUDES}
)

# Windows specific resource preparation
if (WIN32)
  message(STATUS "Include resource version information for LibTIFF")
  target_sources(tiff PRIVATE tif_win32_versioninfo.rc)
  source_group("Resource Files" FILES tif_win32_versioninfo.rc)
endif ()


set(tiff_libs_private_list "") # cmake list
set(tiff_requires_private "") # cmake string

if(ZIP_SUPPORT)
  target_link_libraries(tiff PRIVATE ZLIB::ZLIB)
  string(APPEND tiff_requires_private " zlib")
endif()
if(ZIP_SUPPORT AND LIBDEFLATE_SUPPORT)
  target_link_libraries(tiff PRIVATE Deflate::Deflate)
  if(Deflate_VERSION_STRING VERSION_GREATER_EQUAL "1.9")
    string(APPEND tiff_requires_private " libdeflate")
  else()
    list(APPEND tiff_libs_private_list "${Deflate_LIBRARY}")
  endif()
endif()
if(JPEG_SUPPORT)
  target_link_libraries(tiff PRIVATE JPEG::JPEG)
  string(APPEND tiff_requires_private " libjpeg")
  if(JPEG_DUAL_MODE_8_12 AND NOT HAVE_JPEGTURBO_DUAL_MODE_8_12)
    target_include_directories(tiff PRIVATE ${JPEG12_INCLUDE_DIR})
    target_link_libraries(tiff PRIVATE ${JPEG12_LIBRARIES})
  endif()
endif()
if(JBIG_SUPPORT)
  target_link_libraries(tiff PRIVATE JBIG::JBIG)
  list(APPEND tiff_libs_private_list "${JBIG_LIBRARY}")
endif()
if(LERC_SUPPORT)
  target_link_libraries(tiff PRIVATE LERC::LERC)
  if(LERC_VERSION_STRING VERSION_GREATER_EQUAL "4.0")
    string(APPEND tiff_requires_private " Lerc")
  else()
    list(APPEND tiff_libs_private_list "${LERC_LIBRARY}")
  endif()
endif()
if(LZMA_SUPPORT)
  target_link_libraries(tiff PRIVATE liblzma::liblzma)
  string(APPEND tiff_requires_private " liblzma")
endif()
if(ZSTD_SUPPORT)
  target_link_libraries(tiff PRIVATE ZSTD::ZSTD)
  string(APPEND tiff_requires_private " libzstd")
endif()
if(WEBP_SUPPORT)
  target_link_libraries(tiff PRIVATE WebP::webp)
  string(APPEND tiff_requires_private " libwebp")
endif()
if(CMath_LIBRARY)
  target_link_libraries(tiff PRIVATE ${CMath_LIBRARIES})
  list(APPEND tiff_libs_private_list "${CMath_LIBRARIES}")
endif()

set(tiff_libs_private_list "${tiff_libs_private_list}" PARENT_SCOPE)
set(tiff_requires_private "${tiff_requires_private}" PARENT_SCOPE)

set_target_properties(tiff PROPERTIES SOVERSION ${SO_COMPATVERSION})
if(MINGW)
  # This causes "-${SOVERSION}" to be added to shared libraries, so it matches
  # Autotools output as well.  This setting is already enabled on Cygwin/MSYS
  # by default.  We don't enable it for other Windows builds, as there is no
  # Autotools output to match there.
  set(CMAKE_SHARED_LIBRARY_NAME_WITH_VERSION 1)
endif()
if(NOT CYGWIN)
    # This property causes shared libraries on Linux to have the full version
    # encoded into their final filename.  We disable this on Cygwin because
    # it causes cygtiff-${TIFF_FULL_VERSION}.dll to be created when
    # cygtiff-${SOVERSION}.dll seems to be the default.
    set_target_properties(tiff PROPERTIES VERSION ${SO_VERSION})
endif()
if(HAVE_LD_VERSION_SCRIPT)
  if (CMAKE_VERSION GREATER_EQUAL 3.29 AND CMAKE_C_COMPILER_LINKER_ID STREQUAL "LLD" AND CMAKE_C_COMPILER_LINKER_VERSION GREATER_EQUAL 17)
    set_target_properties(tiff PROPERTIES LINK_FLAGS
                              "-Wl,--undefined-version -Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/libtiff.map")
  else()
    set_target_properties(tiff PROPERTIES LINK_FLAGS
                              "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/libtiff.map")
  endif()
endif()

macro(set_framework_properties target_name headers_var)
  if(tiff-framework)
    set_target_properties(${target_name} PROPERTIES
      FRAMEWORK TRUE
      FRAMEWORK_VERSION "A"
      PRODUCT_BUNDLE_IDENTIFIER "com.gitlab.libtiff.libtiff_${target_name}"
      PUBLIC_HEADER "${${headers_var}}"
      XCODE_ATTRIBUTE_INSTALL_PATH "@rpath"
      XCODE_ATTRIBUTE_CODE_SIGN_IDENTITY ""
      XCODE_ATTRIBUTE_CODE_SIGNING_ALLOWED "NO"
      XCODE_ATTRIBUTE_CODE_SIGNING_REQUIRED "NO"
      MACOSX_FRAMEWORK_IDENTIFIER "com.gitlab.libtiff.libtiff_${target_name}"
      MACOSX_FRAMEWORK_BUNDLE_VERSION "${LIBTIFF_VERSION_FULL}"
      MACOSX_FRAMEWORK_SHORT_VERSION_STRING "${LIBTIFF_RELEASE_VERSION}"
      MACOSX_RPATH TRUE)
  endif()
endmacro()

set_framework_properties(tiff tiff_public_HEADERS)

if(tiff-install)
  export(TARGETS tiff NAMESPACE TIFF:: FILE "${CMAKE_CURRENT_BINARY_DIR}/libtiffTargets.cmake")
  install(TARGETS tiff EXPORT TiffTargets
          RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
          LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
          ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
          FRAMEWORK DESTINATION ${CMAKE_INSTALL_LIBDIR})

  install(FILES ${tiff_public_HEADERS}
          DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")

  set(TIFF_CONFIGDIR ${CMAKE_INSTALL_LIBDIR}/cmake/tiff)
  install(EXPORT TiffTargets
          FILE tiff-targets.cmake
          NAMESPACE TIFF::
          DESTINATION ${TIFF_CONFIGDIR}
  )

  include(CMakePackageConfigHelpers)
  write_basic_package_version_file(
    tiff-config-version.cmake
    VERSION ${LIBTIFF_VERSION}
    COMPATIBILITY SameMajorVersion)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/tiff-config-version.cmake DESTINATION ${TIFF_CONFIGDIR})

  configure_file(${PROJECT_SOURCE_DIR}/cmake/tiff-config.cmake.in
                 ${CMAKE_CURRENT_BINARY_DIR}/tiff-config.cmake @ONLY)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/tiff-config.cmake DESTINATION ${TIFF_CONFIGDIR})
endif()

if(CXX_SUPPORT)
  set(tiffxx_HEADERS
          tiffio.hxx)

  # No .def file for this library.
  if (WIN32 AND NOT MINGW)
      add_library(tiffxx STATIC ../placeholder.h)
  else()
      add_library(tiffxx ${TIFF_BUILD_LIB_VALUE} ../placeholder.h)
  endif()

  add_library(TIFF::CXX ALIAS tiffxx)

  target_sources(tiffxx PRIVATE
          ${tiffxx_HEADERS}
          tif_stream.cxx)
  target_link_libraries(tiffxx tiff)
  set_target_properties(tiffxx PROPERTIES SOVERSION ${SO_COMPATVERSION})
  if(NOT CYGWIN)
    # This property causes shared libraries on Linux to have the full version
    # encoded into their final filename.  We disable this on Cygwin because
    # it causes cygtiffxx-${TIFF_FULL_VERSION}.dll to be created when
    # cygtiffxx-${SOVERSION}.dll seems to be the default.
    set_target_properties(tiffxx PROPERTIES VERSION ${SO_VERSION})
  endif()
  if(HAVE_LD_VERSION_SCRIPT)
    set_target_properties(tiffxx PROPERTIES LINK_FLAGS
                          "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/libtiffxx.map")
  endif()

  set_framework_properties(tiffxx tiffxx_HEADERS)

  if(tiff-install)
    install(TARGETS tiffxx EXPORT TiffTargets
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
            FRAMEWORK DESTINATION ${CMAKE_INSTALL_LIBDIR})

    install(FILES ${tiffxx_HEADERS}
            DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")
  endif()

endif()

