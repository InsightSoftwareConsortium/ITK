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

set(MANGLE_PREFIX itk_tiff)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/itk_tiff_mangle.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/itk_tiff_mangle.h
               @ONLY)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/itktiff.def.in
               ${CMAKE_CURRENT_BINARY_DIR}/itktiff.def
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


add_library(itktiff ${CMAKE_CURRENT_BINARY_DIR}/itktiff.def)

target_sources(itktiff PRIVATE
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
target_compile_definitions(itktiff PRIVATE TIFF_DO_NOT_USE_NON_EXT_ALLOC_FUNCTIONS)
set_property(SOURCE tif_open.c APPEND PROPERTY COMPILE_DEFINITIONS ALLOW_TIFF_NON_EXT_ALLOC_FUNCTIONS)

if(USE_WIN32_FILEIO)
  target_sources(itktiff PRIVATE tif_win32.c)
  set_property(SOURCE tif_win32.c APPEND PROPERTY COMPILE_DEFINITIONS ALLOW_TIFF_NON_EXT_ALLOC_FUNCTIONS)
else()
  target_sources(itktiff PRIVATE tif_unix.c)
  set_property(SOURCE tif_unix.c APPEND PROPERTY COMPILE_DEFINITIONS ALLOW_TIFF_NON_EXT_ALLOC_FUNCTIONS)
endif()

target_include_directories(itktiff
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
    PRIVATE
        ${TIFF_INCLUDES}
)

if(FALSE) # XXX(ITK): ITK does not need TIFF library version info
# Windows specific resource preparation
if (WIN32)
  message(STATUS "Include resource version information for LibTIFF")
  target_sources(itktiff PRIVATE tif_win32_versioninfo.rc)
  source_group("Resource Files" FILES tif_win32_versioninfo.rc)
endif ()
endif()


set(tiff_libs_private_list "") # cmake list
set(tiff_requires_private "") # cmake string

if(ZIP_SUPPORT)
  target_link_libraries(itktiff PRIVATE ${ITKZLIB_LIBRARIES})
  string(APPEND tiff_requires_private " zlib")
endif()
if(ZIP_SUPPORT AND LIBDEFLATE_SUPPORT)
  target_link_libraries(itktiff PRIVATE Deflate::Deflate)
  if(Deflate_VERSION_STRING VERSION_GREATER_EQUAL "1.9")
    string(APPEND tiff_requires_private " libdeflate")
  else()
    list(APPEND tiff_libs_private_list "${Deflate_LIBRARY}")
  endif()
endif()
if(JPEG_SUPPORT)
  target_link_libraries(itktiff PRIVATE ${ITKJPEG_LIBRARIES})
  string(APPEND tiff_requires_private " libjpeg")
  if(HAVE_JPEGTURBO_DUAL_MODE_8_12)
    target_compile_definitions(itktiff PRIVATE HAVE_JPEGTURBO_DUAL_MODE_8_12)
  elseif(JPEG_DUAL_MODE_8_12)
    target_include_directories(itktiff PRIVATE ${JPEG12_INCLUDE_DIR})
    target_link_libraries(itktiff PRIVATE ${JPEG12_LIBRARIES})
  endif()
endif()
if(JBIG_SUPPORT)
  target_link_libraries(itktiff PRIVATE JBIG::JBIG)
  list(APPEND tiff_libs_private_list "${JBIG_LIBRARY}")
endif()
if(LERC_SUPPORT)
  target_link_libraries(itktiff PRIVATE LERC::LERC)
  if(LERC_VERSION_STRING VERSION_GREATER_EQUAL "4.0")
    if(NOT BUILD_SHARED_LIBS)
      set_target_properties(itktiff PROPERTIES COMPILE_DEFINITIONS LERC_STATIC)
    endif()
    string(APPEND tiff_requires_private " Lerc")
  else()
    list(APPEND tiff_libs_private_list "${LERC_LIBRARY}")
  endif()
endif()
if(LZMA_SUPPORT)
  target_link_libraries(itktiff PRIVATE liblzma::liblzma)
  string(APPEND tiff_requires_private " liblzma")
endif()
if(ZSTD_SUPPORT)
  target_link_libraries(itktiff PRIVATE ZSTD::ZSTD)
  string(APPEND tiff_requires_private " libzstd")
endif()
if(WEBP_SUPPORT)
  target_link_libraries(itktiff PRIVATE WebP::webp)
  string(APPEND tiff_requires_private " libwebp")
endif()
if(CMath_LIBRARY)
  target_include_directories(itktiff
          PRIVATE
            ${CMath_INCLUDE_DIRS}
  )
  target_link_libraries(itktiff PRIVATE "${CMath_LIBRARIES}")
  if(FALSE) # XXX(ITK): We don't want to use an imported target for CMath
  target_link_libraries(itktiff PRIVATE CMath::CMath)
  list(APPEND tiff_libs_private_list "${CMath_LIBRARY}")
  endif()
endif()

set(tiff_libs_private_list "${tiff_libs_private_list}" PARENT_SCOPE)
set(tiff_requires_private "${tiff_requires_private}" PARENT_SCOPE)

if(FALSE) # XXX(ITK): ITK handles library properties and installation
set_target_properties(tiff PROPERTIES SOVERSION ${SO_COMPATVERSION})
if(NOT CYGWIN)
    # This property causes shared libraries on Linux to have the full version
    # encoded into their final filename.  We disable this on Cygwin because
    # it causes cygz-${TIFF_FULL_VERSION}.dll to be created when cygz.dll
    # seems to be the default.
    set_target_properties(tiff PROPERTIES VERSION ${SO_VERSION})
endif()
if(HAVE_LD_VERSION_SCRIPT)
  set_target_properties(tiff PROPERTIES LINK_FLAGS
                        "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/libtiff.map")
endif()
else()
# Apply user-defined properties to the library target.
if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(itktiff PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

install(TARGETS itktiff
  EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
  RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
  LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
  ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development
  )
endif()

if(tiff-install)
  export(TARGETS tiff NAMESPACE TIFF:: FILE "${CMAKE_CURRENT_BINARY_DIR}/libtiffTargets.cmake")
  install(TARGETS tiff EXPORT TiffTargets
          RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
          LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
          ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

  install(FILES ${tiff_public_HEADERS}
          DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")

  set(TIFF_CONFIGDIR ${CMAKE_INSTALL_LIBDIR}/cmake/tiff)
  install(EXPORT TiffTargets
          FILE TiffTargets.cmake
          NAMESPACE TIFF::
          DESTINATION ${TIFF_CONFIGDIR}
  )

  include(CMakePackageConfigHelpers)
  write_basic_package_version_file(
    TiffConfigVersion.cmake
    VERSION ${LIBTIFF_VERSION}
    COMPATIBILITY SameMajorVersion)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/TiffConfigVersion.cmake DESTINATION ${TIFF_CONFIGDIR})

  configure_file(${PROJECT_SOURCE_DIR}/cmake/TiffConfig.cmake.in
                 ${CMAKE_CURRENT_BINARY_DIR}/TiffConfig.cmake @ONLY)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/TiffConfig.cmake DESTINATION ${TIFF_CONFIGDIR})
endif()

if(CXX_SUPPORT)
  set(tiffxx_HEADERS
          tiffio.hxx)

  # No .def file for this library.
  if (WIN32 AND NOT MINGW)
      add_library(tiffxx STATIC ../placeholder.h)
  else()
      add_library(tiffxx ../placeholder.h)
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
    # it causes cygz-${TIFF_FULL_VERSION}.dll to be created when cygz.dll
    # seems to be the default.
    set_target_properties(tiffxx PROPERTIES VERSION ${SO_VERSION})
  endif()
  if(HAVE_LD_VERSION_SCRIPT)
    set_target_properties(tiffxx PROPERTIES LINK_FLAGS
                          "-Wl,--version-script=${CMAKE_CURRENT_SOURCE_DIR}/libtiffxx.map")
  endif()

  if(FALSE) # XXX(ITK): ITK handles installation
  if(tiff-install)
    install(TARGETS tiffxx
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

    install(FILES ${tiffxx_HEADERS}
            DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")
  endif()
  endif()

endif()

return() # XXX(ITK): We do not need TIFF tools.

if(NOT CMAKE_CROSSCOMPILING)
  add_executable(tiff_mkg3states ../placeholder.h)
  target_sources(tiff_mkg3states PRIVATE mkg3states.c tif_fax3.h)
  target_link_libraries(tiff_mkg3states tiff tiff_port)

  add_custom_target(tiff_faxtable
          DEPENDS tiff_mkg3states
          COMMAND ${CMAKE_COMMAND} -E rm "tif_fax3sm.c"
          COMMAND tiff_mkg3states -b -c const "tif_fax3sm.c"
          WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
endif()
