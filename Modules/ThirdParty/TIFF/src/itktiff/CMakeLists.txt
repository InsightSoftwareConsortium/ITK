project(ITK3P_TIFF)
include_regular_expression("^(itk|tif|fax|port|uv|tconf|t4|mk|itkjpeg|jpeg|mangle|jconfig|jmorecfg).*$")

include_directories(BEFORE ${ITK3P_TIFF_SOURCE_DIR})
include_directories(BEFORE ${ITK3P_TIFF_BINARY_DIR})

# library object files common to compression and decompression
set(common_SRCS
  tif_aux.c
  tif_close.c
  tif_codec.c
  tif_compress.c
  tif_color.c
  tif_dir.c
  tif_dirinfo.c
  tif_dirread.c
  tif_dirwrite.c
  tif_dumpmode.c
  tif_error.c
  tif_extension.c
  tif_fax3.c
  tif_flush.c
  tif_getimage.c
  tif_luv.c
  tif_lzw.c
  tif_next.c
  tif_open.c
  tif_packbits.c
  tif_pixarlog.c
  tif_predict.c
  tif_print.c
  tif_read.c
  tif_strip.c
  tif_swab.c
  tif_tile.c
  tif_thunder.c
  tif_version.c
  tif_warning.c
  tif_write.c
  tif_zip.c
  tif_jpeg.c
  )

add_definitions(-DJPEG_SUPPORT -DZIP_SUPPORT -D_HPUX_SOURCE)

if(WIN32)
  if(BUILD_SHARED_LIBS)
    set(TIFFDLL 1)
  else(BUILD_SHARED_LIBS)
    set(TIFFSTATIC 1)
  endif(BUILD_SHARED_LIBS)
endif(WIN32)

if(BORLAND)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-8004 -w-8057")
endif(BORLAND)

include(TestBigEndian)
include(CheckIncludeFiles)
include(CheckSymbolExists)
include(CheckFunctionExists)

# Check if header file exists and add it to the list.
CHECK_INCLUDE_FILES("assert.h" HAVE_ASSERT_H)
CHECK_INCLUDE_FILES("dlfcn.h" HAVE_DLFCN_H)
CHECK_INCLUDE_FILES("fcntl.h" HAVE_FCNTL_H)
CHECK_INCLUDE_FILES("inttypes.h" HAVE_INTTYPES_H)
CHECK_INCLUDE_FILES("limits.h" HAVE_LIMITS_H)
CHECK_INCLUDE_FILES("malloc.h" HAVE_MALLOC_H)
CHECK_INCLUDE_FILES("memory.h" HAVE_MEMORY_H)
CHECK_INCLUDE_FILES("stdint.h" HAVE_STDINT_H)
CHECK_INCLUDE_FILES("stdlib.h" HAVE_STDLIB_H)
CHECK_INCLUDE_FILES("string.h" HAVE_STRING_H)
CHECK_INCLUDE_FILES("strings.h" HAVE_STRINGS_H)
CHECK_INCLUDE_FILES("sys/stat.h" HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILES("sys/time.h" HAVE_SYS_TIME_H)
CHECK_INCLUDE_FILES("sys/types.h" HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILES("unistd.h" HAVE_UNISTD_H)
CHECK_INCLUDE_FILES("windows.h" HAVE_WINDOWS_H)
CHECK_INCLUDE_FILES("strings.h" HAVE_STRINGS_H)
CHECK_INCLUDE_FILES("ieeefp.h" HAVE_IEEEFP_H)
CHECK_INCLUDE_FILES("search.h" HAVE_SEARCH_H)


CHECK_FUNCTION_EXISTS(floor HAVE_FLOOR)
CHECK_FUNCTION_EXISTS(getopt HAVE_GETOPT)
CHECK_FUNCTION_EXISTS(isascii HAVE_ISASCII)
CHECK_FUNCTION_EXISTS(memmove HAVE_MEMMOVE)
CHECK_FUNCTION_EXISTS(memset HAVE_MEMSET)
CHECK_FUNCTION_EXISTS(mmap HAVE_MMAP)
CHECK_FUNCTION_EXISTS(pow HAVE_POW)
CHECK_FUNCTION_EXISTS(sqrt HAVE_SQRT)
CHECK_FUNCTION_EXISTS(strcasecmp HAVE_STRCASECMP)
CHECK_FUNCTION_EXISTS(strchr HAVE_STRCHR)
CHECK_FUNCTION_EXISTS(strrchr HAVE_STRRCHR)
CHECK_FUNCTION_EXISTS(strstr HAVE_STRSTR)
CHECK_FUNCTION_EXISTS(strtol HAVE_STRTOL)
CHECK_FUNCTION_EXISTS(strtoul HAVE_STRTOUL)

include(CheckTypeSize)

CHECK_TYPE_SIZE("int" SIZEOF_INT)
CHECK_TYPE_SIZE("long" SIZEOF_LONG)

TEST_BIG_ENDIAN(WORDS_BIGENDIAN)

configure_file(${ITK3P_TIFF_SOURCE_DIR}/tiffDllConfig.h.in
  ${ITK3P_TIFF_BINARY_DIR}/itk_tiffDllConfig.h)
configure_file(${ITK3P_TIFF_SOURCE_DIR}/tif_config.h.in
  ${ITK3P_TIFF_BINARY_DIR}/itk_tif_config.h)

if(NOT UNIX)
  add_definitions( -DTIF_PLATFORM_CONSOLE )
  set(common_SRCS ${common_SRCS} tif_win32.c tif_fax3sm.c)
  if(BUILD_SHARED_LIBS)
    if(NOT BORLAND)
      if(NOT MINGW)
        set(common_SRCS ${common_SRCS} itktiff.def)
      endif(NOT MINGW)
    endif(NOT BORLAND)
  endif(BUILD_SHARED_LIBS)
else()
  set(TIFF_RIGHT_VERSION 1)

  set(common_SRCS ${common_SRCS} tif_unix.c tif_fax3sm.c)
  if(NOT CMAKE_CROSSCOMPILING)
    set_source_files_properties(tif_fax3sm.c GENERATED)
    add_executable(itkmkg3states mkg3states.c)
    set_property(TARGET itkmkg3states PROPERTY RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    add_custom_command(
      COMMAND itkmkg3states -c const ${ITK3P_TIFF_BINARY_DIR}/tif_fax3sm.c
      OUTPUT ${ITK3P_TIFF_BINARY_DIR}/tif_fax3sm.c
      DEPENDS itkmkg3states
      )
  endif()
endif()

add_library(itktiff ${common_SRCS})
target_link_libraries(itktiff ${ITKZLIB_LIBRARIES} ${ITKJPEG_LIBRARIES})

if(UNIX)
  target_link_libraries(itktiff -lm)
endif(UNIX)

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

if(CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-strict-aliasing")
endif(CMAKE_COMPILER_IS_GNUCXX)
