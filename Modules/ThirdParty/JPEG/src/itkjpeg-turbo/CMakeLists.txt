# ITK downstream fork of libjpeg-turbo's build system.
# Builds the core libjpeg API as the library itkjpeg.
# No SIMD, no TurboJPEG API, no tools.
# Three compiled precision tiers (BITS_IN_JSAMPLE=8/12/16) cover:
#   lossy DCT JPEG at 8 and 12 bits/sample;
#   lossless JPEG at 2-16 bits/sample (2-8 via 8-bit objects,
#   9-12 via 12-bit objects, 13-16 via 16-bit objects).

set(JPEG16_SOURCES src/jcapistd.c src/jccolor.c src/jcdiffct.c src/jclossls.c src/jcmainct.c
        src/jcprepct.c src/jcsample.c src/jdapistd.c src/jdcolor.c src/jddiffct.c src/jdlossls.c src/jdmainct.c
        src/jdpostct.c src/jdsample.c src/jutils.c)
set(JPEG12_SOURCES ${JPEG16_SOURCES} src/jccoefct.c src/jcdctmgr.c src/jdcoefct.c
        src/jddctmgr.c src/jdmerge.c src/jfdctfst.c src/jfdctint.c src/jidctflt.c src/jidctfst.c src/jidctint.c
        src/jidctred.c src/jquant1.c src/jquant2.c)
set(JPEG_SOURCES ${JPEG12_SOURCES} src/jcapimin.c src/jchuff.c src/jcicc.c src/jcinit.c
        src/jclhuff.c src/jcmarker.c src/jcmaster.c src/jcomapi.c src/jcparam.c src/jcphuff.c src/jctrans.c
        src/jdapimin.c src/jdatadst.c src/jdatasrc.c src/jdhuff.c src/jdicc.c src/jdinput.c src/jdlhuff.c
        src/jdmarker.c src/jdmaster.c src/jdphuff.c src/jdtrans.c src/jerror.c src/jfdctflt.c src/jmemmgr.c
        src/jmemnobs.c src/jpeg_nbits.c)

set(JPEG_SOURCES ${JPEG_SOURCES} src/jaricom.c src/jcarith.c src/jdarith.c)


include(CheckSymbolExists)
check_symbol_exists("memset" "string.h" HAVE_MEMSET)
check_symbol_exists("memcpy" "string.h" HAVE_MEMCPY)
if (NOT HAVE_MEMSET OR NOT HAVE_MEMCPY)
  set(NEED_BSD_STRINGS 1)
endif ()

include(CheckIncludeFile)
if(MSVC)
  check_include_file("intrin.h" HAVE_INTRIN_H)
endif()

set(BITS_IN_JSAMPLE 8)

set(JPEG_LIB_VERSION 80)
set(LIBJPEG_TURBO_VERSION "3.1.4")
set(LIBJPEG_TURBO_VERSION_NUMBER "003001004")
set(COPYRIGHT_YEAR "1991-2026")
set(MEM_SRCDST_SUPPORTED 1)
set(C_ARITH_CODING_SUPPORTED 1)
set(D_ARITH_CODING_SUPPORTED 1)
set(NEED_SYS_TYPES_H 1)
#set(WITH_SIMD 1)

set(THREAD_LOCAL "thread_local")


if(UNIX)
  if(CMAKE_CROSSCOMPILING)
    set(RIGHT_SHIFT_IS_UNSIGNED 0)
  else()
    include(CheckCSourceRuns)
    check_c_source_runs("
      #include <stdio.h>
      #include <stdlib.h>
      static int is_shifting_signed (long arg) {
        long res = arg >> 4;
        if (res == -0x7F7E80CL)
          return 1; /* right shift is signed */
        /* see if unsigned-shift hack will fix it. */
        /* we can't just test exact value since it depends on width of long... */
        res |= 0xFFFFFFFFL << (32-4);
        if (res == -0x7F7E80CL)
          return 0; /* right shift is unsigned */
        printf(\"Right shift isn't acting as I expect it to.\\\\n\");
        printf(\"I fear the JPEG software will not work at all.\\\\n\\\\n\");
        return 0; /* try it with unsigned anyway */
      }
      int main (void) {
        exit(is_shifting_signed(-0x7F7E80B1L));
      }" RIGHT_SHIFT_IS_UNSIGNED)
  endif()
endif()


if (BUILD_SHARED_LIBS)
  set(JPEG_SHARED_LIBS 1)
else ()
  set(JPEG_SHARED_LIBS 0)
endif ()

set(VERSION "${LIBJPEG_TURBO_VERSION}")
configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/src/jconfig.h.in"
        "${CMAKE_CURRENT_BINARY_DIR}/jconfig.h")

set(headers
        src/jerror.h
        src/jmorecfg.h
        src/jpeglib.h
        itk_jpeg_mangle.h
        "${CMAKE_CURRENT_BINARY_DIR}/jconfig.h")

include(CheckCSourceCompiles)
include(CheckTypeSize)
check_type_size("size_t" SIZE_T)

if(SIZE_T EQUAL UNSIGNED_LONG)
  check_c_source_compiles("int main(int argc, char **argv) { unsigned long a = argc;  return __builtin_ctzl(a); }"
          HAVE_BUILTIN_CTZL)
endif()

if(NOT WIN32)
  check_c_source_compiles(
    "extern const int table[1]; const int __attribute__((visibility(\"hidden\"))) table[1] = { 0 }; int main(void) { return table[0]; }"
    HIDDEN_WORKS)
  if(HIDDEN_WORKS)
    set(HIDDEN "__attribute__((visibility(\"hidden\")))") 
  endif()
endif()

set(SIZEOF_SIZE_T "${SIZE_T}")
set(PACKAGE_NAME "itkjpeg")
string(TIMESTAMP BUILD "%Y%m%d")

configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/src/jconfigint.h.in"
        "${CMAKE_CURRENT_BINARY_DIR}/jconfigint.h")
configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/src/jversion.h.in"
        "${CMAKE_CURRENT_BINARY_DIR}/jversion.h")

set(MANGLE_PREFIX "itk") # prefix for symbols mangling
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/itk_jpeg_mangle.h.in"
        "${CMAKE_CURRENT_BINARY_DIR}/itk_jpeg_mangle.h")


# Compile a separate version of these source files with 12-bit and 16-bit
# data precision.
add_library(itkjpeg12 OBJECT ${JPEG12_SOURCES})
set_property(TARGET itkjpeg12 PROPERTY COMPILE_FLAGS "-DBITS_IN_JSAMPLE=12")
target_include_directories(itkjpeg12
        PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>"
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>")
add_library(itkjpeg16 OBJECT ${JPEG16_SOURCES})
set_property(TARGET itkjpeg16 PROPERTY COMPILE_FLAGS "-DBITS_IN_JSAMPLE=16")
target_include_directories(itkjpeg16
        PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>"
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>")


add_library(itkjpeg ${JPEG_SOURCES} $<TARGET_OBJECTS:itkjpeg12> $<TARGET_OBJECTS:itkjpeg16>)
target_include_directories(itkjpeg
        PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>"
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>")


# Apply user-defined properties to the library target.
if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(itkjpeg PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  set_target_properties(itkjpeg12 PROPERTIES ${ITK_LIBRARY_PROPERTIES})
  set_target_properties(itkjpeg16 PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

install(TARGETS itkjpeg
        EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
        RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
        LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
        ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development
)
install(FILES ${headers}
        DESTINATION ${ITK3P_INSTALL_INCLUDE_DIR}/itkjpeg-turbo/src
        COMPONENT Development
)
