set(JPEG16_SOURCES jcapistd.c jccolor.c jcdiffct.c jclossls.c jcmainct.c
        jcprepct.c jcsample.c jdapistd.c jdcolor.c jddiffct.c jdlossls.c jdmainct.c
        jdpostct.c jdsample.c jutils.c)
set(JPEG12_SOURCES ${JPEG16_SOURCES} jccoefct.c jcdctmgr.c jdcoefct.c
        jddctmgr.c jdmerge.c jfdctfst.c jfdctint.c jidctflt.c jidctfst.c jidctint.c
        jidctred.c jquant1.c jquant2.c)
set(JPEG_SOURCES ${JPEG12_SOURCES} jcapimin.c jchuff.c jcicc.c jcinit.c
        jclhuff.c jcmarker.c jcmaster.c jcomapi.c jcparam.c jcphuff.c jctrans.c
        jdapimin.c jdatadst.c jdatasrc.c jdhuff.c jdicc.c jdinput.c jdlhuff.c
        jdmarker.c jdmaster.c jdphuff.c jdtrans.c jerror.c jfdctflt.c jmemmgr.c
        jmemnobs.c jpeg_nbits.c)

set(JPEG_SOURCES ${JPEG_SOURCES} jaricom.c jcarith.c jdarith.c)


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
set(LIBJPEG_TURBO_VERSION "3.0.4")
set(LIBJPEG_TURBO_VERSION_NUMBER "003000004")
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

configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}${header_input_dir}/jconfig.h.in"
        "${CMAKE_CURRENT_BINARY_DIR}/jconfig.h")

set(headers
        jerror.h
        jmorecfg.h
        jpeglib.h
        "${CMAKE_CURRENT_BINARY_DIR}/jconfig.h")

include(CheckTypeSize)
check_type_size("size_t" SIZE_T)

if(SIZE_T EQUAL UNSIGNED_LONG)
  check_c_source_compiles("int main(int argc, char **argv) { unsigned long a = argc;  return __builtin_ctzl(a); }"
          HAVE_BUILTIN_CTZL)
endif()

set(SIZEOF_SIZE_T "${SIZE_T}")
set(VERSION "${LIBJPEG_TURBO_VERSION}")
set(PACKAGE_NAME "itkjpeg")
string(TIMESTAMP BUILD "%Y%m%d")

configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/jconfigint.h.in"
        "${CMAKE_CURRENT_BINARY_DIR}/jconfigint.h")
configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/jversion.h.in"
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
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>")
add_library(itkjpeg16 OBJECT ${JPEG16_SOURCES})
set_property(TARGET itkjpeg16 PROPERTY COMPILE_FLAGS "-DBITS_IN_JSAMPLE=16")
target_include_directories(itkjpeg16
        PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>")


add_library(itkjpeg ${JPEG_SOURCES} $<TARGET_OBJECTS:itkjpeg12> $<TARGET_OBJECTS:itkjpeg16>)
target_include_directories(itkjpeg
        PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>")


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