PROJECT(ITK3P_EXPAT)
INCLUDE_REGULAR_EXPRESSION("^(expat|xml|ascii|utf|name|iascii|latin|itk).*$")

SET(expat_SRCS
  xmlparse.c
  xmltok.c
  xmlrole.c
)

INCLUDE_DIRECTORIES(BEFORE ${ITK3P_EXPAT_SOURCE_DIR})
INCLUDE_DIRECTORIES(BEFORE ${ITK3P_EXPAT_BINARY_DIR})

IF(WIN32)
  IF(NOT BUILD_SHARED_LIBS)
    SET (ITK_EXPAT_STATIC 1)
  ENDIF(NOT BUILD_SHARED_LIBS)
ENDIF(WIN32)

#
# Configuration
#
option(EXPAT_BUILD_TOOLS "build the xmlwf tool for expat library" ${_EXPAT_BUILD_TOOLS_DEFAULT})
option(EXPAT_BUILD_EXAMPLES "build the examples for expat library" ON)
option(EXPAT_BUILD_TESTS "build the tests for expat library" ON)
option(EXPAT_SHARED_LIBS "build a shared expat library" ${_EXPAT_SHARED_LIBS_DEFAULT})
option(EXPAT_BUILD_DOCS "build man page for xmlwf" ${_EXPAT_BUILD_DOCS_DEFAULT})
option(EXPAT_BUILD_FUZZERS "build fuzzers for the expat library" OFF)
option(EXPAT_BUILD_PKGCONFIG "build pkg-config file" ${_EXPAT_BUILD_PKGCONFIG_DEFAULT})
option(EXPAT_OSSFUZZ_BUILD "build fuzzers via ossfuzz for the expat library" OFF)
if(UNIX OR _EXPAT_HELP)
    option(EXPAT_WITH_LIBBSD "utilize libbsd (for arc4random_buf)" OFF)
endif()
option(EXPAT_ENABLE_INSTALL "install expat files in cmake install target" ON)
set(EXPAT_CONTEXT_BYTES 1024 CACHE STRING "Define to specify how much context to retain around the current parse point")
mark_as_advanced(EXPAT_CONTEXT_BYTES)
option(EXPAT_DTD "Define to make parameter entity parsing functionality available" ON)
mark_as_advanced(EXPAT_DTD)
option(EXPAT_NS "Define to make XML Namespaces functionality available" ON)
mark_as_advanced(EXPAT_NS)
option(EXPAT_WARNINGS_AS_ERRORS "Treat all compiler warnings as errors" OFF)
if(UNIX OR _EXPAT_HELP)
    option(EXPAT_DEV_URANDOM "Define to include code reading entropy from `/dev/urandom'." ON)
    set(EXPAT_WITH_GETRANDOM "AUTO" CACHE STRING
            "Make use of getrandom function (ON|OFF|AUTO) [default=AUTO]")
    set(EXPAT_WITH_SYS_GETRANDOM "AUTO" CACHE STRING
            "Make use of syscall SYS_getrandom (ON|OFF|AUTO) [default=AUTO]")
    mark_as_advanced(EXPAT_DEV_URANDOM)
endif()
set(EXPAT_CHAR_TYPE "char" CACHE STRING "Character type to use (char|ushort|wchar_t) [default=char]")
option(EXPAT_ATTR_INFO "Define to allow retrieving the byte offsets for attribute names and values" OFF)
mark_as_advanced(EXPAT_ATTR_INFO)
option(EXPAT_LARGE_SIZE "Make XML_GetCurrent* functions return <(unsigned) long long> rather than <(unsigned) long>" OFF)
mark_as_advanced(EXPAT_LARGE_SIZE)
option(EXPAT_MIN_SIZE "Get a smaller (but slower) parser (in particular avoid multiple copies of the tokenizer)" OFF)
mark_as_advanced(EXPAT_MIN_SIZE)
if(MSVC OR _EXPAT_HELP)
    set(EXPAT_MSVC_STATIC_CRT ${ITK_MSVC_STATIC_RUNTIME_LIBRARY} CACHE INTERNAL "Use /MT flag (static CRT) when compiling in MSVC")
endif()

#
# Environment checks
#
if(EXPAT_WITH_LIBBSD)
    find_library(LIB_BSD NAMES bsd)
    if(NOT LIB_BSD)
        message(SEND_ERROR "EXPAT_WITH_LIBBSD option is enabled, but libbsd was not found")
    else()
        set(HAVE_LIBBSD TRUE)
    endif()
endif()

if(MSVC)
    # For the three types of MSVC version values, please see:
    # - https://cmake.org/cmake/help/latest/variable/MSVC_VERSION.html
    # - https://sourceforge.net/p/predef/wiki/Compilers/
    # - https://en.wikipedia.org/wiki/Microsoft_Visual_Studio#History
    set(_EXPAT_MSVC_REQUIRED_INT 1800)  # i.e. 12.0/2013/1800; see PR #426
    set(_EXPAT_MSVC_SUPPORTED_INT 1910)
    set(_EXPAT_MSVC_SUPPORTED_DISPLAY "Visual Studio 15.0/2017/${_EXPAT_MSVC_SUPPORTED_INT}")

    if(MSVC_VERSION VERSION_LESS ${_EXPAT_MSVC_SUPPORTED_INT})
        if(MSVC_VERSION VERSION_LESS ${_EXPAT_MSVC_REQUIRED_INT})
            message(SEND_ERROR "MSVC_VERSION ${MSVC_VERSION} is TOO OLD to compile Expat without errors.")
            message(SEND_ERROR "Please use officially supported ${_EXPAT_MSVC_SUPPORTED_DISPLAY} or later.  Thank you!")
        else()
            message(WARNING "MSVC_VERSION ${MSVC_VERSION} is NOT OFFICIALLY SUPPORTED by Expat.")
            message(WARNING "Please use ${_EXPAT_MSVC_SUPPORTED_DISPLAY} or later.  Thank you!")
        endif()
    endif()
endif()

macro(_expat_copy_bool_int source_ref dest_ref)
    if(${source_ref})
        set(${dest_ref} 1)
    else()
        set(${dest_ref} 0)
    endif()
endmacro()

if(EXPAT_LARGE_SIZE)
    add_definitions(-DXML_LARGE_SIZE)
endif()

if(EXPAT_MIN_SIZE)
    add_definitions(-DXML_MIN_SIZE)
endif()

if(EXPAT_CHAR_TYPE STREQUAL "char")
    set(_EXPAT_UNICODE OFF)
    set(_EXPAT_UNICODE_WCHAR_T OFF)
elseif(EXPAT_CHAR_TYPE STREQUAL "ushort")
    set(_EXPAT_UNICODE ON)
    set(_EXPAT_UNICODE_WCHAR_T OFF)
    if(EXPAT_BUILD_EXAMPLES)
        message(SEND_ERROR "Examples can not be built with option -DEXPAT_CHAR_TYPE=ushort. Please pass -DEXPAT_CHAR_TYPE=(char|wchar_t) or -DEXPAT_BUILD_EXAMPLES=OFF.")
    endif()
    if(EXPAT_BUILD_TESTS)
        message(SEND_ERROR "The testsuite can not be built with option -DEXPAT_CHAR_TYPE=ushort. Please pass -DEXPAT_CHAR_TYPE=(char|wchar_t) or -DEXPAT_BUILD_TESTS=OFF.")
    endif()
    if(EXPAT_BUILD_TOOLS)
        message(SEND_ERROR "The xmlwf tool can not be built with option -DEXPAT_CHAR_TYPE=ushort. Please pass -DEXPAT_CHAR_TYPE=(char|wchar_t) or -DEXPAT_BUILD_TOOLS=OFF.")
    endif()
elseif(EXPAT_CHAR_TYPE STREQUAL "wchar_t")
    set(_EXPAT_UNICODE ON)
    set(_EXPAT_UNICODE_WCHAR_T ON)
    if(NOT WIN32)
        string(FIND "${CMAKE_C_FLAGS}" "-fshort-wchar" _expat_short_wchar_found)
        if(${_expat_short_wchar_found} EQUAL "-1")
            message(SEND_ERROR "Configuration -DEXPAT_CHAR_TYPE=wchar_t requires -DCMAKE_{C,CXX}_FLAGS=-fshort-wchar (which was not found) and libc compiled with -fshort-wchar, too.")
        endif()
        if (EXPAT_BUILD_TOOLS)
            message(SEND_ERROR "The xmlwf tool can not be built with option -DEXPAT_CHAR_TYPE=wchar_t outside of Windows. Please pass -DEXPAT_CHAR_TYPE=char or -DEXPAT_BUILD_TOOLS=OFF.")
        endif()
    endif()
else()
    message(SEND_ERROR "Option -DEXPAT_CHAR_TYPE=(char|ushort|wchar_t) cannot be \"${EXPAT_CHAR_TYPE}\".")
endif()

if(_EXPAT_UNICODE)
    add_definitions(-DXML_UNICODE)              # for unsigned short
    if(_EXPAT_UNICODE_WCHAR_T)
        add_definitions(-DXML_UNICODE_WCHAR_T)  # for wchar_t
    endif()
endif()

include(${CMAKE_CURRENT_LIST_DIR}/ConfigureChecks.cmake)

macro(evaluate_detection_results use_ref have_ref thing_lower thing_title)
    if(${use_ref} AND NOT (${use_ref} STREQUAL "AUTO") AND NOT ${have_ref})
        message(SEND_ERROR
                "Use of ${thing_lower} was enforced by ${use_ref}=ON but it could not be found.")
    elseif(NOT ${use_ref} AND ${have_ref})
        message("${thing_title} was found but it will not be used due to ${use_ref}=OFF.")
        set(${have_ref} 0)
    endif()
endmacro()

if(NOT WIN32)
    evaluate_detection_results(EXPAT_WITH_GETRANDOM HAVE_GETRANDOM "function getrandom" "Function getrandom")
    evaluate_detection_results(EXPAT_WITH_SYS_GETRANDOM HAVE_SYSCALL_GETRANDOM "syscall SYS_getrandom" "Syscall SYS_getrandom")
endif()

_expat_copy_bool_int(EXPAT_ATTR_INFO        XML_ATTR_INFO)
_expat_copy_bool_int(EXPAT_DTD              XML_DTD)
_expat_copy_bool_int(EXPAT_LARGE_SIZE       XML_LARGE_SIZE)
_expat_copy_bool_int(EXPAT_MIN_SIZE         XML_MIN_SIZE)
_expat_copy_bool_int(EXPAT_NS               XML_NS)
if(NOT WIN32)
    _expat_copy_bool_int(EXPAT_DEV_URANDOM  XML_DEV_URANDOM)
endif()
set(XML_CONTEXT_BYTES ${EXPAT_CONTEXT_BYTES})


CONFIGURE_FILE(${ITK3P_EXPAT_SOURCE_DIR}/expatConfig.h.in
               ${ITK3P_EXPAT_BINARY_DIR}/expatConfig.h)
CONFIGURE_FILE(${ITK3P_EXPAT_SOURCE_DIR}/expatDllConfig.h.in
               ${ITK3P_EXPAT_BINARY_DIR}/expatDllConfig.h)

configure_file(expat_config.h.cmake "${ITK3P_EXPAT_BINARY_DIR}/expat_config.h")

ADD_LIBRARY(ITKEXPAT ${expat_SRCS})

# Apply user-defined properties to the library target.
IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKEXPAT PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

INSTALL(TARGETS ITKEXPAT
  EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
  RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
  LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
  ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development
  )

INSTALL(FILES
  ${ITK3P_EXPAT_BINARY_DIR}/expatDllConfig.h
  ${ITK3P_EXPAT_SOURCE_DIR}/expat.h
  ${ITK3P_EXPAT_BINARY_DIR}/expat_config.h
  ${ITK3P_EXPAT_SOURCE_DIR}/expat_external.h
  ${ITK3P_EXPAT_SOURCE_DIR}/itk_expat_mangle.h
  DESTINATION ${ITK3P_INSTALL_INCLUDE_DIR} # TODO: itk_expat.h #include "itkexpat/expat.h"
  COMPONENT Development)
