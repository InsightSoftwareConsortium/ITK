project(ITKDCMTK)

set(ITKDCMTK_THIRD_PARTY 1)

# DCMTK is built in-scope via FetchContent; this module dir has no sources.
set(ITKDCMTK_NO_SRC 1)

include(CMakeParseArguments)

set(
  _ITKDCMTK_LIB_NAMES
  dcmdata
  dcmpstat
  dcmsr
  dcmqrdb
  dcmimgle
  dcmimage
  dcmjpeg
  dcmjpls
  dcmnet
  dcmiod
  dcmfg
  dcmseg
  dcmpmap
  ijg12
  ijg16
  ijg8
  oflog
  ofstd
  oficonv
)
if(WIN32)
  set(
    ITKDCMTK_LIBDEP_WIN
    iphlpapi
    ws2_32
    netapi32
    wsock32
  )
endif()

#
# if DCMTK is built elsewhere
if(ITK_USE_SYSTEM_DCMTK)
  # assign includes to std module var
  set(ITKDCMTK_SYSTEM_INCLUDE_DIRS ${DCMTK_INCLUDE_DIRS})

  # Module standard library var
  set(ITKDCMTK_LIBRARIES)
  foreach(lib IN LISTS DCMTK_LIBRARIES)
    if(TARGET DCMTK::${lib})
      list(APPEND ITKDCMTK_LIBRARIES DCMTK::${lib})
    else()
      list(APPEND ITKDCMTK_LIBRARIES ${lib})
    endif()
  endforeach()

  # When this module is loaded by an app, load DCMTK too.
  set(
    ITKDCMTK_EXPORT_CODE_INSTALL
    "
set(DCMTK_DIR \"${DCMTK_DIR}\")
find_package(DCMTK REQUIRED NO_MODULE)
"
  )
  set(
    ITKDCMTK_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  set(CMAKE_MODULE_PATH
    \"${CMAKE_CURRENT_SOURCE_DIR}/CMake\" \${CMAKE_MODULE_PATH})
  set(DCMTK_DIR \"${DCMTK_DIR}\")
  find_package(DCMTK REQUIRED NO_MODULE)
endif()
"
  )

  itk_module_impl()
else(ITK_USE_SYSTEM_DCMTK)
  # FetchContent + add_subdirectory builds DCMTK in ITK's scope so it consumes
  # ITK's codec targets directly.
  include(FetchContent)
  include(${CMAKE_CURRENT_LIST_DIR}/DCMTKGitTag.cmake)

  # Pass ITK's codec libraries as the standard find_package variables; in-scope
  # they resolve to ITK's imported targets with their transitive usage requirements.
  set(DCMTK_USE_FIND_PACKAGE ON)
  set(DCMTK_WITH_TIFF ON)
  set(DCMTK_WITH_PNG ON)
  set(DCMTK_WITH_ZLIB ON)
  set(DCMTK_WITH_OPENSSL OFF)
  set(DCMTK_WITH_XML OFF)
  set(TIFF_LIBRARIES ITK::ITKTIFFModule)
  set(JPEG_LIBRARIES ITK::ITKJPEGModule)
  set(PNG_LIBRARIES ITK::ITKPNGModule)
  set(ZLIB_LIBRARIES ITK::ITKZLIBModule)

  # DCMTK build options; compiler and language-standard settings inherit from ITK's scope.
  set(BUILD_APPS OFF)
  # Per-library targets only: a single-library build renames targets with a
  # suffix the DCMTK IO modules' DCMTK::<lib> link names cannot match.
  set(BUILD_SINGLE_SHARED_LIBRARY OFF)
  set(DCMTK_ENABLE_CXX11 ON)
  set(DCMTK_FORCE_FPIC_ON_UNIX ON)
  set(DCMTK_DEFAULT_DICT builtin)
  set(DCMTK_WITH_DOXYGEN OFF)
  set(DCMTK_WITH_SNDFILE OFF)
  set(DCMTK_WITH_WRAP OFF)
  set(DCMTK_ENABLE_PRIVATE_TAGS ON)

  # DCMTK 3.7 has no ICU backend; pin built-in oficonv and skip external iconv probes.
  set(DCMTK_WITH_ICU OFF)
  set(DCMTK_WITH_ICONV OFF)
  set(DCMTK_WITH_STDLIBC_ICONV OFF)
  set(DCMTK_ENABLE_BUILTIN_OFICONV_DATA ON)
  set(DCMTK_ENABLE_CHARSET_CONVERSION "oficonv" CACHE INTERNAL "") # NOTE: DCMTK requires a cache entry

  set(DCMTK_OVERWRITE_WIN32_COMPILER_FLAGS OFF)

  # Silence DCMTK third-party warnings here because add_subdirectory() runs
  # before itk_module_impl() would disable them.
  itk_module_warnings_disable(C CXX)

  # No EXCLUDE_FROM_ALL: it leaks onto ITK's own test targets and drops them
  # from ALL. DCMTK's libraries are needed by ITKIODCMTK and it builds no apps
  # (BUILD_APPS OFF) or tests.
  FetchContent_Declare(
    dcmtk
    GIT_REPOSITORY ${DCMTK_GIT_REPOSITORY}
    GIT_TAG ${DCMTK_GIT_TAG}
  )

  FetchContent_MakeAvailable(dcmtk)

  # DCMTK:: IMPORTED INTERFACE wrappers (not ALIAS): IMPORTED preserves the
  # namespaced name in ITKTargets.cmake, matching what find_package(DCMTK)
  # yields for downstream consumers.
  foreach(_dcmtk_lib IN LISTS _ITKDCMTK_LIB_NAMES)
    if(TARGET "${_dcmtk_lib}")
      if(NOT TARGET "DCMTK::${_dcmtk_lib}")
        add_library("DCMTK::${_dcmtk_lib}" INTERFACE IMPORTED GLOBAL)
        target_link_libraries("DCMTK::${_dcmtk_lib}" INTERFACE "${_dcmtk_lib}")
      endif()
      list(APPEND ITKDCMTK_LIBRARIES "DCMTK::${_dcmtk_lib}")
    endif()
  endforeach()
  unset(_dcmtk_lib)

  # Build-tree consumers: locate DCMTKConfig.cmake in ITK's build root.
  set(
    ITKDCMTK_EXPORT_CODE_BUILD
    "
if(NOT ITK_BINARY_DIR)
  set(DCMTK_DIR \"${CMAKE_BINARY_DIR}\")
  find_package(DCMTK REQUIRED NO_MODULE)
endif()
"
  )
  # Install-tree consumers: locate DCMTKConfig.cmake under ITK_INSTALL_PREFIX.
  set(
    ITKDCMTK_EXPORT_CODE_INSTALL
    "
set(DCMTK_DIR \"\${ITK_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/cmake/dcmtk\")
find_package(DCMTK REQUIRED NO_MODULE)
"
  )

  itk_module_impl()
endif(ITK_USE_SYSTEM_DCMTK)

if(MSVC)
  # DCMTK's osconfig.h guards C++11/14/17 on __cplusplus, which MSVC underreports without this.
  target_compile_options(ITKDCMTKModule INTERFACE /Zc:__cplusplus)
endif()
