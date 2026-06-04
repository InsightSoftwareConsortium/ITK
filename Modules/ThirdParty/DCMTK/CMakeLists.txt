project(ITKDCMTK)

set(ITKDCMTK_THIRD_PARTY 1)

# This depends on the external project, nothing locally built
set(ITKDCMTK_NO_SRC 1)

include(CMakeParseArguments)

if(DCMTK_USE_ICU)
  option(ITK_USE_SYSTEM_ICU "Use an installed version of ICU" OFF)
  if(NOT ITK_USE_SYSTEM_ICU)
    set(ITKDCMTK_PREREQS ${ITKDCMTK_BINARY_DIR}/DCMTK_Prereqs)
    set(ITKDCMTK_ICU_LIBRARIES)
    if(WIN32)
      message(
        FATAL_ERROR
        "ICU is not built as part of ITK on Windows. Please compile it outside of ITK."
      )
    else()
      set(
        ITKDCMTK_ICU_LIBRARY_NAMES
        uc
        data
      )
    endif()
    foreach(lib_name ${ITKDCMTK_ICU_LIBRARY_NAMES})
      set(lib ICU::${lib_name})
      list(APPEND ITKDCMTK_ICU_LIBRARIES ${lib})
      add_library(${lib} STATIC IMPORTED GLOBAL)
      if(CMAKE_CONFIGURATION_TYPES)
        foreach(c ${CMAKE_CONFIGURATION_TYPES})
          string(TOUPPER "${c}" C)
          set(debug_suffix)
          if("${C}" STREQUAL "DEBUG")
            set(
              lib_path
              ${ITKDCMTK_PREREQS}/${c}/lib/${lib_prefix}icu${lib_name}d${CMAKE_STATIC_LIBRARY_SUFFIX}
            )
          else() # For all non-debug, use release
            set(
              lib_path
              ${ITKDCMTK_PREREQS}/Release/lib/${lib_prefix}icu${lib_name}${CMAKE_STATIC_LIBRARY_SUFFIX}
            )
          endif()
          set_property(
            TARGET
              ${lib}
            PROPERTY
              IMPORTED_LOCATION_${C}
                ${lib_path}
          )
        endforeach()
      else()
        set(
          lib_path
          ${ITKDCMTK_PREREQS}/lib/${lib_prefix}icu${lib_name}${CMAKE_STATIC_LIBRARY_SUFFIX}
        )
        set_property(
          TARGET
            ${lib}
          PROPERTY
            IMPORTED_LOCATION
              ${lib_path}
        )
      endif()
      list(APPEND ICU_BYPRODUCTS "${lib_path}")
    endforeach()

    set(
      ICU_URL
      "http://download.icu-project.org/files/icu4c/58.2/icu4c-58_2-src.tgz"
    )
    set(ICU_MD5 fac212b32b7ec7ab007a12dff1f3aea1)
    # If not Debug, use Release (for Release, MinSizeRel,...)
    if(CMAKE_CONFIGURATION_TYPES)
      set(
        ICU_ROOT_DIR
        ${ITKDCMTK_PREREQS}/$<$<CONFIG:Debug>:Debug>$<$<NOT:$<CONFIG:Debug>>:Release>
      )
    else()
      set(ICU_ROOT_DIR ${ITKDCMTK_PREREQS})
    endif()
    set(
      CHARSET_CONVERSION_ARGS
      -DDCMTK_WITH_STDLIBC_ICONV:BOOL=OFF
      -DDCMTK_WITH_ICU:BOOL=ON
      -DDCMTK_ENABLE_CHARSET_CONVERSION:STRING=ICU
      -DICU_ROOT:PATH=${ICU_ROOT_DIR}
    )
    itk_download_attempt_check(icu)
    ExternalProject_Add(
      icu
      URL
        ${ICU_URL}
      URL_MD5 ${ICU_MD5}
      PREFIX ${ITKDCMTK_BINARY_DIR}
      UPDATE_COMMAND
        ""
      BUILD_BYPRODUCTS
        ${ICU_BYPRODUCTS}
      CONFIGURE_COMMAND
        <SOURCE_DIR>/source/configure --prefix=${ITKDCMTK_PREREQS}
        --with-data-packaging=static --enable-static=yes --enable-shared=no
        "CC=${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}"
        "CXX=${CMAKE_CXX_COMPILER_LAUNCHER} ${CMAKE_CXX_COMPILER}"
        "CFLAGS=-fPIC" "CXXFLAGS=-fPIC"
    )
    set(ICU_DEPENDENCY icu)
  endif()
else()
  set(CHARSET_CONVERSION_ARGS -DDCMTK_WITH_ICU:BOOL=OFF)
endif()

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
  # Build DCMTK in ITK's own CMake scope (FetchContent + add_subdirectory)
  # rather than a separate ExternalProject, so DCMTK consumes ITK's codec
  # *targets* directly (the GDCM model) with no $<TARGET_FILE:> marshalling
  # or :::-separated CMAKE_ARGS lists.
  include(FetchContent)
  include(${CMAKE_CURRENT_LIST_DIR}/DCMTKGitTag.cmake)

  # The ITK<dep>_LIBRARIES/_INCLUDE_DIRS variables are only populated by
  # itk_module_use(); call it up front so the codec variables exist before we
  # configure DCMTK. (Idempotent: itk_module_impl()'s later call is guarded by
  # <mod>_USED.)
  itk_module_use(${ITK_MODULE_${itk-module}_DEPENDS})

  # Hand DCMTK ITK's codec libraries as the standard variables. Because DCMTK
  # is now in-scope these are the live ITK imported targets, whose include dirs
  # and transitive dependencies propagate through normal target linking.
  set(DCMTK_USE_FIND_PACKAGE ON CACHE INTERNAL "")
  set(DCMTK_WITH_TIFF ON CACHE INTERNAL "")
  set(DCMTK_WITH_PNG ON CACHE INTERNAL "")
  set(DCMTK_WITH_ZLIB ON CACHE INTERNAL "")
  set(DCMTK_WITH_OPENSSL OFF CACHE INTERNAL "")
  set(DCMTK_WITH_XML OFF CACHE INTERNAL "")
  set(TIFF_LIBRARIES ${ITKTIFF_LIBRARIES})
  set(TIFF_INCLUDE_DIRS ${ITKTIFF_INCLUDE_DIRS})
  set(JPEG_LIBRARIES ${ITKJPEG_LIBRARIES})
  set(JPEG_INCLUDE_DIRS ${ITKJPEG_INCLUDE_DIRS})
  set(PNG_LIBRARIES ${ITKPNG_LIBRARIES})
  set(PNG_INCLUDE_DIRS ${ITKPNG_INCLUDE_DIRS})
  set(ZLIB_LIBRARIES ${ITKZLIB_LIBRARIES})
  set(ZLIB_INCLUDE_DIRS ${ITKZLIB_INCLUDE_DIRS})

  # DCMTK build options formerly passed via the ExternalProject CMAKE_ARGS.
  # CMAKE_CXX_STANDARD and the compiler/runtime settings are inherited directly
  # from ITK's scope.
  set(BUILD_APPS OFF CACHE INTERNAL "")
  set(DCMTK_ENABLE_CXX11 ON CACHE INTERNAL "")
  set(DCMTK_FORCE_FPIC_ON_UNIX ON CACHE INTERNAL "")
  set(DCMTK_DEFAULT_DICT builtin CACHE INTERNAL "")
  set(DCMTK_WITH_DOXYGEN OFF CACHE INTERNAL "")
  set(DCMTK_WITH_SNDFILE OFF CACHE INTERNAL "")
  set(DCMTK_WITH_WRAP OFF CACHE INTERNAL "")
  set(DCMTK_ENABLE_PRIVATE_TAGS ON CACHE INTERNAL "")

  # Charset conversion: honor the DCMTK_USE_ICU opt-in. The DCMTK_USE_ICU block
  # at the top of this file builds ICU and sets ICU_ROOT_DIR; wire those through
  # to the in-scope DCMTK build instead of unconditionally forcing oficonv.
  # Otherwise use DCMTK's built-in oficonv data, which needs no dependency.
  if(DCMTK_USE_ICU)
    set(DCMTK_WITH_ICU ON CACHE INTERNAL "")
    set(DCMTK_WITH_STDLIBC_ICONV OFF CACHE INTERNAL "")
    set(DCMTK_ENABLE_CHARSET_CONVERSION "ICU" CACHE INTERNAL "")
    set(ICU_ROOT "${ICU_ROOT_DIR}" CACHE INTERNAL "")
  else()
    set(DCMTK_WITH_ICU OFF CACHE INTERNAL "")
    set(DCMTK_ENABLE_BUILTIN_OFICONV_DATA ON CACHE INTERNAL "")
    set(DCMTK_ENABLE_CHARSET_CONVERSION "oficonv" CACHE INTERNAL "")
  endif()

  if(MSVC)
    set(DCMTK_OVERWRITE_WIN32_COMPILER_FLAGS OFF CACHE INTERNAL "")
    # DCMTK's STL feature probes read __cplusplus, which MSVC underreports
    # until /Zc:__cplusplus is set. This set() is directory-scoped, so it
    # reaches the DCMTK subdirectory without leaking to sibling ITK modules.
    string(APPEND CMAKE_CXX_FLAGS " /Zc:__cplusplus")
  endif()

  # FetchContent_MakeAvailable() (CMake 3.14+, below ITK's 3.22.1 floor) does
  # the add_subdirectory and avoids the FetchContent_Populate(<name>) form that
  # was deprecated in CMake 3.30. EXCLUDE_FROM_ALL is intentionally NOT used:
  # on the in-scope DCMTK directory it leaked into ITK's own test targets and
  # dropped them from the ALL target. DCMTK's libraries are needed by
  # ITKIODCMTK so they belong in ALL anyway, and DCMTK builds no apps
  # (BUILD_APPS OFF) or tests, so nothing extraneous is added.
  FetchContent_Declare(
    dcmtk
    GIT_REPOSITORY ${DCMTK_GIT_REPOSITORY}
    GIT_TAG ${DCMTK_GIT_TAG}
  )
  FetchContent_MakeAvailable(dcmtk)

  # Expose DCMTK's now-in-scope targets to the rest of ITK.
  set(ITKDCMTK_LIBRARIES ${_ITKDCMTK_LIB_NAMES})
  foreach(lib ${_ITKDCMTK_LIB_NAMES})
    list(APPEND ITKDCMTK_INCLUDE_DIRS ${dcmtk_SOURCE_DIR}/${lib}/include)
  endforeach()
  list(APPEND ITKDCMTK_INCLUDE_DIRS ${dcmtk_BINARY_DIR}/config/include)

  # Register DCMTK's targets with ITK's module system. itk_module_target()
  # applies ITK's output naming, creates the namespaced ITK::<lib> aliases that
  # itk_module_impl() links ITKDCMTKModule against, and appends them to ITK's
  # build-tree targets file. Installation is handled separately, so NO_INSTALL.
  foreach(lib ${_ITKDCMTK_LIB_NAMES})
    if(TARGET ${lib})
      itk_module_target(${lib} NO_INSTALL)
    endif()
  endforeach()

  itk_module_impl()
endif(ITK_USE_SYSTEM_DCMTK)

if(MSVC)
  # DCMTK's osconfig.h guards C++11/14/17 on __cplusplus, which MSVC underreports without this.
  target_compile_options(ITKDCMTKModule INTERFACE /Zc:__cplusplus)
endif()
