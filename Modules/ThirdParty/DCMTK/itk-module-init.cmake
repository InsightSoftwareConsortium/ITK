## Only present and build DCMTK options if ITKIODCMTK is requested

option(ITK_USE_SYSTEM_DCMTK "Use an outside build of DCMTK." OFF)
if(NOT WIN32)
  set(lib_prefix lib)
  if(BUILD_SHARED_LIBS)
    set(lib_suffix "${CMAKE_SHARED_LIBRARY_SUFFIX}")
    set(lib_prefix "${CMAKE_SHARED_LIBRARY_PREFIX}")
  else()
    set(lib_suffix "${CMAKE_STATIC_LIBRARY_SUFFIX}")
    set(lib_prefix "${CMAKE_STATIC_LIBRARY_PREFIX}")
  endif()
else()
  set(lib_prefix "")
  if(BUILD_SHARED_LIBS)
    set(lib_suffix "${CMAKE_IMPORT_LIBRARY_SUFFIX}")
    set(lib_prefix "${CMAKE_IMPORT_LIBRARY_PREFIX}")
  else()
    set(lib_suffix "${CMAKE_STATIC_LIBRARY_SUFFIX}")
    set(lib_prefix "${CMAKE_IMPORT_LIBRARY_PREFIX}")
  endif()
endif()

if(ITK_USE_SYSTEM_DCMTK)
  # Use local FindDCMTK.cmake.
  list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}/CMake")
  find_package(DCMTK REQUIRED NO_MODULE)
else(ITK_USE_SYSTEM_DCMTK)
  # Copied and mofified from DCMTK/CMake/3rdparty.cmake
  if(NOT DEFINED DCMTK_USE_ICU)
    include(CheckCXXSourceCompiles)
    check_cxx_source_compiles("#include <iconv.h>\nint main(){iconv_t cd = iconv_open(\"\",\"\");iconv(cd,0,0,0,0);iconv_close(cd);return 0;}" WITH_STDLIBC_ICONV)
    if(WITH_STDLIBC_ICONV)
      message(STATUS "Info: found builtin ICONV support inside the C standard library.")
      set(CHARSET_CONVERSION_ARGS
        -DDCMTK_WITH_STDLIBC_ICONV:BOOL=ON
        -DDCMTK_WITH_ICU:BOOL=OFF
        "-DDCMTK_ENABLE_CHARSET_CONVERSION:STRING=stdlibc (iconv)"
        CACHE INTERNAL "DCMTK Internal arguments"
        )
    endif()
    # ICU creates problems on MacOS and Windows, so by default it is disabled.
    # On Linux, the C standard library can have builtin ICONV support. We
    # disable building ICU by default.
    option(DCMTK_USE_ICU "Downloads and compile ICU for DCMTK" OFF)
  endif()
  if(DCMTK_USE_ICU)
    if(ITK_USE_SYSTEM_ICU)
      find_package(ICU REQUIRED COMPONENTS uc data)
      set(ITKDCMTK_ICU_LIBRARIES ${ICU_LIBRARIES})
    endif()
  endif()
endif(ITK_USE_SYSTEM_DCMTK)
