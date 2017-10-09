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

# Once ITK requires CMake >= 2.8.8 we can use
#  add_library(... IMPORTED GLOBAL)
# in the adjacent CMakeLists.txt file instead.
# For now define targets here to make them global.
if(ITK_USE_SYSTEM_DCMTK)
  # Use local FindDCMTK.cmake.
  list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}/CMake")
  find_package(DCMTK REQUIRED NO_MODULE)
else(ITK_USE_SYSTEM_DCMTK)
  set(_ITKDCMTK_LIB_NAMES dcmdata dcmimage dcmimgle dcmjpeg dcmjpls
    dcmnet dcmpstat dcmqrdb dcmsr dcmtls ijg12 ijg16 ijg8 oflog ofstd)
  if(BUILD_SHARED_LIBS)
    set(_ITKDCMTK_LIB_LINKAGE SHARED)
  else()
    set(_ITKDCMTK_LIB_LINKAGE STATIC)
  endif()
  foreach(lib ${_ITKDCMTK_LIB_NAMES})
    # add it as an imported  library target
    add_library(${lib} ${_ITKDCMTK_LIB_LINKAGE} IMPORTED)
  endforeach()
  # Copied and mofified from DCMTK/CMake/3rdparty.cmake
  if(NOT DEFINED DCMTK_USE_ICU)
    include(CheckCXXSourceCompiles)
    check_cxx_source_compiles("#include <iconv.h>\nint main(){iconv_t cd = iconv_open(\"\",\"\");iconv(cd,0,0,0,0);iconv_close(cd);return 0;}" WITH_STDLIBC_ICONV)
    if(WITH_STDLIBC_ICONV)
      message(STATUS "Info: found builtin ICONV support inside the C standard library.")
      set(ICU_ARGS
        -DDCMTK_WITH_STDLIBC_ICONV:BOOL=ON
        -DDCMTK_WITH_ICU:BOOL=OFF
        "-DDCMTK_ENABLE_CHARSET_CONVERSION:STRING=stdlibc (iconv)"
        CACHE INTERNAL "ICU Internal arguments"
        )
      set(_DCMTK_USE_ICU_default OFF)
    else()
      set(_DCMTK_USE_ICU_default ON)
    endif()
    option(DCMTK_USE_ICU "Downloads and compile ICU for DCMTK" ${_DCMTK_USE_ICU_default})
  endif()
  if(DCMTK_USE_ICU)
    if(ITK_USE_SYSTEM_ICU)
      find_package(ICU COMPONENTS uc data)
      set(ITKDCMTK_ICU_LIBRARIES ${ICU_LIBRARIES})
    else()
      set(ITKDCMTK_PREREQS ${ITKDCMTK_BINARY_DIR}/DCMTK_Prereqs)
      set(ITKDCMTK_ICU_LIBRARIES )
      foreach(lib_name uc data)
        set(lib ICU::${lib_name})
        list(APPEND ITKDCMTK_ICU_LIBRARIES ${lib})
        add_library(${lib} STATIC IMPORTED)
        set(lib_path ${ITKDCMTK_PREREQS}/lib/${lib_prefix}icu${lib_name}${lib_suffix})
        set_property(TARGET ${lib} PROPERTY
          IMPORTED_LOCATION ${lib_path})
        list(APPEND ICU_BYPRODUCTS "${lib_path}")
      endforeach()
    endif()
  endif()
endif(ITK_USE_SYSTEM_DCMTK)
