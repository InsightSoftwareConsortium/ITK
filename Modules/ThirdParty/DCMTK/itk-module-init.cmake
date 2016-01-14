## Only present and build DCMTK options if ITKIODCMTK is requested

option(ITK_USE_SYSTEM_DCMTK "Use an outside build of DCMTK." OFF)
# iconv library seems to be present unpredictably, never
# on linux, sometimes on OS X and probably never on Windows
option(DCMTK_USE_LIBICONV "Use IConv Library in DCMTK" OFF)

# Once ITK requires CMake >= 2.8.8 we can use
#  add_library(... IMPORTED GLOBAL)
# in the adjacent CMakeLists.txt file instead.
# For now define targets here to make them global.
if(ITK_USE_SYSTEM_DCMTK)
  # Use local FindDCMTK.cmake.
  list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}/CMake")
  find_package(DCMTK REQUIRED NO_MODULE)
else(ITK_USE_SYSTEM_DCMTK)
  if(MSVC)
    message(FATAL_ERROR "The ITKDCMTK module requires ITK_USE_SYSTEM_DCMTK to be ON for MSVC.")
  endif()
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
endif(ITK_USE_SYSTEM_DCMTK)
