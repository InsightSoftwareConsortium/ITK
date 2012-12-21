option(ITK_USE_SYSTEM_MINC "Use system-installed MINC" OFF)
mark_as_advanced(ITK_USE_SYSTEM_MINC)

# Once ITK requires CMake >= 2.8.8 we can use
#  add_library(... IMPORTED GLOBAL)
# in the adjacent CMakeLists.txt file instead.
# For now define targets here to make them global.
if(NOT ITK_USE_SYSTEM_MINC)
#   if(BUILD_SHARED_LIBS)
#     set(_ITKMINC_LIB_LINKAGE SHARED)
#   else()
#     set(_ITKMINC_LIB_LINKAGE STATIC)
#   endif()
  set(_ITKMINC_LIB_LINKAGE STATIC)
  set(MINC_LIBRARY_NAME itk_minc2)
  # add it as an imported  library target
  add_library(${MINC_LIBRARY_NAME} ${_ITKMINC_LIB_LINKAGE} IMPORTED)
endif(NOT ITK_USE_SYSTEM_MINC)
