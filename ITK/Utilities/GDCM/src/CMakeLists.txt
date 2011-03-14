set(GDCM_TARGETS_NAME ${ITK-GDCM-targets})
set(GDCM_BUILD_TESTING_DEFAULT OFF)
set(GDCM_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS} CACHE BOOL "" FORCE)

# Tell GDCM to use "system" libraries but give it ITK's libraries:
#
# Expat:
set(GDCM_USE_SYSTEM_EXPAT ON CACHE INTERNAL "")
set(EXPAT_INCLUDE_DIR ${ITK-Expat_INCLUDE_DIRS})
set(EXPAT_LIBRARY ${ITK-Expat_LIBRARIES})
# OpenJPEG:
set(GDCM_USE_SYSTEM_OPENJPEG ON CACHE INTERNAL "")
set(OPENJPEG_MAJOR_VERSION 2)
set(OPENJPEG_LIBRARY ${ITK-OpenJPEG_LIBRARIES})
set(OPENJPEG_LIBRARIES ${OPENJPEG_LIBRARY})
set(OPENJPEG_INCLUDE_DIR ${ITK-OpenJPEG_INCLUDE_DIRS})
# ZLIB:
set(GDCM_USE_SYSTEM_ZLIB ON CACHE INTERNAL "")
if(NOT ITK_USE_SYSTEM_ZLIB)
  set(ZLIB_INCLUDE_DIR ${ITK-ZLIB_SOURCE_DIR}/src/itkzlib)
  set(ZLIB_LIBRARY itkzlib)
endif()

add_subdirectory(gdcm)

foreach(lib
    gdcmCommon
    gdcmDICT
    gdcmDSED
    gdcmIOD
    gdcmMSFF
    gdcmcharls
    gdcmjpeg12
    gdcmjpeg16
    gdcmjpeg8
    gdcmuuid
    )
  if(TARGET ${lib}) # some gdcm targets are conditional
    #itk_module_target_name(${lib}) # TODO: Mangle lib names with itk prefix too
    itk_module_target_export(${lib})
  endif()
endforeach()
