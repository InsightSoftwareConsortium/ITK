if(CMAKE_COMPILER_IS_GNUCC)
  ITK_CHECK_C_COMPILER_FLAG("-fno-ipa-cp-clone" _HAS_fno-ipa-cp-clone)
  if(_HAS_fno-ipa-cp-clone)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-ipa-cp-clone")
  endif()
endif()
if(CMAKE_COMPILER_IS_GNUCXX)
  ITK_CHECK_CXX_COMPILER_FLAG("-fno-ipa-cp-clone" _HAS_fno-ipa-cp-clone)
  if(_HAS_fno-ipa-cp-clone)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-ipa-cp-clone")
  endif()
endif()


set(GDCM_TARGETS_NAME ${ITKGDCM-targets})
set(GDCM_INSTALL_BIN_DIR ${ITKGDCM_INSTALL_RUNTIME_DIR})
set(GDCM_INSTALL_LIB_DIR ${ITKGDCM_INSTALL_LIBRARY_DIR})
set(GDCM_INSTALL_INCLUDE_DIR ${ITKGDCM_INSTALL_INCLUDE_DIR})
set(GDCM_INSTALL_DATA_DIR ${ITK_INSTALL_DATA_DIR}/gdcm)
set(GDCM_BUILD_TESTING_DEFAULT OFF)

# Tell GDCM to use "system" libraries but give it ITK's libraries:
#
# Expat:
set(GDCM_USE_SYSTEM_EXPAT ON CACHE INTERNAL "")
set(EXPAT_INCLUDE_DIR ${ITKExpat_INCLUDE_DIRS})
set(EXPAT_LIBRARY ${ITKExpat_LIBRARIES})
# OpenJPEG:
set(GDCM_USE_SYSTEM_OPENJPEG OFF CACHE INTERNAL "")
set(GDCM_USE_OPENJPEG_V2 ON CACHE INTERNAL "")
# ZLIB:
set(GDCM_USE_SYSTEM_ZLIB ON CACHE INTERNAL "")
set(ZLIB_INCLUDE_DIR ${ITKZLIB_INCLUDE_DIRS})
set(ZLIB_LIBRARY ${ITKZLIB_LIBRARIES})

# Configure GDCM privately so its options do not appear to the user.
set(GDCM_LEGACY_REMOVE OFF CACHE INTERNAL "Remove all legacy code completely.")
set(GDCM_LEGACY_SILENT OFF CACHE INTERNAL "Silence all legacy code messages.")
set(GDCM_SUPPORT_BROKEN_IMPLEMENTATION ON CACHE INTERNAL "Handle broken DICOM")
set(GDCM_TEMP_DIRECTORY "${ITK_TEST_OUTPUT_DIR}" CACHE INTERNAL "Path to a valid temp directory")
set(GDCM_USE_KAKADU OFF CACHE INTERNAL "Use kakadu lib, only turn it on if you know what you are doing.")
set(GDCM_USE_PVRG OFF CACHE INTERNAL "Use pvrg lib, only turn it on if you know what you are doing.")
set(GDCM_USE_SYSTEM_LJPEG OFF CACHE INTERNAL "Use system ljpeg (ijg lib)")
set(GDCM_USE_SYSTEM_OPENSSL OFF CACHE INTERNAL "Use system OpenSSL")
set(GDCM_USE_SYSTEM_PODOFO OFF CACHE INTERNAL "Use system podofo (pdf)")
set(GDCM_USE_SYSTEM_POPPLER OFF CACHE INTERNAL "Use system poppler (pdf)")
set(GDCM_USE_SYSTEM_UUID OFF CACHE INTERNAL "Use system uuid")
set(GDCM_USE_WXWIDGETS OFF CACHE INTERNAL "wxWidgets bridge ?")
set(GDCM_USE_JPEGLS ON CACHE INTERNAL "")
set(GDCM_USE_SYSTEM_CHARLS OFF CACHE INTERNAL "")
set(GDCM_USE_SYSTEM_JSON OFF CACHE INTERNAL "")
set(GDCM_USE_SYSTEM_LIBXML2 OFF CACHE INTERNAL "")
set(GDCM_USE_SYSTEM_PAPYRUS3 OFF CACHE INTERNAL "")
set(GDCM_USE_SYSTEM_SOCKETXX OFF CACHE INTERNAL "")
set(GDCM_ALWAYS_TRACE_MACRO OFF CACHE INTERNAL "")
set(GDCMV2_0_COMPATIBILITY ON CACHE INTERNAL "")
set(GDCM_WRITE_ODD_LENGTH OFF CACHE INTERNAL "Do not change")
set(GDCM_DEBUG_POSTFIX "" CACHE INTERNAL "")
set(GDCM_USE_COREFOUNDATION_LIBRARY OFF CACHE INTERNAL "")

# GDCM uses these to refer to executable and library locations
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
set(LIBRARY_OUTPUT_PATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

add_subdirectory(gdcm)

foreach(lib
    gdcmCommon
    gdcmDICT
    gdcmDSED
    gdcmIOD
    gdcmMEXD
    gdcmMSFF
    gdcmcharls
    gdcmjpeg12
    gdcmjpeg16
    gdcmjpeg8
    gdcmopenjpeg
    itkgdcmopenjp2
    gdcmsocketxx
    gdcmuuid
    )
  if(TARGET ${lib}) # some gdcm targets are conditional
    itk_module_target(${lib} NO_INSTALL)
  endif()
endforeach()
