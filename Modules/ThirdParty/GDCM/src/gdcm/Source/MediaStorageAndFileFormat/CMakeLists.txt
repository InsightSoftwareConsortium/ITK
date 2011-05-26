# Define the srcs for Media Storage And FileFormat
# MSFF
SET(MSFF_SRCS
  gdcmAnonymizer.cxx
  gdcmDICOMDIRGenerator.cxx
  gdcmSpacing.cxx
  gdcmFileExplicitFilter.cxx
  gdcmFileDerivation.cxx
  gdcmImageFragmentSplitter.cxx
  gdcmTagPath.cxx
  gdcmSimpleSubjectWatcher.cxx
  gdcmAnonymizeEvent.cxx
  gdcmPixmap.cxx
  gdcmBitmap.cxx
  gdcmRescaler.cxx
  gdcmImageToImageFilter.cxx
  gdcmPixmapToPixmapFilter.cxx
  gdcmImageChangeTransferSyntax.cxx
  gdcmImageApplyLookupTable.cxx
  gdcmOrientation.cxx
  gdcmDataSetHelper.cxx
  gdcmImageChangePlanarConfiguration.cxx
  gdcmImageChangePhotometricInterpretation.cxx
  gdcmDirectionCosines.cxx
  gdcmSorter.cxx
  gdcmSerieHelper.cxx
  gdcmIPPSorter.cxx
  gdcmApplicationEntity.cxx
  gdcmDICOMDIR.cxx
  gdcmSpectroscopy.cxx
  gdcmEncapsulatedDocument.cxx
  gdcmSplitMosaicFilter.cxx
  gdcmFiducials.cxx
  gdcmWaveform.cxx
  gdcmPersonName.cxx
  gdcmIconImage.cxx
  gdcmUIDGenerator.cxx
  gdcmPrinter.cxx
  gdcmDictPrinter.cxx
  gdcmScanner.cxx
  gdcmPixmapReader.cxx
  gdcmImageReader.cxx
  gdcmPixmapWriter.cxx
  gdcmImageWriter.cxx
  gdcmStringFilter.cxx
  gdcmImageHelper.cxx
  gdcmValidate.cxx
  gdcmDumper.cxx
  gdcmImage.cxx
  gdcmImageConverter.cxx
  gdcmImageCodec.cxx
  gdcmJPEG12Codec.cxx
  gdcmRLECodec.cxx
  gdcmPDFCodec.cxx
  gdcmAudioCodec.cxx
  gdcmJPEG16Codec.cxx
  gdcmJPEGLSCodec.cxx
  gdcmJPEG8Codec.cxx
  gdcmJPEGCodec.cxx
  gdcmPVRGCodec.cxx
  gdcmKAKADUCodec.cxx
  gdcmPNMCodec.cxx
  gdcmRAWCodec.cxx
  gdcmLookupTable.cxx
  gdcmOverlay.cxx
  gdcmCurve.cxx
  gdcmPhotometricInterpretation.cxx
  gdcmPixelFormat.cxx
  gdcmSegmentedPaletteColorLookupTable.cxx
  )
if(OPENJPEG_VERSION VERSION_LESS 2.0)
  list(APPEND MSFF_SRCS
    gdcmJPEG2000Codec.cxx
  )
else ()
  list(APPEND MSFF_SRCS
    gdcmOpenJPEG2Codec.cxx
  )
endif()

# Do the proper thing when building static...if only there was configured
# headers or def files instead
IF(NOT BUILD_SHARED_LIBS)
  SET_SOURCE_FILES_PROPERTIES(gdcmJPEG2000Codec.cxx
                              PROPERTIES
                              COMPILE_FLAGS -DOPJ_STATIC
                              )
ENDIF(NOT BUILD_SHARED_LIBS)


# Add the include paths
INCLUDE_DIRECTORIES(
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_BINARY_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  "${GDCM_SOURCE_DIR}/Source/InformationObjectDefinition"
  ${CMAKE_CURRENT_SOURCE_DIR}

  # FIXME:
  "${GDCM_SOURCE_DIR}/Utilities"
  "${GDCM_BINARY_DIR}/Utilities"
  )

# CharLS
IF(GDCM_USE_JPEGLS)
  #INCLUDE_DIRECTORIES("${GDCM_BINARY_DIR}/Utilities/gdcmcharls")
ENDIF(GDCM_USE_JPEGLS)

IF(GDCM_USE_SYSTEM_OPENJPEG)
  INCLUDE_DIRECTORIES(${OPENJPEG_INCLUDE_DIRS} )
ELSE(GDCM_USE_SYSTEM_OPENJPEG)
  INCLUDE_DIRECTORIES(
  "${GDCM_BINARY_DIR}/Utilities/gdcmopenjpeg"
    )
ENDIF(GDCM_USE_SYSTEM_OPENJPEG)
IF(GDCM_USE_SYSTEM_LJPEG)
  #MESSAGE(${LJPEG_INCLUDE_DIRS} )
  INCLUDE_DIRECTORIES(${LJPEG_INCLUDE_DIRS} )
ENDIF(GDCM_USE_SYSTEM_LJPEG)
IF(NOT GDCM_USE_SYSTEM_ZLIB)
  INCLUDE_DIRECTORIES(
    "${GDCM_BINARY_DIR}/Utilities/gdcmzlib"
  )
ENDIF(NOT GDCM_USE_SYSTEM_ZLIB)
IF(GDCM_USE_SYSTEM_UUID)
  INCLUDE_DIRECTORIES(
    ${UUID_INCLUDE_DIR}
  )
  SET(GDCMUUID ${UUID_LIBRARIES})
ELSE(GDCM_USE_SYSTEM_UUID)
  INCLUDE_DIRECTORIES(
    "${GDCM_BINARY_DIR}/Utilities/gdcmuuid" # uuid_mangle.h
  )
  SET(GDCMUUID gdcmuuid)
ENDIF(GDCM_USE_SYSTEM_UUID)

ADD_LIBRARY(gdcmMSFF ${MSFF_SRCS})

# main libs:
TARGET_LINK_LIBRARIES(gdcmMSFF gdcmIOD gdcmDSED gdcmDICT ${GDCM_LJPEG_LIBRARIES} ${GDCM_OPENJPEG_LIBRARIES})
SET_TARGET_PROPERTIES(gdcmMSFF PROPERTIES ${GDCM_LIBRARY_PROPERTIES} LINK_INTERFACE_LIBRARIES "gdcmDSED;gdcmDICT;gdcmIOD")
IF(GDCM_USE_JPEGLS)
  TARGET_LINK_LIBRARIES(gdcmMSFF gdcmcharls)
ENDIF(GDCM_USE_JPEGLS)

IF(CMAKE_COMPILER_IS_GNUCXX AND MINGW)
  # I am getting:
# CMakeFiles/gdcmMSFF.dir/gdcmScanner.obj(.text$_ZN4gdcm6ReaderC1Ev[gdcm::Reader::Reader()]+0x3a):gdcmScanner.cxx: variable 'vtable for gdcm::Reader' can't be auto-imported. Please read the documentation for ld's --enable-auto-import for details.
# CMakeFiles/gdcmMSFF.dir/gdcmImageReader.obj(.text$_ZN4gdcm6ReaderC2Ev[gdcm::Reader::Reader()]+0x3a):gdcmImageReader.cxx: variable 'vtable for gdcm::Reader' can't be auto-imported. Please read the documentation for ld's --enable-auto-import for details.
# CMakeFiles/gdcmMSFF.dir/gdcmImageWriter.obj(.text$_ZN4gdcm6WriterC2Ev[gdcm::Writer::Writer()]+0x3a):gdcmImageWriter.cxx: variable 'vtable for gdcm::Writer' can't be auto-imported. Please read the documentation for ld's --enable-auto-import for details.

  SET_TARGET_PROPERTIES(gdcmMSFF PROPERTIES LINK_FLAGS "-Wl,--enable-runtime-pseudo-reloc")
ENDIF(CMAKE_COMPILER_IS_GNUCXX AND MINGW)
#IF(HAVE_UUIDCREATE)
IF(WIN32)
  # For UuidCreate
  # http://msdn.microsoft.com/en-us/library/aa379205(VS.85).aspx
  TARGET_LINK_LIBRARIES(gdcmMSFF rpcrt4)
#ENDIF(HAVE_UUIDCREATE)
ELSE(WIN32)
TARGET_LINK_LIBRARIES(gdcmMSFF ${GDCMUUID})
ENDIF(WIN32)
IF(NOT GDCM_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS gdcmMSFF
    EXPORT ${GDCM_TARGETS_NAME}
    RUNTIME DESTINATION ${GDCM_INSTALL_BIN_DIR} COMPONENT Applications
    LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Libraries ${NAMELINK_SKIP}
    ARCHIVE DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Development
  )
  if(NAMELINK_ONLY)
    INSTALL(TARGETS gdcmMSFF
    EXPORT ${GDCM_TARGETS_NAME}
      LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Development ${NAMELINK_ONLY}
      )
  endif(NAMELINK_ONLY)
ENDIF(NOT GDCM_INSTALL_NO_LIBRARIES)

IF(NOT GDCM_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB header_files "*.h" "*.txx")
  INSTALL(FILES ${header_files}
    DESTINATION ${GDCM_INSTALL_INCLUDE_DIR} COMPONENT Headers
  )
ENDIF(NOT GDCM_INSTALL_NO_DEVELOPMENT)
