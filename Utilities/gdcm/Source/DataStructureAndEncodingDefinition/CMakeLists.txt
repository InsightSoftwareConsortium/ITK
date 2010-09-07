# Define the srcs for Data Structure and Encoding Definitions
# DSED
SET(DSED_SRCS
  gdcmTag.cxx
  gdcmPrivateTag.cxx
  gdcmCodeString.cxx
  gdcmByteValue.cxx
  gdcmFileSet.cxx
  gdcmDataSet.cxx
  gdcmByteSwapFilter.cxx
  gdcmUNExplicitImplicitDataElement.cxx
  gdcmExplicitDataElement.cxx
  gdcmFileMetaInformation.cxx
  gdcmFragment.cxx
  gdcmImplicitDataElement.cxx
  gdcmItem.cxx
  gdcmReader.cxx
  gdcmWriter.cxx
  #gdcmParser.cxx
  gdcmCSAHeader.cxx
  gdcmPDBHeader.cxx
  gdcmSequenceOfFragments.cxx
  gdcmSequenceOfItems.cxx
  gdcmTransferSyntax.cxx
  gdcmMediaStorage.cxx
  gdcmVM.cxx
  gdcmVR.cxx
  gdcmFile.cxx
  gdcmPreamble.cxx
  gdcmParseException.cxx
  gdcmDataElement.cxx
  gdcmUNExplicitDataElement.cxx
  gdcmCP246ExplicitDataElement.cxx
  gdcmExplicitImplicitDataElement.cxx
  gdcmVR16ExplicitDataElement.cxx
  )

# Add the include paths
INCLUDE_DIRECTORIES(
  # Bin:
  "${GDCM_BINARY_DIR}/Source/Common"

  # src:
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition/"


  "${GDCM_SOURCE_DIR}/Utilities"
 )
IF(NOT GDCM_USE_SYSTEM_ZLIB)
INCLUDE_DIRECTORIES(
  "${GDCM_BINARY_DIR}/Utilities/gdcmzlib"
)
ENDIF(NOT GDCM_USE_SYSTEM_ZLIB)


ADD_LIBRARY(gdcmDSED ${DSED_SRCS})
TARGET_LINK_LIBRARIES(gdcmDSED gdcmCommon)
# zlib stuff are actually included (template) so we need to link them here.
TARGET_LINK_LIBRARIES(gdcmDSED ${GDCM_ZLIB_LIBRARIES})
SET_TARGET_PROPERTIES(gdcmDSED PROPERTIES ${GDCM_LIBRARY_PROPERTIES} LINK_INTERFACE_LIBRARIES "gdcmCommon")

IF(NOT GDCM_INSTALL_NO_LIBRARIES)
  # Runtime
  INSTALL(TARGETS gdcmDSED
    EXPORT ${GDCM_TARGETS_NAME}
    RUNTIME DESTINATION ${GDCM_INSTALL_BIN_DIR} COMPONENT Applications
    LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Libraries ${NAMELINK_SKIP}
    ARCHIVE DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Development
  )
  #Development
  if(NAMELINK_ONLY)
    INSTALL(TARGETS gdcmDSED
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
