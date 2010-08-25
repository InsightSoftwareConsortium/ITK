# Define the srcs for Data Dictionary
# DICT

# Add the include paths
INCLUDE_DIRECTORIES(
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_BINARY_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  "${GDCM_SOURCE_DIR}/Source/InformationObjectDefinition"
  )


#-----------------------------------------------------------------------------
# DICOM dictionary stuff
#SET(PREP_DICT_SRCS
#  ${GDCM_SOURCE_DIR}/Source/DataDictionary/gdcmDictConverter.cxx
#  ${GDCM_SOURCE_DIR}/Source/DataDictionary/gdcmPrepDict.cxx
#  )
#
#ADD_EXECUTABLE(PrepDict ${PREP_DICT_SRCS}
#  "${GDCM_SOURCE_DIR}/Source/Common/gdcmSwapCode.cxx"
#  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition/gdcmVR.cxx"
#  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition/gdcmVM.cxx"
#  )
#
#ADD_EXECUTABLE(PrepGroupName
#  ${GDCM_SOURCE_DIR}/Source/DataDictionary/gdcmPrepGroupName.cxx
#  )
#
#  GET_TARGET_PROPERTY( PREP_DICT
#    PrepDict
#    LOCATION
#    )
#  GET_TARGET_PROPERTY( PREP_GROUPNAME
#    PrepGroupName
#    LOCATION
#    )
#SET(DICT_LIST
#  DICOMV3
#  NIH
#  SPI
#  )
#
## List of file needed to get all dict into c++ code
#SET(DICT_SRCS)
#SET(DEFAULT_DICTS)
#FOREACH(dict ${DICT_LIST})
#  SET(CXX_DICT ${GDCM_BINARY_DIR}/Source/DataDictionary/gdcm${dict}.cxx)
#  SET(DICT_SRCS ${DICT_SRCS} ${CXX_DICT})
#  ADD_CUSTOM_COMMAND(
#    OUTPUT    ${CXX_DICT}
#    COMMAND   ${PREP_DICT}
#    ARGS      ${GDCM_SOURCE_DIR}/Source/DataDictionary/${dict}.dic
#              ${CXX_DICT}
#              ${dict}
#    DEPENDS   ${GDCM_SOURCE_DIR}/Source/DataDictionary/${dict}.dic
#              ${PREP_DICT}
#    COMMENT   "Generating gdcm${dict}.cxx based on ${dict}.dic"
#    )
#ENDFOREACH(dict)
#
#ADD_CUSTOM_COMMAND(
#  OUTPUT    ${GDCM_BINARY_DIR}/Source/DataDictionary/gdcmDefaultGroupNames.cxx
#  COMMAND   ${PREP_GROUPNAME}
#  ARGS      ${GDCM_SOURCE_DIR}/Source/DataDictionary/GroupName.dic
#            ${GDCM_BINARY_DIR}/Source/DataDictionary/gdcmDefaultGroupNames.cxx
#  DEPENDS   ${GDCM_SOURCE_DIR}/Source/DataDictionary/GroupName.dic
#            ${PREP_GROUPNAME}
#  COMMENT   "Generating gdcmDefaultGroupNames.cxx based on GroupName.dic"
#  )
#

# Since the file gdcmDefaultDicts.cxx does not exist, mark it as GENERATED:
#SET_SOURCE_FILES_PROPERTIES(
#  "${GDCM_BINARY_DIR}/Source/gdcmDefaultDicts.cxx" GENERATED)

#CONFIGURE_FILE(
#  ${GDCM_SOURCE_DIR}/Source/DataDictionary/gdcmDefaultDicts.cxx
#  ${GDCM_BINARY_DIR}/Source/DataDictionary/gdcmDefaultDicts.cxx
#  @ONLY
#  )

SET(DICT_SRCS
  #${DICT_SRCS} # All the dicts cxx resources
  gdcmDicts.cxx
  gdcmDictEntry.cxx
  gdcmDefaultDicts.cxx # pseudo generated file
  gdcmPrivateDefaultDicts.cxx # pseudo generated file
  gdcmGlobal.cxx
  gdcmGroupDict.cxx
  gdcmDefaultGroupNames.cxx
  gdcmUIDs.cxx
  gdcmSOPClassUIDToIOD.cxx
  gdcmCSAHeaderDefaultDicts.cxx
  )

#MESSAGE( ${DICT_SRCS} )
ADD_LIBRARY(gdcmDICT ${DICT_SRCS})
SET_TARGET_PROPERTIES(gdcmDICT PROPERTIES ${GDCM_LIBRARY_PROPERTIES} LINK_INTERFACE_LIBRARIES "")
TARGET_LINK_LIBRARIES(gdcmDICT gdcmDSED gdcmIOD)
#SET_TARGET_PROPERTIES(gdcmDICT PROPERTIES ${GDCM_LIBRARY_PROPERTIES} LINK_INTERFACE_LIBRARIES "gdcmDICT;gdcmIOD")
IF(NOT GDCM_INSTALL_NO_LIBRARIES)
  # Runtime
  INSTALL(TARGETS gdcmDICT
    EXPORT GDCMTargets
    RUNTIME DESTINATION ${GDCM_INSTALL_BIN_DIR} COMPONENT Applications
    LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Libraries ${NAMELINK_SKIP}
    ARCHIVE DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Development
  )
  #Development
  if(NAMELINK_ONLY)
    INSTALL(TARGETS gdcmDICT
    EXPORT GDCMTargets
      LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR} COMPONENT Development ${NAMELINK_ONLY}
    )
  endif(NAMELINK_ONLY)
ENDIF(NOT GDCM_INSTALL_NO_LIBRARIES)

IF(NOT GDCM_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB header_files "*.h")
  INSTALL(FILES ${header_files}
    DESTINATION ${GDCM_INSTALL_INCLUDE_DIR} COMPONENT Headers
  )
ENDIF(NOT GDCM_INSTALL_NO_DEVELOPMENT)

SET(XML_FILES
  ${CMAKE_CURRENT_SOURCE_DIR}/CSAHeader.xml
  ${CMAKE_CURRENT_SOURCE_DIR}/DICOMV3.xml
  ${CMAKE_CURRENT_SOURCE_DIR}/Part6.xml
  ${CMAKE_CURRENT_SOURCE_DIR}/Part7.xml
  ${CMAKE_CURRENT_SOURCE_DIR}/UIDs.xml
  ${CMAKE_CURRENT_SOURCE_DIR}/cp699.xml
  )

SET_SOURCE_FILES_PROPERTIES(
  ${XML_FILES}
  PROPERTIES
  MACOSX_PACKAGE_LOCATION Resources
  )

#-----------------------------------------------------------------------------
# Install Part6.xml
INSTALL(FILES
  ${XML_FILES}
  DESTINATION ${GDCM_INSTALL_DATA_DIR}/XML COMPONENT Libraries
)
