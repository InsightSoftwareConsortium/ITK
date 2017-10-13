# Add the include paths
include_directories(
  "${GDCM_BINARY_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  "${GDCM_SOURCE_DIR}/Source/InformationObjectDefinition"
  "${GDCM_SOURCE_DIR}/Source/MediaStorageAndFileFormat"

  "${GDCM_BINARY_DIR}/Testing/Source/Data"
  "${GDCM_SOURCE_DIR}/Testing/Source/Data"

  "${GDCM_SOURCE_DIR}/Utilities"
  )

if(NOT GDCM_USE_SYSTEM_SOCKETXX)
  include_directories(
    "${GDCM_SOURCE_DIR}/Utilities/socketxx"
    "${GDCM_SOURCE_DIR}/Utilities/socketxx/socket++" # local.h
    "${GDCM_BINARY_DIR}/Utilities/socketxx/socket++" # config.h
    )
endif()

set(MessageExchangeDefinition_SRCS
  gdcmAAbortPDU.cxx
  gdcmAAssociateACPDU.cxx
  gdcmAAssociateRJPDU.cxx
  gdcmAAssociateRQPDU.cxx
  gdcmAbstractSyntax.cxx
  gdcmApplicationContext.cxx
  gdcmAReleaseRPPDU.cxx
  gdcmAReleaseRQPDU.cxx
  gdcmARTIMTimer.cxx
  gdcmAsynchronousOperationsWindowSub.cxx
  gdcmBaseQuery.cxx
  gdcmBaseRootQuery.cxx
  gdcmCEchoMessages.cxx
  gdcmCFindMessages.cxx
  gdcmCMoveMessages.cxx
  gdcmCommandDataSet.cxx
  gdcmCompositeMessageFactory.cxx
  gdcmCompositeNetworkFunctions.cxx
  gdcmCStoreMessages.cxx
  gdcmFindPatientRootQuery.cxx
  gdcmFindStudyRootQuery.cxx
  gdcmImplementationClassUIDSub.cxx
  gdcmImplementationUIDSub.cxx
  gdcmImplementationVersionNameSub.cxx
  gdcmMaximumLengthSub.cxx
  gdcmModalityPerformedProcedureStepCreateQuery.cxx
  gdcmModalityPerformedProcedureStepSetQuery.cxx
  gdcmMovePatientRootQuery.cxx
  gdcmMoveStudyRootQuery.cxx
  gdcmNActionMessages.cxx
  gdcmNCreateMessages.cxx
  gdcmNDeleteMessages.cxx
  gdcmNEventReportMessages.cxx
  gdcmNGetMessages.cxx
  gdcmNormalizedMessageFactory.cxx
  gdcmNormalizedNetworkFunctions.cxx
  gdcmNSetMessages.cxx
  gdcmPDataTFPDU.cxx
  gdcmPDUFactory.cxx
  gdcmPresentationContext.cxx
  gdcmPresentationContextAC.cxx
  gdcmPresentationContextGenerator.cxx
  gdcmPresentationContextRQ.cxx
  gdcmPresentationDataValue.cxx
  gdcmQueryBase.cxx
  gdcmQueryFactory.cxx
  gdcmQueryImage.cxx
  gdcmQueryPatient.cxx
  gdcmQuerySeries.cxx
  gdcmQueryStudy.cxx
  gdcmRoleSelectionSub.cxx
  gdcmServiceClassApplicationInformation.cxx
  gdcmServiceClassUser.cxx
  gdcmSOPClassExtendedNegociationSub.cxx
  gdcmTransferSyntaxSub.cxx
  gdcmULActionAA.cxx
  gdcmULActionAE.cxx
  gdcmULActionAR.cxx
  gdcmULActionDT.cxx
  gdcmULBasicCallback.cxx
  gdcmULConnection.cxx
  gdcmULConnectionInfo.cxx
  gdcmULConnectionManager.cxx
  gdcmULTransitionTable.cxx
  gdcmULWritingCallback.cxx
  gdcmUserInformation.cxx
  gdcmWLMFindQuery.cxx
  )

add_library(gdcmMEXD ${MessageExchangeDefinition_SRCS})
target_link_libraries(gdcmMEXD LINK_PRIVATE gdcmMSFF gdcmDICT gdcmDSED gdcmIOD)
if(GDCM_USE_SYSTEM_SOCKETXX)
  target_link_libraries(gdcmMEXD LINK_PRIVATE socket++)
else()
  target_link_libraries(gdcmMEXD LINK_PRIVATE gdcmsocketxx)
endif()
if(WIN32)
  target_link_libraries(gdcmMEXD LINK_PRIVATE ws2_32)
endif()
set_target_properties(gdcmMEXD PROPERTIES ${GDCM_LIBRARY_PROPERTIES})
if(BUILD_SHARED_LIBS)
  #set_target_properties(gdcmMEXD PROPERTIES INTERFACE_LINK_LIBRARIES "" LINK_INTERFACE_LIBRARIES "")
endif()

# libs
install_library(gdcmMEXD)
# PDB
install_pdb(gdcmMEXD)
# include files
install_includes("*.h")
