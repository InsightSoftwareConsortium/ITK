#-----------------------------------------------------------------------------
# Rebuild gdcm whenever a file starting with gdcm* is modified
INCLUDE_REGULAR_EXPRESSION("^gdcm.*$")

INCLUDE_DIRECTORIES(
  ${GDCM_SOURCE_DIR}/src
  ${GDCM_BINARY_DIR} #for gdcmConfigure.h
  ${GDCM_BINARY_DIR}/src #for gdcmjpeg 8, 12 and 16 
)

SET(libgdcm_la_SOURCES
   gdcmBase.cxx
   gdcmBinEntry.cxx
   gdcmContentEntry.cxx
   gdcmDebug.cxx
   gdcmDicomDir.cxx
   gdcmDicomDirElement.cxx
   gdcmDicomDirImage.cxx
   gdcmDicomDirMeta.cxx
   gdcmDicomDirObject.cxx
   gdcmDicomDirPatient.cxx
   gdcmDicomDirSerie.cxx
   gdcmDicomDirStudy.cxx
   gdcmDict.cxx
   gdcmDictEntry.cxx
   gdcmDictSet.cxx
   gdcmDirList.cxx
   gdcmDocEntry.cxx
   gdcmDocEntryArchive.cxx
   gdcmDocEntrySet.cxx
   gdcmDocument.cxx
   gdcmElementSet.cxx
   gdcmException.cxx
   gdcmFileHelper.cxx
   gdcmGlobal.cxx
   gdcmFile.cxx
   gdcmJPEGFragment.cxx
   gdcmJPEGFragmentsInfo.cxx
   gdcmJpeg8.cxx
   gdcmJpeg12.cxx
   gdcmJpeg16.cxx
   gdcmJpeg2000.cxx
   gdcmPixelReadConvert.cxx
   gdcmPixelWriteConvert.cxx
   gdcmRLEFrame.cxx
   gdcmRLEFramesInfo.cxx
   gdcmSeqEntry.cxx
   gdcmSerieHelper.cxx
   gdcmSQItem.cxx
   gdcmTS.cxx
   gdcmUtil.cxx
   gdcmValEntry.cxx   
   gdcmVR.cxx
   ${GDCM_BINARY_DIR}/src/gdcmDefaultDicts.cxx
   )

ADD_LIBRARY(itkgdcm ${libgdcm_la_SOURCES} )
TARGET_LINK_LIBRARIES(itkgdcm 
  itkjpeg8 
  itkjpeg12
  itkjpeg16
)
IF(WIN32)
  IF(NOT BORLAND)
    TARGET_LINK_LIBRARIES(itkgdcm wsock32 snmpapi)
  ENDIF(NOT BORLAND)
ENDIF(WIN32)

# Need nsl to resolve gethostbyname on SunOS-5.8
# and socket also
IF(CMAKE_SYSTEM MATCHES "SunOS.*")
  TARGET_LINK_LIBRARIES(itkgdcm socket nsl)
ENDIF(CMAKE_SYSTEM MATCHES "SunOS.*")


INSTALL_FILES(/include/InsightToolkit/gdcm/src "\\.h$")
INSTALL_TARGETS(/lib/InsightToolkit itkgdcm)
