#-----------------------------------------------------------------------------
# "jpeglib.h" is defined here:
INCLUDE_DIRECTORIES(
  ${GDCM_SOURCE_DIR}/src
  ${GDCM_BINARY_DIR}/ #for gdcmConfigure.h
  ${GDCM_BINARY_DIR}/src #for gdcmjpeg 8, 12 and 16 
)

SET(libgdcm_la_SOURCES
   gdcmBinEntry.cxx
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
   gdcmDocEntrySet.cxx
   gdcmDocument.cxx
   gdcmElementSet.cxx
   gdcmException.cxx
   gdcmFile.cxx
   gdcmGlobal.cxx
   gdcmHeader.cxx
   gdcmHeaderHelper.cxx
   gdcmJPEGFragment.cxx
   gdcmJPEGFragmentsInfo.cxx
   gdcmJpeg8.cxx
   gdcmJpeg12.cxx
   gdcmJpeg16.cxx
   gdcmJpeg2000.cxx
   gdcmPixelConvert.cxx
   gdcmRLEFrame.cxx
   gdcmRLEFramesInfo.cxx
   gdcmSeqEntry.cxx
   gdcmSQItem.cxx
   gdcmTS.cxx
   gdcmUtil.cxx
   gdcmValEntry.cxx   
   gdcmVR.cxx
   ${GDCM_BINARY_DIR}/src/gdcmDefaultDicts.cxx
   )

ADD_LIBRARY(gdcm ${libgdcm_la_SOURCES} )
IF(UNIX)
  TARGET_LINK_LIBRARIES(gdcm 
    itkjpeg8 
    itkjpeg12
    itkjpeg16
  )
ELSE(UNIX)
  TARGET_LINK_LIBRARIES(gdcm 
    itkjpeg8 
    itkjpeg12
    itkjpeg16
    Wsock32.lib   #doesn't exist on cygwin
  )
ENDIF(UNIX)

#The following is not working because when a header file is not found it tries 
#to find one in the binary dir
#INSTALL_FILES(/include .h ${libgdcm_la_SOURCES})
INSTALL_FILES(/include "\\.h$")

INSTALL_TARGETS(/lib/ gdcm)
