INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/IO)

ADD_LIBRARY(ITKIOTcl)

CABLE_WRAP_TCL(ITKIOTcl
  wrap_ITKIO.cxx
  wrap_IOBase.cxx
  wrap_itkImageFileReader.cxx
  wrap_itkImageFileWriter.cxx
)

LINK_LIBRARIES(ITKIO ITKCommon VXLNumerics ${CABLE_TCL_LIBRARY})

