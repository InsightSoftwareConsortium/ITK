INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/IO)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)

ADD_LIBRARY(ITKIOTcl
  itkTkImageViewer2D.cxx
)

ITK_WRAP_TCL(ITKIOTcl
  wrap_ITKIO.cxx
  wrap_IOBase.cxx
  wrap_itkImageFileReader.cxx
  wrap_itkImageFileWriter.cxx
  wrap_itkTkImageViewer2D.cxx
)

TARGET_LINK_LIBRARIES(ITKIOTcl ITKIO ${CABLE_TCL_LIBRARY} ${TK_LIBRARY})

INSTALL_TARGETS(/lib/InsightToolkit ITKIOTcl)
