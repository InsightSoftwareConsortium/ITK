SET(ITKIO_SRCS
itkWriter.cxx
itkImageIOBase.cxx
itkImageIOFactory.cxx
itkAnalyzeImageIO.cxx
itkAnalyzeImageIOFactory.cxx
itkMetaImageIOFactory.cxx
itkMetaImageIO.cxx
itkPNGImageIOFactory.cxx
itkPNGImageIO.cxx
itkVOLImageIOFactory.cxx
itkVOLImageIO.cxx
itkVTKImageIOFactory.cxx
itkVTKImageIO.cxx
itkDicomImageIOFactory.cxx
itkDicomImageIO.cxx
itkGiplImageIOFactory.cxx
itkGiplImageIO.cxx
itkDICOMImageIO2Factory.cxx
itkDICOMImageIO2.cxx
)

# Add ITKIO library
ADD_LIBRARY(ITKIO ${ITKIO_SRCS})
TARGET_LINK_LIBRARIES(ITKIO ITKCommon ${ITK_PNG_LIBRARIES}
                      ${ITK_ZLIB_LIBRARIES} ITKMetaIO ITKDICOMParser)

INSTALL_TARGETS(/lib/InsightToolkit ITKIO)
INSTALL_FILES(/include/InsightToolkit/IO "(\\.h|\\.txx)$")
