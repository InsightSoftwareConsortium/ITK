SET(ITKIO_SRCS
itkAnalyzeImageIO.cxx
itkAnalyzeImageIOFactory.cxx
itkDicomImageIOFactory.cxx
itkDicomImageIO.cxx
itkDICOMImageIO2Factory.cxx
itkDICOMImageIO2.cxx
itkFileIteratorBase.cxx
itkGiplImageIOFactory.cxx
itkGiplImageIO.cxx
itkImageIOBase.cxx
itkImageIOFactory.cxx
itkIOCommon.cxx
itkMetaImageIOFactory.cxx
itkMetaImageIO.cxx
itkNumericSeriesFileIterator.cxx
itkPNGImageIOFactory.cxx
itkPNGImageIO.cxx
itkVOLImageIOFactory.cxx
itkVOLImageIO.cxx
itkVTKImageIOFactory.cxx
itkVTKImageIO.cxx
itkStimulateImageIO.cxx
itkStimulateImageIOFactory.cxx
itkWriter.cxx
)

# Add ITKIO library
ADD_LIBRARY(ITKIO ${ITKIO_SRCS})
TARGET_LINK_LIBRARIES(ITKIO ITKCommon ${ITK_PNG_LIBRARIES}
                      ${ITK_ZLIB_LIBRARIES} ITKMetaIO ITKDICOMParser)

INSTALL_TARGETS(/lib/InsightToolkit ITKIO)
INSTALL_FILES(/include/InsightToolkit/IO "(\\.h|\\.txx)$")
