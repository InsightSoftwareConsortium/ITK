SET(ITKIO_SRCS
  itkAnalyzeImageIO.cxx
  itkAnalyzeImageIOFactory.cxx
  itkDICOMImageIO2.cxx
  itkDICOMImageIO2Factory.cxx
  itkDicomImageIO.cxx
  itkDicomImageIOFactory.cxx
  itkFileIteratorBase.cxx
  itkGE4ImageIO.cxx
  itkGE4ImageIOFactory.cxx
  itkGE5ImageIO.cxx
  itkGE5ImageIOFactory.cxx
  itkGEAdwImageIO.cxx
  itkGEAdwImageIOFactory.cxx
  itkSiemensVisionImageIO.cxx
  itkSiemensVisionImageIOFactory.cxx
  itkGiplImageIO.cxx
  itkGiplImageIOFactory.cxx
  itkIOCommon.cxx
  itkIPLCommonImageIO.cxx
  itkImageIOBase.cxx
  itkImageIOFactory.cxx
  itkMetaImageIO.cxx
  itkMetaImageIOFactory.cxx
  itkNumericSeriesFileIterator.cxx
  itkPNGImageIO.cxx
  itkPNGImageIOFactory.cxx
  itkStimulateImageIO.cxx
  itkStimulateImageIOFactory.cxx
  itkVOLImageIO.cxx
  itkVOLImageIOFactory.cxx
  itkVTKImageIO.cxx
  itkVTKImageIOFactory.cxx
  itkWriter.cxx
)

# Add ITKIO library
ADD_LIBRARY(ITKIO ${ITKIO_SRCS})
TARGET_LINK_LIBRARIES(ITKIO ITKCommon ${ITK_PNG_LIBRARIES}
                      ${ITK_ZLIB_LIBRARIES} ITKMetaIO ITKDICOMParser)

INSTALL_TARGETS(/lib/InsightToolkit ITKIO)
INSTALL_FILES(/include/InsightToolkit/IO "(\\.h|\\.txx)$")
