SET(ITKIO_SRCS
  itkAnalyzeImageIO.cxx
  itkAnalyzeImageIOFactory.cxx
  itkBrains2HeaderBase.cxx
  itkBrains2HeaderFactory.cxx
  itkBrains2IPLHeaderInfo.cxx
  itkBrains2MaskHeaderInfo.cxx
  itkBrains2MaskImageIO.cxx
  itkBrains2MaskImageIOFactory.cxx
  itkBMPImageIO.cxx
  itkBMPImageIOFactory.cxx
  itkDICOMImageIO2.cxx
  itkDICOMImageIO2Factory.cxx
  itkDicomImageIO.cxx
  itkDicomImageIOFactory.cxx
  itkDICOMSeriesFileNames.cxx
  itkGE4ImageIO.cxx
  itkGE4ImageIOFactory.cxx
  itkGE5ImageIO.cxx
  itkGE5ImageIOFactory.cxx
  itkGEAdwImageIO.cxx
  itkGEAdwImageIOFactory.cxx
  itkJPEGImageIO.cxx
  itkJPEGImageIOFactory.cxx
  itkRegularExpressionSeriesFileNames.cxx
  itkArchetypeSeriesFileNames.cxx
  itkSiemensVisionImageIO.cxx
  itkSiemensVisionImageIOFactory.cxx
  itkGDCMImageIO.cxx
  itkGDCMImageIOFactory.cxx
  itkGDCMSeriesFileNames.cxx
  itkGiplImageIO.cxx
  itkGiplImageIOFactory.cxx
  itkIOCommon.cxx
  itkIPLCommonImageIO.cxx
  itkIPLFileNameList.cxx
  itkImageIOBase.cxx
  itkImageIOFactory.cxx
  itkImageIORegion.cxx
  itkMetaImageIO.cxx
  itkMetaImageIOFactory.cxx
  itkNrrdImageIO.cxx
  itkNrrdImageIOFactory.cxx
  itkNumericSeriesFileNames.cxx
  itkPNGImageIO.cxx
  itkPNGImageIOFactory.cxx
  itkPolygonGroupSpatialObjectXMLFile.cxx
  itkXMLFile.cxx
  itkStimulateImageIO.cxx
  itkStimulateImageIOFactory.cxx
  itkTIFFImageIO.cxx
  itkTIFFImageIOFactory.cxx
  itkTransformFileReader.cxx
  itkTransformFileWriter.cxx
  itkVTKImageIO.cxx
  itkVTKImageIOFactory.cxx
)

# Add ITKIO library
ADD_LIBRARY(ITKIO ${ITKIO_SRCS})

TARGET_LINK_LIBRARIES(ITKIO 
ITKCommon 
ITKNrrdIO 
itkjpeg8
itkjpeg12
itkjpeg16
itkgdcm
${ITK_PNG_LIBRARIES}
${ITK_ZLIB_LIBRARIES} 
${ITK_TIFF_LIBRARIES} 
ITKMetaIO
ITKDICOMParser 
ITKEXPAT)

INSTALL_TARGETS(/lib/InsightToolkit ITKIO)
INSTALL_FILES(/include/InsightToolkit/IO "(\\.h|\\.txx)$")
