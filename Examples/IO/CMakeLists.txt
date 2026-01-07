project(IOExamples)

add_executable(ImageReadWrite ImageReadWrite.cxx)
target_link_libraries(
  ImageReadWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ImageReadCastWrite ImageReadCastWrite.cxx)
target_link_libraries(
  ImageReadCastWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageIntensityModule
)

add_executable(
  ImageReadExtractFilterInsertWrite
  ImageReadExtractFilterInsertWrite.cxx
)
target_link_libraries(
  ImageReadExtractFilterInsertWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageGridModule
    ITK::ITKSmoothingModule
)

add_executable(
  ImageReadRegionOfInterestWrite
  ImageReadRegionOfInterestWrite.cxx
)
target_link_libraries(
  ImageReadRegionOfInterestWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageGridModule
)

add_executable(RGBImageReadWrite RGBImageReadWrite.cxx)
target_link_libraries(
  RGBImageReadWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(CovariantVectorImageRead CovariantVectorImageRead.cxx)
target_link_libraries(
  CovariantVectorImageRead
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageIntensityModule
)

add_executable(
  CovariantVectorImageExtractComponent
  CovariantVectorImageExtractComponent.cxx
)
target_link_libraries(
  CovariantVectorImageExtractComponent
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageIntensityModule
)

add_executable(VectorImageReadWrite VectorImageReadWrite.cxx)
target_link_libraries(
  VectorImageReadWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ComplexImageReadWrite ComplexImageReadWrite.cxx)
target_link_libraries(
  ComplexImageReadWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ImageReadExportVTK ImageReadExportVTK.cxx)
target_link_libraries(
  ImageReadExportVTK
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOVTKModule
)

add_executable(RGBImageSeriesReadWrite RGBImageSeriesReadWrite.cxx)
target_link_libraries(
  RGBImageSeriesReadWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOPNGModule
)

add_executable(ImageSeriesReadWrite ImageSeriesReadWrite.cxx)
target_link_libraries(
  ImageSeriesReadWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOPNGModule
)

add_executable(ImageSeriesReadWrite2 ImageSeriesReadWrite2.cxx)
target_link_libraries(
  ImageSeriesReadWrite2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOPNGModule
)

add_executable(ImageReadImageSeriesWrite ImageReadImageSeriesWrite.cxx)
target_link_libraries(
  ImageReadImageSeriesWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(DicomSeriesReadImageWrite2 DicomSeriesReadImageWrite2.cxx)
target_link_libraries(
  DicomSeriesReadImageWrite2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOGDCMModule
)

add_executable(
  DicomSeriesReadGaussianImageWrite
  DicomSeriesReadGaussianImageWrite.cxx
)
target_link_libraries(
  DicomSeriesReadGaussianImageWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKIOGDCMModule
    ITK::ITKSmoothingModule
)

add_executable(DicomSeriesReadPrintTags DicomSeriesReadPrintTags.cxx)
target_link_libraries(
  DicomSeriesReadPrintTags
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKIOGDCMModule
)

add_executable(DicomImageReadPrintTags DicomImageReadPrintTags.cxx)
target_link_libraries(
  DicomImageReadPrintTags
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOGDCMModule
    ITK::ITKGDCMModule
)

add_executable(DicomImageReadWrite DicomImageReadWrite.cxx)
target_link_libraries(
  DicomImageReadWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKIOGDCMModule
    ITK::ITKImageIntensityModule
)

add_executable(ImageReadDicomSeriesWrite ImageReadDicomSeriesWrite.cxx)
target_link_libraries(
  ImageReadDicomSeriesWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOGDCMModule
)

add_executable(
  DicomImageReadChangeHeaderWrite
  DicomImageReadChangeHeaderWrite.cxx
)
target_link_libraries(
  DicomImageReadChangeHeaderWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOGDCMModule
)

add_executable(DicomSeriesReadSeriesWrite DicomSeriesReadSeriesWrite.cxx)
target_link_libraries(
  DicomSeriesReadSeriesWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKIOGDCMModule
)

add_executable(DicomPrintPatientInformation DicomPrintPatientInformation.cxx)
target_link_libraries(
  DicomPrintPatientInformation
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIOGDCMModule
)

add_executable(TransformReadWrite TransformReadWrite.cxx)
target_link_libraries(
  TransformReadWrite
  PRIVATE
    ITK::ITKTransformIO
    ITK::ITKTransformModule
    ITK::ITKTransformFactoryModule
)

add_executable(VisibleHumanStreamReadWrite VisibleHumanStreamReadWrite.cxx)
target_link_libraries(
  VisibleHumanStreamReadWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKIORAWModule
    ITK::ITKImageComposeModule
    ITK::ITKImageGridModule
    ITK::ITKSmoothingModule
)

add_executable(VisibleHumanPasteWrite VisibleHumanPasteWrite.cxx)
target_link_libraries(
  VisibleHumanPasteWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageAdaptorsModule
    ITK::ITKImageComposeModule
    ITK::ITKImageGradientModule
)

add_executable(CovariantVectorImageWrite CovariantVectorImageWrite.cxx)
target_link_libraries(
  CovariantVectorImageWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKImageGradientModule
)

add_executable(ImageReadExtractWrite ImageReadExtractWrite.cxx)
target_link_libraries(
  ImageReadExtractWrite
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(IOPlugin IOPlugin.cxx)
target_link_libraries(IOPlugin PRIVATE ITK::ITKImageIO)

add_executable(IOFactoryRegistration IOFactoryRegistration.cxx)
target_link_libraries(IOFactoryRegistration PRIVATE ITK::ITKImageIO)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()

add_subdirectory(XML)
