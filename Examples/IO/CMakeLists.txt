project(IOExamples)

add_executable(ImageReadWrite ImageReadWrite.cxx)
target_link_libraries(
  ImageReadWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ImageReadCastWrite ImageReadCastWrite.cxx)
target_link_libraries(
  ImageReadCastWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
)

add_executable(
  ImageReadExtractFilterInsertWrite
  ImageReadExtractFilterInsertWrite.cxx
)
target_link_libraries(
  ImageReadExtractFilterInsertWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageGridModule
    ITKSmoothingModule
)

add_executable(
  ImageReadRegionOfInterestWrite
  ImageReadRegionOfInterestWrite.cxx
)
target_link_libraries(
  ImageReadRegionOfInterestWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageGridModule
)

add_executable(RGBImageReadWrite RGBImageReadWrite.cxx)
target_link_libraries(
  RGBImageReadWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(CovariantVectorImageRead CovariantVectorImageRead.cxx)
target_link_libraries(
  CovariantVectorImageRead
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
)

add_executable(
  CovariantVectorImageExtractComponent
  CovariantVectorImageExtractComponent.cxx
)
target_link_libraries(
  CovariantVectorImageExtractComponent
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageIntensityModule
)

add_executable(VectorImageReadWrite VectorImageReadWrite.cxx)
target_link_libraries(
  VectorImageReadWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ComplexImageReadWrite ComplexImageReadWrite.cxx)
target_link_libraries(
  ComplexImageReadWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ImageReadExportVTK ImageReadExportVTK.cxx)
target_link_libraries(
  ImageReadExportVTK
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOVTKModule
)

add_executable(RGBImageSeriesReadWrite RGBImageSeriesReadWrite.cxx)
target_link_libraries(
  RGBImageSeriesReadWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOPNGModule
)

add_executable(ImageSeriesReadWrite ImageSeriesReadWrite.cxx)
target_link_libraries(
  ImageSeriesReadWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOPNGModule
)

add_executable(ImageSeriesReadWrite2 ImageSeriesReadWrite2.cxx)
target_link_libraries(
  ImageSeriesReadWrite2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOPNGModule
)

add_executable(ImageReadImageSeriesWrite ImageReadImageSeriesWrite.cxx)
target_link_libraries(
  ImageReadImageSeriesWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(DicomSeriesReadImageWrite2 DicomSeriesReadImageWrite2.cxx)
target_link_libraries(
  DicomSeriesReadImageWrite2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOGDCMModule
)

add_executable(
  DicomSeriesReadGaussianImageWrite
  DicomSeriesReadGaussianImageWrite.cxx
)
target_link_libraries(
  DicomSeriesReadGaussianImageWrite
  PRIVATE
    ITKImageIO
    ITKIOGDCMModule
    ITKSmoothingModule
)

add_executable(DicomSeriesReadPrintTags DicomSeriesReadPrintTags.cxx)
target_link_libraries(
  DicomSeriesReadPrintTags
  PRIVATE
    ITKImageIO
    ITKIOGDCMModule
)

add_executable(DicomImageReadPrintTags DicomImageReadPrintTags.cxx)
target_link_libraries(
  DicomImageReadPrintTags
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOGDCMModule
    ITKGDCMModule
)

add_executable(DicomImageReadWrite DicomImageReadWrite.cxx)
target_link_libraries(
  DicomImageReadWrite
  PRIVATE
    ITKImageIO
    ITKIOGDCMModule
    ITKImageIntensityModule
)

add_executable(ImageReadDicomSeriesWrite ImageReadDicomSeriesWrite.cxx)
target_link_libraries(
  ImageReadDicomSeriesWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOGDCMModule
)

add_executable(
  DicomImageReadChangeHeaderWrite
  DicomImageReadChangeHeaderWrite.cxx
)
target_link_libraries(
  DicomImageReadChangeHeaderWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOGDCMModule
)

add_executable(DicomSeriesReadSeriesWrite DicomSeriesReadSeriesWrite.cxx)
target_link_libraries(
  DicomSeriesReadSeriesWrite
  PRIVATE
    ITKImageIO
    ITKIOGDCMModule
)

add_executable(DicomPrintPatientInformation DicomPrintPatientInformation.cxx)
target_link_libraries(
  DicomPrintPatientInformation
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIOGDCMModule
)

add_executable(TransformReadWrite TransformReadWrite.cxx)
target_link_libraries(
  TransformReadWrite
  PRIVATE
    ITKTransformIO
    ITKTransformModule
    ITKTransformFactoryModule
)

add_executable(VisibleHumanStreamReadWrite VisibleHumanStreamReadWrite.cxx)
target_link_libraries(
  VisibleHumanStreamReadWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKIORAWModule
    ITKImageComposeModule
    ITKImageGridModule
    ITKSmoothingModule
)

add_executable(VisibleHumanPasteWrite VisibleHumanPasteWrite.cxx)
target_link_libraries(
  VisibleHumanPasteWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageAdaptorsModule
    ITKImageComposeModule
    ITKImageGradientModule
)

add_executable(CovariantVectorImageWrite CovariantVectorImageWrite.cxx)
target_link_libraries(
  CovariantVectorImageWrite
  PRIVATE
    ITKImageIO
    ITKImageGradientModule
)

add_executable(ImageReadExtractWrite ImageReadExtractWrite.cxx)
target_link_libraries(
  ImageReadExtractWrite
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(IOPlugin IOPlugin.cxx)
target_link_libraries(IOPlugin PRIVATE ITKImageIO)

add_executable(IOFactoryRegistration IOFactoryRegistration.cxx)
target_link_libraries(IOFactoryRegistration PRIVATE ITKImageIO)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()

add_subdirectory(XML)
