project(ImageExamples)

add_executable(Image1 Image1.cxx)
target_link_libraries(Image1 PRIVATE ITK::ITKCommonModule)

add_executable(Image2 Image2.cxx)
target_link_libraries(
  Image2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(Image3 Image3.cxx)
target_link_libraries(Image3 PRIVATE ITK::ITKCommonModule)

add_executable(Image4 Image4.cxx)
target_link_libraries(Image4 PRIVATE ITK::ITKCommonModule)

add_executable(VectorImage VectorImage.cxx)
target_link_libraries(VectorImage PRIVATE ITK::ITKCommonModule)

add_executable(ImageAdaptor2 ImageAdaptor2.cxx)
target_link_libraries(
  ImageAdaptor2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageAdaptorsModule
    ITK::ITKImageIntensityModule
)

add_executable(ImageAdaptor3 ImageAdaptor3.cxx)
target_link_libraries(
  ImageAdaptor3
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageAdaptorsModule
    ITK::ITKImageGradientModule
    ITK::ITKImageIntensityModule
)

add_executable(ImageAdaptor4 ImageAdaptor4.cxx)
target_link_libraries(
  ImageAdaptor4
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKImageAdaptorsModule
    ITK::ITKImageIntensityModule
)

add_executable(Image5 Image5.cxx)
target_link_libraries(
  Image5
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

add_executable(ImageAdaptor1 ImageAdaptor1.cxx)
target_link_libraries(
  ImageAdaptor1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageAdaptorsModule
)

add_executable(RGBImage RGBImage.cxx)
target_link_libraries(
  RGBImage
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
