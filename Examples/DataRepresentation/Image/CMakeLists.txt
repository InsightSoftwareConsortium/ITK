project(ImageExamples)

add_executable(Image1 Image1.cxx)
target_link_libraries(Image1 PRIVATE ITKCommonModule)

add_executable(Image2 Image2.cxx)
target_link_libraries(
  Image2
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(Image3 Image3.cxx)
target_link_libraries(Image3 PRIVATE ITKCommonModule)

add_executable(Image4 Image4.cxx)
target_link_libraries(Image4 PRIVATE ITKCommonModule)

add_executable(VectorImage VectorImage.cxx)
target_link_libraries(VectorImage PRIVATE ITKCommonModule)

add_executable(ImageAdaptor2 ImageAdaptor2.cxx)
target_link_libraries(
  ImageAdaptor2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageAdaptorsModule
    ITKImageIntensityModule
)

add_executable(ImageAdaptor3 ImageAdaptor3.cxx)
target_link_libraries(
  ImageAdaptor3
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageAdaptorsModule
    ITKImageGradientModule
    ITKImageIntensityModule
)

add_executable(ImageAdaptor4 ImageAdaptor4.cxx)
target_link_libraries(
  ImageAdaptor4
  PRIVATE
    ITKImageIO
    ITKImageAdaptorsModule
    ITKImageIntensityModule
)

add_executable(Image5 Image5.cxx)
target_link_libraries(
  Image5
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

add_executable(ImageAdaptor1 ImageAdaptor1.cxx)
target_link_libraries(
  ImageAdaptor1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageAdaptorsModule
)

add_executable(RGBImage RGBImage.cxx)
target_link_libraries(
  RGBImage
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
