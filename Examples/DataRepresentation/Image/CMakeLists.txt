project(ImageExamples)
include_regular_expression("^.*$")

set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

add_executable(Image1 Image1.cxx )
target_link_libraries(Image1 ITKCommon)

add_executable(Image2 Image2.cxx )
target_link_libraries(Image2 ITKIO)

add_executable(Image3 Image3.cxx )
target_link_libraries(Image3 ITKCommon)

add_executable(Image4 Image4.cxx )
target_link_libraries(Image4 ITKCommon)

add_executable(VectorImage VectorImage.cxx )
target_link_libraries(VectorImage ITKCommon)

add_executable(ImageAdaptor2 ImageAdaptor2.cxx )
target_link_libraries(ImageAdaptor2 ITKIO)

add_executable(ImageAdaptor3 ImageAdaptor3.cxx )
target_link_libraries(ImageAdaptor3 ITKIO)

add_executable(ImageAdaptor4 ImageAdaptor4.cxx )
target_link_libraries(ImageAdaptor4 ITKIO)

add_executable(ImageExamples2 ImageExamples2.cxx )
target_link_libraries(ImageExamples2 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters)

add_executable(ImageExamples3 ImageExamples3.cxx )
target_link_libraries(ImageExamples3 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters)

add_executable(Image5 Image5.cxx )
target_link_libraries(Image5 ITKIO)

add_executable(ImageAdaptor1 ImageAdaptor1.cxx )
target_link_libraries(ImageAdaptor1 ITKIO)

add_executable(RGBImage RGBImage.cxx )
target_link_libraries(RGBImage ITKIO)

add_executable(ImageExamples ImageExamples.cxx )
target_link_libraries(ImageExamples ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters)

add_executable(ImageExamples4 ImageExamples4.cxx )
target_link_libraries(ImageExamples4 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters)


set(IMAGE_EXAMPLES ${CXX_TEST_PATH}/ImageExamples)
set(IMAGE_EXAMPLES2 ${CXX_TEST_PATH}/ImageExamples2)
set(IMAGE_EXAMPLES3 ${CXX_TEST_PATH}/ImageExamples3)
set(IMAGE_EXAMPLES4 ${CXX_TEST_PATH}/ImageExamples4)

if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)
add_test(ImageAdaptor2Test ${IMAGE_EXAMPLES3}
  ImageAdaptor2Test
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor2Test.png
)

add_test(ImageAdaptor3Test ${IMAGE_EXAMPLES3}
  ImageAdaptor3Test
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor3Test.mhd 1
)

add_test(ImageAdaptor4Test ${IMAGE_EXAMPLES3}
  ImageAdaptor4Test
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor4Test.png 127
)

add_test(ImageAdaptor1Test ${IMAGE_EXAMPLES2}
  ImageAdaptor1Test ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png
  )

add_test(Image1Test ${IMAGE_EXAMPLES}
  Image1Test
)

add_test(Image2Test ${IMAGE_EXAMPLES}
  Image2Test ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png
)

add_test(Image3Test ${IMAGE_EXAMPLES}
  Image3Test
)

add_test(Image4Test ${IMAGE_EXAMPLES}
  Image4Test
)

add_test(Image5Test ${IMAGE_EXAMPLES}
   Image5Test ${TEMP}/Image5Test.mhd
)

add_test(RGBImageTest ${IMAGE_EXAMPLES4}
  RGBImageTest ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
)

add_test(VectorImageTest ${IMAGE_EXAMPLES4}
  VectorImageTest
)

endif(BUILD_TESTING)
endif( NOT ITK_DISABLE_CXX_TESTING )

