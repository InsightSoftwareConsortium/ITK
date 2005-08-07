PROJECT(ImageExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_EXECUTABLE(Image1 Image1.cxx )
TARGET_LINK_LIBRARIES(Image1 ITKCommon)

ADD_EXECUTABLE(Image2 Image2.cxx )
TARGET_LINK_LIBRARIES(Image2 ITKCommon ITKIO)

ADD_EXECUTABLE(Image3 Image3.cxx )
TARGET_LINK_LIBRARIES(Image3 ITKCommon)

ADD_EXECUTABLE(Image4 Image4.cxx )
TARGET_LINK_LIBRARIES(Image4 ITKCommon)

ADD_EXECUTABLE(Image5 Image5.cxx )
TARGET_LINK_LIBRARIES(Image5 ITKCommon ITKIO)

ADD_EXECUTABLE(RGBImage RGBImage.cxx )
TARGET_LINK_LIBRARIES(RGBImage ITKCommon ITKIO) 

ADD_EXECUTABLE(VectorImage VectorImage.cxx )
TARGET_LINK_LIBRARIES(VectorImage ITKCommon)

ADD_EXECUTABLE(ImageAdaptor1 ImageAdaptor1.cxx )
TARGET_LINK_LIBRARIES(ImageAdaptor1 ITKCommon ITKIO)

ADD_EXECUTABLE(ImageAdaptor2 ImageAdaptor2.cxx )
TARGET_LINK_LIBRARIES(ImageAdaptor2 ITKCommon ITKIO)

ADD_EXECUTABLE(ImageAdaptor3 ImageAdaptor3.cxx )
TARGET_LINK_LIBRARIES(ImageAdaptor3 ITKCommon ITKIO)

ADD_EXECUTABLE(ImageAdaptor4 ImageAdaptor4.cxx )
TARGET_LINK_LIBRARIES(ImageAdaptor4 ITKCommon ITKIO)

SET(IMAGE_EXAMPLES ${CXX_TEST_PATH}/ImageExamples)
SET(IMAGE_EXAMPLES2 ${CXX_TEST_PATH}/ImageExamples2)
SET(IMAGE_EXAMPLES3 ${CXX_TEST_PATH}/ImageExamples3)
SET(IMAGE_EXAMPLES4 ${CXX_TEST_PATH}/ImageExamples4)

IF( NOT ITK_DISABLE_CXX_TESTING )

ADD_TEST(Image1Test ${IMAGE_EXAMPLES}
  Image1Test
)

ADD_TEST(Image2Test ${IMAGE_EXAMPLES}
  Image2Test ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png

)

ADD_TEST(Image3Test ${IMAGE_EXAMPLES}
  Image3Test
)

ADD_TEST(Image4Test ${IMAGE_EXAMPLES}
  Image4Test
)

ADD_TEST(Image5Test ${IMAGE_EXAMPLES}
  Image5Test ${TEMP}/Image5Test.mhd
)

ADD_TEST(ImageAdaptor2Test ${IMAGE_EXAMPLES3}
  ImageAdaptor2Test
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor2Test.png
)

ADD_TEST(ImageAdaptor3Test ${IMAGE_EXAMPLES3}
  ImageAdaptor3Test
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor3Test.mhd 1
)

ADD_TEST(ImageAdaptor4Test ${IMAGE_EXAMPLES3}
  ImageAdaptor4Test
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor4Test.png 127
)


ADD_TEST(RGBImageTest ${IMAGE_EXAMPLES4}
  RGBImageTest ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
)

ADD_TEST(VectorImageTest ${IMAGE_EXAMPLES4}
  VectorImageTest
)

ADD_EXECUTABLE(ImageExamples ImageExamples.cxx )
TARGET_LINK_LIBRARIES(ImageExamples ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

IF(NOT BORLAND)
  ADD_EXECUTABLE(ImageExamples2 ImageExamples2.cxx )
  TARGET_LINK_LIBRARIES(ImageExamples2 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

  ADD_TEST(ImageAdaptor1Test ${IMAGE_EXAMPLES2}
    ImageAdaptor1Test ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png
)

ENDIF(NOT BORLAND)

ADD_EXECUTABLE(ImageExamples3 ImageExamples3.cxx )
TARGET_LINK_LIBRARIES(ImageExamples3 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(ImageExamples4 ImageExamples4.cxx )
TARGET_LINK_LIBRARIES(ImageExamples4 ITKAlgorithms ITKFEM ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )

