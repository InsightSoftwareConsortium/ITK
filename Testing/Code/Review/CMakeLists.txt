IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Review ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Review)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)


SET(REVIEW_TESTS ${CXX_TEST_PATH}/itkReviewTests)

SET(REVIEW_PRINT_TEST ${CXX_TEST_PATH}/itkReviewPrintTest)

SET(REVIEW_HEADER_TESTS ${CXX_TEST_PATH}/itkReviewHeaderTest)

ADD_TEST(itkReviewPrintTest ${REVIEW_PRINT_TEST})

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Review)
SET(INPUTDATA ${ITK_DATA_ROOT}/Input)
SET(TEMP ${ITKTesting_BINARY_DIR}/Temporary)


#
#  List of Tests in this directory
#

ADD_TEST( itkThresholdMaximumConnectedComponentsImageFilterTest1 
          ${REVIEW_TESTS} 
  --compare ${BASELINE}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
            ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png
          itkThresholdMaximumConnectedComponentsImageFilterTest 
          ${INPUTDATA}/CellsFluorescence1.png
          ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest1.png 10)

ADD_TEST( itkThresholdMaximumConnectedComponentsImageFilterTest2 
          ${REVIEW_TESTS} 
  --compare ${BASELINE}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
            ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png
          itkThresholdMaximumConnectedComponentsImageFilterTest 
          ${INPUTDATA}/CellsFluorescence2.png
          ${TEMP}/itkThresholdMaximumConnectedComponentsImageFilterTest2.png 10)

ADD_TEST( itkSignedMaurerDistanceMapImageFilterTest1
          ${REVIEW_TESTS} 
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/SquareBinary201.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest1.mhd)

ADD_TEST( itkSignedMaurerDistanceMapImageFilterTest2
          ${REVIEW_TESTS} 
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/BrainSliceBinary.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest2.mhd)

ADD_TEST( itkSignedMaurerDistanceMapImageFilterTest3
          ${REVIEW_TESTS} 
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/LungSliceBinary.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest3.mhd)

SET(ReviewTest_SRCS
  itkSignedMaurerDistanceMapImageFilterTest.cxx
  itkThresholdMaximumConnectedComponentsImageFilterTest.cxx
)

ADD_EXECUTABLE(itkReviewPrintTest itkReviewPrintTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewPrintTest ITKAlgorithms ITKIO 
                                              ITKBasicFilters ITKFEM)

ADD_EXECUTABLE(itkReviewHeaderTest itkReviewHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewHeaderTest ITKAlgorithms ITKIO 
                                              ITKBasicFilters ITKFEM)

ADD_TEST(itkReviewHeaderTest ${REVIEW_HEADER_TESTS})

ADD_EXECUTABLE(itkReviewTests itkReviewTests.cxx ${ReviewTest_SRCS})

TARGET_LINK_LIBRARIES(itkReviewTests 
                      ITKAlgorithms ITKIO 
                      ITKBasicFilters ITKFEM ITKStatistics)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )

