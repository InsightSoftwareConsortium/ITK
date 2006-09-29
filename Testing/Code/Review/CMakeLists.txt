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

SET(ReviewTest_SRCS
  itkChiSquareDistributionTest.cxx
  itkFlatStructuringElementTest.cxx
  itkGaussianDistributionTest.cxx
  itkTDistributionTest.cxx
)

ADD_TEST(itkChiSquareDistributionTest ${REVIEW_TESTS} itkChiSquareDistributionTest)
ADD_TEST(itkFlatStructuringElementTest ${REVIEW_TESTS} itkFlatStructuringElementTest)
ADD_TEST(itkGaussianDistributionTest ${REVIEW_TESTS} itkGaussianDistributionTest)
ADD_TEST(itkTDistributionTest ${REVIEW_TESTS} itkTDistributionTest)


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
                      ITKBasicFilters ITKFEM ITKStatistics ITKDistributions)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )

