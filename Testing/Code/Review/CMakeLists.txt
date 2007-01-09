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
  itkLabelOverlayImageFilterTest.cxx
)

ADD_TEST(itkChiSquareDistributionTest ${REVIEW_TESTS} itkChiSquareDistributionTest)
ADD_TEST(itkFlatStructuringElementTest ${REVIEW_TESTS} itkFlatStructuringElementTest)
ADD_TEST(itkGaussianDistributionTest ${REVIEW_TESTS} itkGaussianDistributionTest)
ADD_TEST(itkTDistributionTest ${REVIEW_TESTS} itkTDistributionTest)


FOREACH(opacity 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
  STRING(REGEX REPLACE "\\." "_" op ${opacity})
  ADD_TEST(itkLabelOverlayImageFilterTest-Opacity-${op} ${REVIEW_TESTS} 
        --compare ${BASELINE}/cthead1-${op}Overlay.png
                  ${TEMP}/cthead1-${op}Overlay.png
                  itkLabelOverlayImageFilterTest
                  ${INPUTDATA}/cthead1.png
                  ${INPUTDATA}/cthead1Label.png 
                  ${opacity} 
                  ${TEMP}/cthead1-${op}Overlay.png)
ENDFOREACH(opacity)

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

