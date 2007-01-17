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
  itkQuadEdgeMeshAddFaceTest1.cxx
  itkQuadEdgeMeshAddFaceTest2.cxx
  itkQuadEdgeMeshBasicLayerTest.cxx
  itkQuadEdgeMeshDeleteEdgeTest.cxx
  itkQuadEdgeMeshFrontIteratorTest.cxx
  #  OppositeOrientationTest.cxx
  itkQuadEdgeMeshPointTest1.cxx
  itkQuadEdgeMeshTest1.cxx
  itkQuadEdgeMeshTest2.cxx
  itkQuadEdgeMeshTest3.cxx
  itkContourExtractor2DImageFilterTest.cxx
  itkSumProjectionImageFilterTest.cxx
  itkMaximumProjectionImageFilterTest.cxx
  itkMaximumProjectionImageFilterTest2.cxx
  itkMaximumProjectionImageFilterTest3.cxx
  itkMeanProjectionImageFilterTest.cxx
  itkMedianProjectionImageFilterTest.cxx
  itkMinimumProjectionImageFilterTest.cxx
  itkSigmaProjectionImageFilterTest.cxx  
  itkBinaryProjectionImageFilterTest.cxx  
)

ADD_TEST(itkChiSquareDistributionTest ${REVIEW_TESTS}
  itkChiSquareDistributionTest
  )
ADD_TEST(itkFlatStructuringElementTest ${REVIEW_TESTS}
  itkFlatStructuringElementTest
  )
ADD_TEST(itkGaussianDistributionTest ${REVIEW_TESTS}
  itkGaussianDistributionTest
  )
ADD_TEST(itkTDistributionTest ${REVIEW_TESTS} itkTDistributionTest)
ADD_TEST(itkQuadEdgeMeshAddFaceTest1 ${REVIEW_TESTS} itkQuadEdgeMeshAddFaceTest1)
ADD_TEST(itkQuadEdgeMeshAddFaceTest2 ${REVIEW_TESTS} itkQuadEdgeMeshAddFaceTest2)
ADD_TEST(itkQuadEdgeMeshBasicLayerTest ${REVIEW_TESTS} itkQuadEdgeMeshBasicLayerTest)
ADD_TEST(itkQuadEdgeMeshDeleteEdgeTest ${REVIEW_TESTS} itkQuadEdgeMeshDeleteEdgeTest)
ADD_TEST(itkQuadEdgeMeshFrontIteratorTest ${REVIEW_TESTS} itkQuadEdgeMeshFrontIteratorTest)
#ADD_TEST(OppositeOrientationTest ${REVIEW_TESTS} OppositeOrientationTest)
ADD_TEST(itkQuadEdgeMeshPointTest1 ${REVIEW_TESTS} itkQuadEdgeMeshPointTest1)
ADD_TEST(itkQuadEdgeMeshTest1 ${REVIEW_TESTS} itkQuadEdgeMeshTest1)
ADD_TEST(itkQuadEdgeMeshTest2 ${REVIEW_TESTS} itkQuadEdgeMeshTest2)
ADD_TEST(itkQuadEdgeMeshTest3 ${REVIEW_TESTS} itkQuadEdgeMeshTest3)

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

ADD_TEST(itkContourExtractor2DImageFilterTest ${REVIEW_TESTS}
                                  itkContourExtractor2DImageFilterTest
                                  ${INPUTDATA}/ContourExtractor2DTest.tif  )

IF(PROJECTION_TESTS_FIXED)

ADD_TEST(itkMaximumProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsMaximumProjection.tif
                  ${TEMP}/EScellsMaximumProjection.tif
                  itkMaximumProjectionImageFilterTest
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsMaximumProjection.tif)

ADD_TEST(itkMaximumProjectionImageFilterTest2_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsMaximumProjection0.tif
                  ${TEMP}/EScellsMaximumProjection0.tif
                  itkMaximumProjectionImageFilterTest2
                  0
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsMaximumProjection0.tif)

ADD_TEST(itkMaximumProjectionImageFilterTest2_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsMaximumProjection1.tif
                  ${TEMP}/EScellsMaximumProjection1.tif
                  itkMaximumProjectionImageFilterTest2
                  1
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsMaximumProjection1.tif)

ADD_TEST(itkMaximumProjectionImageFilterTest3_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsMaximumProjection2D0.tif
                  ${TEMP}/EScellsMaximumProjection2D0.tif
                  itkMaximumProjectionImageFilterTest3
                  0
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsMaximumProjection2D0.tif)

ADD_TEST(itkMaximumProjectionImageFilterTest3_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsMaximumProjection2D1.tif
                  ${TEMP}/EScellsMaximumProjection2D1.tif
                  itkMaximumProjectionImageFilterTest3
                  1
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsMaximumProjection2D1.tif)

ADD_TEST(itkMinimumProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsMinimumProjection.tif
                  ${TEMP}/EScellsMinimumProjection.tif
                  itkMinimumProjectionImageFilterTest
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsMinimumProjection.tif)

ADD_TEST(itkMedianProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsMedianProjection.tif
                  ${TEMP}/EScellsMedianProjection.tif
                  itkMedianProjectionImageFilterTest
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsMedianProjection.tif)

ADD_TEST(itkMeanProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsMeanProjection.tif
                  ${TEMP}/EScellsMeanProjection.tif
                  itkMeanProjectionImageFilterTest
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsMeanProjection.tif)

ADD_TEST(itkSigmaProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsSigmaProjection.tif
                  ${TEMP}/EScellsSigmaProjection.tif
                  itkSigmaProjectionImageFilterTest
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/EScellsSigmaProjection.tif)

ADD_TEST(itkSumProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsSumProjection.tif
                  ${TEMP}/ESCellsSumProjection.tif
                  itkSumProjectionImageFilterTest
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/ESCellsSumProjection.tif)

ADD_TEST(itkBinaryProjectionImageFilterTest1 ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsBinaryProjection100.tif
                  ${TEMP}/ESCellsBinaryProjection100.tif
                  itkBinaryProjectionImageFilterTest
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/ESCellsBinaryProjection100.tif
                  100
                  255)

ADD_TEST(itkBinaryProjectionImageFilterTest2 ${REVIEW_TESTS}
        --compare ${BASELINE}/ESCellsBinaryProjection200.tif
                  ${TEMP}/ESCellsBinaryProjection200.tif
                  itkBinaryProjectionImageFilterTest
                  ${INPUTDATA}/ESCells.hdr
                  ${TEMP}/ESCellsBinaryProjection200.tif
                  200
                  0)

ENDIF(PROJECTION_TESTS_FIXED)

ADD_EXECUTABLE(itkReviewPrintTest itkReviewPrintTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewPrintTest
  ITKAlgorithms
  ITKIO
  ITKBasicFilters
  ITKFEM
  ITKQuadEdgeMesh
  )

ADD_EXECUTABLE(itkReviewHeaderTest itkReviewHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewHeaderTest
  ITKAlgorithms
  ITKIO
  ITKBasicFilters
  ITKFEM
  ITKQuadEdgeMesh
  )

ADD_TEST(itkReviewHeaderTest ${REVIEW_HEADER_TESTS})

ADD_EXECUTABLE(itkReviewTests itkReviewTests.cxx ${ReviewTest_SRCS})

TARGET_LINK_LIBRARIES(itkReviewTests
  ITKAlgorithms
  ITKIO
  ITKBasicFilters
  ITKFEM
  ITKStatistics
  ITKDistributions
  ITKQuadEdgeMesh
  )

ENDIF(NOT ITK_DISABLE_CXX_TESTING)
