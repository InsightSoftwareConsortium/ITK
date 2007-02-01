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
  itkQuadEdgeTest1.cxx
  itkGeometricalQuadEdgeTest1.cxx
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
  itkStandardDeviationProjectionImageFilterTest.cxx  
  itkBinaryProjectionImageFilterTest.cxx  
  itkBinaryThresholdProjectionImageFilterTest.cxx  
  itkProjectionImageFilterTest.cxx  

  itkValuedRegionalMinimaImageFilterTest.cxx
  itkValuedRegionalMaximaImageFilterTest.cxx
  itkRegionalMaximaImageFilterTest.cxx
  itkRegionalMaximaImageFilterTest2.cxx
  itkRegionalMinimaImageFilterTest.cxx
  itkRegionalMinimaImageFilterTest2.cxx

  itkNeuralNetworkIOTest.cxx

  itkConformalFlatteningMeshFilterTest.cxx

  itkVTKPolyDataReaderTest.cxx
  itkVTKPolyDataWriterTest.cxx

  itkMorphologicalWatershedImageFilterTest.cxx
  itkMorphologicalWatershedFromMarkersImageFilterTest.cxx
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
ADD_TEST(itkQuadEdgeTest1 ${REVIEW_TESTS} itkQuadEdgeTest1)
ADD_TEST(itkGeometricalQuadEdgeTest1 ${REVIEW_TESTS} itkGeometricalQuadEdgeTest1)
ADD_TEST(itkQuadEdgeMeshPointTest1 ${REVIEW_TESTS} itkQuadEdgeMeshPointTest1)

#Temporarily disabled ADD_TEST(itkQuadEdgeMeshAddFaceTest1 ${REVIEW_TESTS} itkQuadEdgeMeshAddFaceTest1)
#Temporarily disabled ADD_TEST(itkQuadEdgeMeshAddFaceTest2 ${REVIEW_TESTS} itkQuadEdgeMeshAddFaceTest2)
#Temporarily disabled ADD_TEST(itkQuadEdgeMeshBasicLayerTest ${REVIEW_TESTS} itkQuadEdgeMeshBasicLayerTest)
#Temporarily disabled ADD_TEST(itkQuadEdgeMeshDeleteEdgeTest ${REVIEW_TESTS} itkQuadEdgeMeshDeleteEdgeTest)
#Temporarily disabled ADD_TEST(itkQuadEdgeMeshFrontIteratorTest ${REVIEW_TESTS} itkQuadEdgeMeshFrontIteratorTest)
#Temporarily disabled ADD_TEST(itkQuadEdgeMeshTest1 ${REVIEW_TESTS} itkQuadEdgeMeshTest1)
#Temporarily disabled ADD_TEST(itkQuadEdgeMeshTest2 ${REVIEW_TESTS} itkQuadEdgeMeshTest2)
#Temporarily disabled ADD_TEST(itkQuadEdgeMeshTest3 ${REVIEW_TESTS} itkQuadEdgeMeshTest3)

#ADD_TEST(OppositeOrientationTest ${REVIEW_TESTS} OppositeOrientationTest)

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


ADD_TEST(itkMaximumProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection.tif
                  itkMaximumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection.tif)

ADD_TEST(itkMaximumProjectionImageFilterTest2_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection0.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection0.tif
                  itkMaximumProjectionImageFilterTest2
                  0
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection0.tif)

ADD_TEST(itkMaximumProjectionImageFilterTest2_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection1.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection1.tif
                  itkMaximumProjectionImageFilterTest2
                  1
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection1.tif)

ADD_TEST(itkMaximumProjectionImageFilterTest3_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection2D0.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection2D0.tif
                  itkMaximumProjectionImageFilterTest3
                  0
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection2D0.tif)

ADD_TEST(itkMaximumProjectionImageFilterTest3_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection2D1.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection2D1.tif
                  itkMaximumProjectionImageFilterTest3
                  1
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection2D1.tif)

ADD_TEST(itkMinimumProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeMinimumProjection.tif
                  ${TEMP}/HeadMRVolumeMinimumProjection.tif
                  itkMinimumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMinimumProjection.tif)

ADD_TEST(itkMedianProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeMedianProjection.tif
                  ${TEMP}/HeadMRVolumeMedianProjection.tif
                  itkMedianProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMedianProjection.tif)

ADD_TEST(itkMeanProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeMeanProjection.tif
                  ${TEMP}/HeadMRVolumeMeanProjection.tif
                  itkMeanProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMeanProjection.tif)

ADD_TEST(itkStandardDeviationProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeStandardDeviationProjection.tif
                  ${TEMP}/HeadMRVolumeStandardDeviationProjection.tif
                  itkStandardDeviationProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeStandardDeviationProjection.tif)

ADD_TEST(itkSumProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeSumProjection.tif
                  ${TEMP}/HeadMRVolumeSumProjection.tif
                  itkSumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeSumProjection.tif)

ADD_TEST(itkProjectionImageFilterTest1 ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection100.tif
                  ${TEMP}/HeadMRVolumeProjection100.tif
                  itkProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeProjection100.tif
                  100
                  0)

ADD_TEST(itkBinaryProjectionImageFilterTest1 ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection100.tif
                  ${TEMP}/HeadMRVolumeBinaryProjection100.tif
                  itkBinaryProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryProjection100.tif
                  100
                  0)

ADD_TEST(itkBinaryProjectionImageFilterTest2 ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection200.tif
                  ${TEMP}/HeadMRVolumeBinaryProjection200.tif
                  itkBinaryProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryProjection200.tif
                  200
                  0)

ADD_TEST(itkBinaryThresholdProjectionImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/HeadMRVolumeBinaryThresholdProjection.png
                  ${TEMP}/HeadMRVolumeBinaryThresholdProjection.png
                  itkBinaryThresholdProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryThresholdProjection.png
                  100
                  255
                  0)


ADD_TEST(itkNeuralNetworkIOTest ${REVIEW_TESTS}
                  itkNeuralNetworkIOTest
                  ${INPUTDATA}/xornet.txt
                  ${INPUTDATA}/xortest.txt
                  )

ADD_TEST(itkConformalFlatteningMeshFilterTest1 ${REVIEW_TESTS}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToSphere.vtk
                 319
                 100.0
                 1
                 )

ADD_TEST(itkConformalFlatteningMeshFilterTest2 ${REVIEW_TESTS}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToPlane.vtk
                 319
                 100.0
                 0
                 )

ADD_TEST(itkValuedRegionalMinimaImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMinimal-ref.png
                  ${TEMP}/cthead1ValuedRegionalMinimal.png
                  itkValuedRegionalMinimaImageFilterTest
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMinimal.png
                  ${TEMP}/cthead1ValuedRegionalMinimal-ref.png
                  )

ADD_TEST(itkValuedRegionalMinimaImageFilterTest2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMinimal-ref2.png
                  ${TEMP}/cthead1ValuedRegionalMinimal2.png
                  itkValuedRegionalMinimaImageFilterTest
                  0
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMinimal2.png
                  ${TEMP}/cthead1ValuedRegionalMinimal-ref2.png
                  )

ADD_TEST(itkValuedRegionalMaximaImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMaximal-ref.png
                  ${TEMP}/cthead1ValuedRegionalMaximal.png
                  itkValuedRegionalMaximaImageFilterTest
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMaximal.png
                  ${TEMP}/cthead1ValuedRegionalMaximal-ref.png
                  )

ADD_TEST(itkValuedRegionalMaximaImageFilterTest2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMaximal-ref2.png
                  ${TEMP}/cthead1ValuedRegionalMaximal2.png
                  itkValuedRegionalMaximaImageFilterTest
                  0
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMaximal2.png
                  ${TEMP}/cthead1ValuedRegionalMaximal-ref2.png
                  )

ADD_TEST(itkRegionalMinimaImageFilterTest2_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMinimal-ref2_1.png
                  ${TEMP}/cthead1RegionalMinimal2_1.png
                  itkRegionalMaximaImageFilterTest2
                  1
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMinimal2_1.png
                  ${TEMP}/cthead1RegionalMinimal-ref2_1.png
                  )
ADD_TEST(itkRegionalMinimaImageFilterTest2_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMinimal-ref2_2.png
                  ${TEMP}/cthead1RegionalMinimal2_2.png
                  itkRegionalMaximaImageFilterTest2
                  0
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMinimal2_2.png
                  ${TEMP}/cthead1RegionalMinimal-ref2_2.png
                  )

ADD_TEST(itkRegionalMaximaImageFilterTest2_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMaximal-ref2_1.png
                  ${TEMP}/cthead1RegionalMaximal2_1.png
                  itkRegionalMaximaImageFilterTest2
                  1
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMaximal2_1.png
                  ${TEMP}/cthead1RegionalMaximal-ref2_1.png
                  )
ADD_TEST(itkRegionalMaximaImageFilterTest2_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMaximal-ref2_2.png
                  ${TEMP}/cthead1RegionalMaximal2_2.png
                  itkRegionalMaximaImageFilterTest2
                  0
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMaximal2_2.png
                  ${TEMP}/cthead1RegionalMaximal-ref2_2.png
                  )

ADD_TEST(itkVTKPolyDataReaderTest ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  )

ADD_TEST(itkVTKPolyDataWriterTest01 ${REVIEW_TESTS}
  itkVTKPolyDataWriterTest
  ${TEMP}/VTKPolyDataWriterTest01.vtk
  )

FOREACH(M 0 1)
  FOREACH(F 0 1)
    ADD_TEST(itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/button-hole.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      ${M} ${F} 0
                      )
    
    ADD_TEST(itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/pass-values.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      ${M} ${F} 0
                      )
    
    ADD_TEST(itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/plateau.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      ${M} ${F} 0
                      )
    
    ADD_TEST(itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/thick-lines.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      ${M} ${F} 0
                      )
    
    ADD_TEST(itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      itkMorphologicalWatershedFromMarkersImageFilterTest
                      ${INPUTDATA}/cthead1.png
                      ${INPUTDATA}/cthead1-markers.png
                      ${TEMP}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      ${M} ${F}
                      )
  ENDFOREACH(F)
ENDFOREACH(M)

FOREACH(level 00 10 20 30 40 50)
  ADD_TEST(itkMorphologicalWatershedImageFilterTestLevel${level} ${REVIEW_TESTS}
          --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    ${TEMP}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    itkMorphologicalWatershedImageFilterTest
                    ${INPUTDATA}/level.png
                    ${TEMP}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    1 0 ${level}
                    )
ENDFOREACH(level)


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
