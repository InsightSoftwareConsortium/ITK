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

INCLUDE_DIRECTORIES(
  # This directory is needed to find the itkFilterWatcher.h
  ${ITK_SOURCE_DIR}/Testing/Code/Algorithms
  )

#
#  List of Tests in this directory
#

SET(ReviewTest_SRCS
  itkFlatStructuringElementTest.cxx

  itkLabelToRGBImageFilterTest.cxx
  itkLabelOverlayImageFilterTest.cxx

  itkQuadEdgeTest1.cxx
  itkGeometricalQuadEdgeTest1.cxx
  itkQuadEdgeMeshAddFaceTest1.cxx
  itkQuadEdgeMeshAddFaceTest2.cxx
  itkQuadEdgeMeshBasicLayerTest.cxx
  itkQuadEdgeMeshDeleteEdgeTest.cxx
  itkQuadEdgeMeshFrontIteratorTest.cxx
  itkQuadEdgeMeshPointTest1.cxx
  itkQuadEdgeMeshTest1.cxx
  itkQuadEdgeMeshTest2.cxx
  itkQuadEdgeMeshTest3.cxx
  itkQuadEdgeMeshEulerOperatorFlipTest.cxx
  itkQuadEdgeMeshEulerOperatorSplitVertexTest.cxx
  itkQuadEdgeMeshEulerOperatorJoinVertexTest.cxx
  itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest.cxx
  itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest.cxx
  itkQuadEdgeMeshEulerOperatorJoinFacetTest.cxx
  itkQuadEdgeMeshEulerOperatorSplitEdgeTest.cxx
  itkQuadEdgeMeshEulerOperatorSplitFaceTest.cxx
  itkQuadEdgeMeshCellInterfaceTest.cxx
  itkQuadEdgeMeshIteratorTest.cxx
  itkQuadEdgeMeshPolygonCellTest.cxx
  itkDynamicQuadEdgeMeshTest.cxx
  itkAutomaticTopologyQuadEdgeMeshSourceTest.cxx
  itkRegularSphereQuadEdgeMeshSourceTest.cxx
  itkQuadEdgeMeshCountingCellsTest.cxx
  itkQuadEdgeMeshDeletePointAndReorderIDsTest.cxx
  itkQuadEdgem_NoPointTest.cxx

  itkBinaryMask3DQuadEdgeMeshSourceTest.cxx
  itkContourExtractor2DImageFilterTest.cxx

  itkValuedRegionalMinimaImageFilterTest.cxx
  itkValuedRegionalMaximaImageFilterTest.cxx
  itkRegionalMaximaImageFilterTest.cxx
  itkRegionalMaximaImageFilterTest2.cxx
  itkRegionalMinimaImageFilterTest.cxx
  itkRegionalMinimaImageFilterTest2.cxx

  itkNeuralNetworkIOTest.cxx

  itkConformalFlatteningMeshFilterTest.cxx
  itkConformalFlatteningQuadEdgeMeshFilterTest.cxx

  itkVTKPolyDataIOQuadEdgeMeshTest.cxx
  itkVTKPolyDataReaderQuadEdgeMeshTest.cxx
  itkVTKPolyDataReaderTest.cxx
  itkVTKPolyDataWriterTest.cxx

  itkMorphologicalWatershedImageFilterTest.cxx
  itkMorphologicalWatershedFromMarkersImageFilterTest.cxx

  itkBinaryMorphologicalClosingImageFilterTest.cxx
  itkBinaryMorphologicalOpeningImageFilterTest.cxx

  itkOptImageToImageMetricsTest.cxx

  itkTimeAndMemoryProbeTest.cxx

  itkBruker2DSEQImageIOTest.cxx
  itkPhilipsRECImageIOTest.cxx

  itkVoxBoCUBImageIOTest.cxx

  itkSliceBySliceImageFilterTest.cxx

  itkDiffeomorphicDemonsRegistrationFilterTest.cxx
  itkDiffeomorphicDemonsRegistrationFilterTest2.cxx

  itkMultiplyByConstantImageFilterTest.cxx
  itkDivideByConstantImageFilterTest.cxx
  itkAddConstantToImageFilterTest.cxx
  itkSubtractConstantFromImageFilterTest.cxx

  itkImageReadComplexWriteMagnitudeAndPhaseTest.cxx
  itkImageReadMagnitudeAndPhaseWriteComplexTest.cxx
  itkImageReadRealAndImaginaryWriteComplexTest.cxx

  itkFFTComplexToComplexImageFilterTest01.cxx
  itkFFTComplexToComplexImageFilterTest02.cxx

  itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest.cxx
  itkExponentialDeformationFieldImageFilterTest.cxx
  itkFastSymmetricForcesDemonsRegistrationFilterTest.cxx
  itkGridForwardWarpImageFilterTest.cxx
  itkWarpJacobianDeterminantFilterTest.cxx
  itkWarpHarmonicEnergyCalculatorTest.cxx

  itkBSplineScatteredDataPointSetToImageFilterTest.cxx
  itkBSplineScatteredDataPointSetToImageFilterTest2.cxx
  itkBSplineScatteredDataPointSetToImageFilterTest3.cxx

  itkKappaSigmaThresholdImageCalculatorTest.cxx
  itkKappaSigmaThresholdImageFilterTest.cxx

  itkComposeRGBAImageFilterTest.cxx
)

IF(ITK_USE_TRANSFORM_IO_FACTORIES)
  SET(ReviewTest_SRCS ${ReviewTest_SRCS} itkTransformFileReaderWriterTest.cxx)

  ADD_TEST(itkTransformFileReaderWriterTest0
    ${REVIEW_TESTS} itkTransformFileReaderWriterTest
    ${INPUTDATA}/itkAffineTransform00.txt
    ${TEMP}/itkTransformFileReaderWriterTest0.txt
    )

  ADD_TEST(itkTransformFileReaderWriterTest1
    ${REVIEW_TESTS} itkTransformFileReaderWriterTest
    ${INPUTDATA}/itkAffineTransform01.txt
    ${TEMP}/itkTransformFileReaderWriterTest1.txt
    )
ENDIF(ITK_USE_TRANSFORM_IO_FACTORIES)


ADD_TEST(itkOptImageToImageMetricsTest01
  ${REVIEW_TESTS} itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

ADD_TEST(itkOptImageToImageMetricsTest02
  ${REVIEW_TESTS}
  --with-threads 2
  itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

ADD_TEST(itkOptImageToImageMetricsTest03
  ${REVIEW_TESTS}
  --without-threads
  itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

IF(ITK_USE_MINC2 AND MINC2_FOUND)
  SET(ReviewTest_SRCS ${ReviewTest_SRCS} itkMINC2ImageIOTest.cxx)
ENDIF(ITK_USE_MINC2 AND MINC2_FOUND)

ADD_TEST(itkFlatStructuringElementTest ${REVIEW_TESTS}
  itkFlatStructuringElementTest
  )
ADD_TEST(itkQuadEdgem_NoPointTest ${REVIEW_TESTS} itkQuadEdgem_NoPointTest)
ADD_TEST(itkQuadEdgeTest1 ${REVIEW_TESTS} itkQuadEdgeTest1)
ADD_TEST(itkGeometricalQuadEdgeTest1 ${REVIEW_TESTS}
  itkGeometricalQuadEdgeTest1)
ADD_TEST(itkQuadEdgeMeshPointTest1 ${REVIEW_TESTS}
  itkQuadEdgeMeshPointTest1)
ADD_TEST(itkQuadEdgeMeshAddFaceTest1 ${REVIEW_TESTS}
  itkQuadEdgeMeshAddFaceTest1)
ADD_TEST(itkQuadEdgeMeshAddFaceTest2 ${REVIEW_TESTS}
  itkQuadEdgeMeshAddFaceTest2)
ADD_TEST(itkQuadEdgeMeshDeleteEdgeTest ${REVIEW_TESTS}
  itkQuadEdgeMeshDeleteEdgeTest)
ADD_TEST(itkQuadEdgeMeshFrontIteratorTest ${REVIEW_TESTS}
  itkQuadEdgeMeshFrontIteratorTest)
ADD_TEST(itkQuadEdgeMeshTest1 ${REVIEW_TESTS} itkQuadEdgeMeshTest1)
ADD_TEST(itkQuadEdgeMeshTest2 ${REVIEW_TESTS} itkQuadEdgeMeshTest2)
ADD_TEST(itkQuadEdgeMeshTest3 ${REVIEW_TESTS} itkQuadEdgeMeshTest3)
ADD_TEST(itkQuadEdgeMeshEulerOperatorFlipTest ${REVIEW_TESTS}
  itkQuadEdgeMeshEulerOperatorFlipTest)
ADD_TEST(itkQuadEdgeMeshEulerOperatorJoinFacetTest ${REVIEW_TESTS} 
  itkQuadEdgeMeshEulerOperatorJoinFacetTest)
ADD_TEST(itkQuadEdgeMeshEulerOperatorJoinVertexTest ${REVIEW_TESTS}
  itkQuadEdgeMeshEulerOperatorJoinVertexTest)
ADD_TEST(itkQuadEdgeMeshEulerOperatorSplitEdgeTest ${REVIEW_TESTS}
  itkQuadEdgeMeshEulerOperatorSplitEdgeTest)
ADD_TEST(itkQuadEdgeMeshEulerOperatorSplitVertexTest ${REVIEW_TESTS}
  itkQuadEdgeMeshEulerOperatorSplitVertexTest ${REVIEW_TESTS})
##ADD_TEST(itkQuadEdgeMeshEulerOperatorsTest ${REVIEW_TESTS}
##  itkQuadEdgeMeshEulerOperatorsTest)
ADD_TEST(itkQuadEdgeMeshCellInterfaceTest ${REVIEW_TESTS}
  itkQuadEdgeMeshCellInterfaceTest)
ADD_TEST(itkQuadEdgeMeshIteratorTest ${REVIEW_TESTS}
  itkQuadEdgeMeshIteratorTest)
ADD_TEST(itkQuadEdgeMeshPolygonCellTest ${REVIEW_TESTS}
  itkQuadEdgeMeshPolygonCellTest)
ADD_TEST(itkDynamicQuadEdgeMeshTest ${REVIEW_TESTS}
  itkDynamicQuadEdgeMeshTest)
ADD_TEST(itkAutomaticTopologyQuadEdgeMeshSourceTest ${REVIEW_TESTS}
  itkAutomaticTopologyQuadEdgeMeshSourceTest)
ADD_TEST(itkRegularSphereQuadEdgeMeshSourceTest ${REVIEW_TESTS}
  itkRegularSphereQuadEdgeMeshSourceTest)
ADD_TEST(itkQuadEdgeMeshCountingCellsTest ${REVIEW_TESTS}
  itkQuadEdgeMeshCountingCellsTest)
ADD_TEST(itkBinaryMask3DQuadEdgeMeshSourceTest ${REVIEW_TESTS}
  itkBinaryMask3DQuadEdgeMeshSourceTest)
ADD_TEST( itkQuadEdgeMeshDeletePointAndReorderIDsTest ${REVIEW_TESTS}
  itkQuadEdgeMeshDeletePointAndReorderIDsTest)
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

ADD_TEST(itkLabelToRGBImageFilterTest ${REVIEW_TESTS}
      --compare ${BASELINE}/cthead1Label-color.png
                ${TEMP}/cthead1Label-color.png
                itkLabelToRGBImageFilterTest
                ${INPUTDATA}/cthead1Label.png
                ${TEMP}/cthead1Label-color.png)

ADD_TEST(itkContourExtractor2DImageFilterTest ${REVIEW_TESTS}
                                  itkContourExtractor2DImageFilterTest
                                  ${INPUTDATA}/ContourExtractor2DTest.tif  )




ADD_TEST(itkNeuralNetworkIOTest ${REVIEW_TESTS}
                  itkNeuralNetworkIOTest
                  ${INPUTDATA}/xornet.txt
                  ${INPUTDATA}/xortest.txt
                  ${TEMP}
                  )

ADD_TEST(itkConformalFlatteningMeshFilterTest1 ${REVIEW_TESTS}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToSphere.vtk
                 1
                 )

ADD_TEST(itkConformalFlatteningQuadEdgeMeshFilterTest1 ${REVIEW_TESTS}
                 itkConformalFlatteningQuadEdgeMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroQuadEdgeMesh01ConformalFlattenedToSphere.vtk
                 3
                 100.0
                 1
                 )

ADD_TEST(itkConformalFlatteningMeshFilterTest2 ${REVIEW_TESTS}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToPlane.vtk
                 0
                 )

ADD_TEST(itkConformalFlatteningQuadEdgeMeshFilterTest2 ${REVIEW_TESTS}
                 itkConformalFlatteningQuadEdgeMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroQuadEdgeMesh01ConformalFlattenedToPlane.vtk
                 3
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

ADD_TEST(itkVTKPolyDataIOQuadEdgeMeshTest ${REVIEW_TESTS}
  itkVTKPolyDataIOQuadEdgeMeshTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  ${TEMP}/VTKPolyDataIOTest.vtk)

ADD_TEST(itkVTKPolyDataReaderQuadEdgeMeshTest ${REVIEW_TESTS}
  itkVTKPolyDataReaderQuadEdgeMeshTest
  ${INPUTDATA}/genusZeroSurface01.vtk
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

ADD_TEST(itkBinaryMorphologicalClosingImageFilterTest$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalClosingImageFilterTest.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTest.png
                  itkBinaryMorphologicalClosingImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTest.png
                  40 1 200
                  )

ADD_TEST(itkBinaryMorphologicalClosingImageFilterTestUnsafe$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  itkBinaryMorphologicalClosingImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  40 0 200
                  )

ADD_TEST(itkBinaryMorphologicalOpeningImageFilterTest$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  ${TEMP}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  itkBinaryMorphologicalOpeningImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  8 150 200
                  )
ADD_TEST(itkTimeAndMemoryProbeTest1 ${REVIEW_TESTS}
        itkTimeAndMemoryProbeTest
        )

ADD_TEST(itkBruker2DSEQImageIOTest1 ${REVIEW_TESTS}
        itkBruker2DSEQImageIOTest
        ${ITK_DATA_ROOT}/Input/DCB120604.t61/11/pdata/1/2dseq
        ${ITK_TEST_OUTPUT_DIR}/itkBruker2DSEQImageIOTest1.mha
        )

ADD_TEST(itkPhilipsRECImageIOTest1 ${REVIEW_TESTS}
        itkPhilipsRECImageIOTest
        ${ITK_DATA_ROOT}/Input/T1_MEASUREMENT_4_1.PAR
        ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOTest1.mha
        )

ADD_TEST(itkVoxBoCUBImageIOTest1 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.cub
        )

ADD_TEST(itkVoxBoCUBImageIOTest2 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub
        )

ADD_TEST(itkVoxBoCUBImageIOTest3 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub.gz
        )

ADD_TEST(itkVoxBoCUBImageIOTest4 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub.gz
        )

ADD_TEST(itkVoxBoCUBImageIOTest5 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub
        )

ADD_TEST(itkVoxBoCUBImageIOTest6 ${REVIEW_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha
        itkVoxBoCUBImageIOTest
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha
        )

ADD_TEST(itkVoxBoCUBImageIOTest7 ${REVIEW_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha
        itkVoxBoCUBImageIOTest
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha
        )

ADD_TEST(itkSliceBySliceImageFilterTest ${REVIEW_TESTS}
        itkSliceBySliceImageFilterTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
        ${ITK_TEST_OUTPUT_DIR}/itkSliceBySliceImageFilterTest.mha
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest01 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  0  0.001  20.0
        200   0.01   0.08   5 10
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest02 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 1  0  0.01  3.0
        400   0.2   0.9   10 70
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest03 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 2  0  0.001  20.0
        200   0.01   0.08   5   10
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest04 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 3  0  0.01  3.0
        200   0.2   0.8   5   70
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest05 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  1  0.01  3.0
        200   0.2   0.08   5  70
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest06 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  0  0.005  20.0
        200   0.01   0.08   5  10
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest07 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest07.mha
        0  0  0.001  0.1
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest08 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest08.mha
        1  0  0.001  0.1
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest09 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest09.mha
        2  0  0.001  0.1
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest10 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest10.mha
        3  0  0.001  0.1
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest11 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest11.mha
        0  1  0.001  0.1
        )

ADD_TEST(itkBSplineScatteredDataPointSetToImageFilterTest01 ${REVIEW_TESTS}
        itkBSplineScatteredDataPointSetToImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cake_easy.png
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
        )

ADD_TEST(itkBSplineScatteredDataPointSetToImageFilterTest02 ${REVIEW_TESTS}
        itkBSplineScatteredDataPointSetToImageFilterTest2
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredApproximationDataPointsTestOutput2.txt
        )

ADD_TEST(itkBSplineScatteredDataPointSetToImageFilterTest03 ${REVIEW_TESTS}
        itkBSplineScatteredDataPointSetToImageFilterTest3
        ${ITK_SOURCE_DIR}/Testing/Data/Input/BSplineScatteredApproximationDataPointsInput.txt
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredApproximationDataPointsTestOutput3.txt
        )

ADD_TEST(itkKappaSigmaThresholdImageCalculatorTest01 ${REVIEW_TESTS}
        itkKappaSigmaThresholdImageCalculatorTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence1.png
        10 3.5 19
        )

ADD_TEST(itkKappaSigmaThresholdImageCalculatorTest02 ${REVIEW_TESTS}
        itkKappaSigmaThresholdImageCalculatorTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence2.png
        10 3.0 19
        )

ADD_TEST(itkKappaSigmaThresholdImageFilterTest01 ${REVIEW_TESTS}
        itkKappaSigmaThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence1.png
        ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest01.mha
        10 3.5
        )

ADD_TEST(itkKappaSigmaThresholdImageFilterTest02 ${REVIEW_TESTS}
        itkKappaSigmaThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence2.png
        ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest02.mha
        10 3.0
        )

ADD_TEST(itkMultiplyByConstantImageFilterTest ${REVIEW_TESTS}
        itkMultiplyByConstantImageFilterTest
        )

ADD_TEST(itkDivideByConstantImageFilterTest ${REVIEW_TESTS}
        itkDivideByConstantImageFilterTest
        )

ADD_TEST(itkAddConstantToImageFilterTest ${REVIEW_TESTS}
        itkAddConstantToImageFilterTest
        )

ADD_TEST(itkSubtractConstantFromImageFilterTest ${REVIEW_TESTS}
        itkSubtractConstantFromImageFilterTest
        )

ADD_TEST(itkImageReadRealAndImaginaryWriteComplexTest ${REVIEW_TESTS}
        itkImageReadRealAndImaginaryWriteComplexTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha
        )

ADD_TEST(itkImageReadComplexWriteMagnitudeAndPhaseTest ${REVIEW_TESTS}
        itkImageReadComplexWriteMagnitudeAndPhaseTest
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexMagnitude.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexPhase.mha
        )

ADD_TEST(itkImageReadMagnitudeAndPhaseWriteComplexTest ${REVIEW_TESTS}
        itkImageReadMagnitudeAndPhaseWriteComplexTest
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexMagnitude.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexPhase.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha
        )

ADD_TEST(itkComposeRGBAImageFilterTest ${REVIEW_TESTS}
    itkComposeRGBAImageFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkComposeRGBAImageFilterTestOutput.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number1inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number2inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number3inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/IntensityRamp64.png
)
ADD_EXECUTABLE(itkReviewPrintTest itkReviewPrintTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewPrintTest
  ITKAlgorithms
  ITKFEM
  ITKQuadEdgeMesh
  )

ADD_EXECUTABLE(itkReviewHeaderTest itkReviewHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewHeaderTest
  ITKAlgorithms
  ITKFEM
  ITKQuadEdgeMesh
  )

ADD_TEST(itkReviewHeaderTest ${REVIEW_HEADER_TESTS})

ADD_EXECUTABLE(itkReviewTests itkReviewTests.cxx ${ReviewTest_SRCS})

SET(ReviewTest_LIBS
  ITKAlgorithms
  ITKFEM
  ITKStatistics
  ITKQuadEdgeMesh
  ITKIOReview
  ITKIO
  )


IF(ITK_USE_TRANSFORM_IO_FACTORIES)
SET(ReviewTest_LIBS ${ReviewTest_LIBS}
  ITKTransformIOReview
  )
ENDIF(ITK_USE_TRANSFORM_IO_FACTORIES)


IF(ITK_USE_MINC2 AND MINC2_FOUND)
  SET(ReviewTest_LIBS ${ReviewTest_LIBS} ITKIOMINC2)
ENDIF(ITK_USE_MINC2 AND MINC2_FOUND)


TARGET_LINK_LIBRARIES(itkReviewTests ${ReviewTest_LIBS})

ENDIF(NOT ITK_DISABLE_CXX_TESTING)
