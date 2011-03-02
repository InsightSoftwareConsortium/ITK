if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-Review ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Review)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


set(REVIEW_PRINT_TEST ${CXX_TEST_PATH}/itkReviewPrintTest)
set(REVIEW_HEADER_TESTS ${CXX_TEST_PATH}/itkReviewHeaderTest)

add_test(itkReviewPrintTest ${REVIEW_PRINT_TEST})

# Define some convenient locations
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Review)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

include_directories(
  # This directory is needed to find the itkFilterWatcher.h
  ${ITK_SOURCE_DIR}/Testing/Code/Algorithms
  # This directory is needed to find the itkTestingMacros.h
  ${ITK_SOURCE_DIR}/Testing/Code/Review
  # This directory is needed for itkJPEG2000ImageIOTest(s)
  ${ITK_SOURCE_DIR}/Utilities/openjpeg
  )

############################################################
# CXX Testing
#
set(KIT Review)
set(KIT_LIBS
  ITKAlgorithms
  ITKFEM
  ITKStatistics
  ITKCommon
  ITKReview
  ITKIOReview
  ITKIO
)
if(ITK_USE_MINC2 AND MINC2_FOUND)
  set(KIT_LIBS ${KIT_LIBS} ITKIOMINC2)
endif(ITK_USE_MINC2 AND MINC2_FOUND)

set(KitTests
  itkFlatStructuringElementTest

  itkLabelToRGBImageFilterTest
  itkLabelOverlayImageFilterTest

  itkRegionalMaximaImageFilterTest
  itkRegionalMaximaImageFilterTest2
  itkRegionalMinimaImageFilterTest
  itkRegionalMinimaImageFilterTest2
  itkValuedRegionalMaximaImageFilterTest
  itkValuedRegionalMinimaImageFilterTest

  itkNeuralNetworkIOTest

  itkConformalFlatteningMeshFilterTest
  itkConformalFlatteningQuadEdgeMeshFilterTest

  itkMorphologicalWatershedImageFilterTest
  itkMorphologicalWatershedFromMarkersImageFilterTest

  itkBinaryMorphologicalClosingImageFilterTest
  itkBinaryMorphologicalOpeningImageFilterTest

  itkOptImageToImageMetricsTest
  itkOptMattesMutualInformationImageToImageMetricThreadsTest1

  itkTimeAndMemoryProbeTest

  itkBruker2DSEQImageIOTest
  itkPhilipsRECImageIOTest

  itkVoxBoCUBImageIOTest

  itkSliceBySliceImageFilterTest

  itkDiffeomorphicDemonsRegistrationFilterTest
  itkDiffeomorphicDemonsRegistrationFilterTest2

  itkAddConstantToImageFilterTest
  itkDivideByConstantImageFilterTest
  itkMultiplyByConstantImageFilterTest
  itkSubtractConstantFromImageFilterTest

  itkImageReadComplexWriteMagnitudeAndPhaseTest
  itkImageReadMagnitudeAndPhaseWriteComplexTest
  itkImageReadRealAndImaginaryWriteComplexTest

  itkExponentialDeformationFieldImageFilterTest
  itkFastSymmetricForcesDemonsRegistrationFilterTest
  itkGridForwardWarpImageFilterTest
  itkVectorCentralDifferenceImageFunctionTest
  itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest
  itkWarpHarmonicEnergyCalculatorTest

  itkBSplineScatteredDataPointSetToImageFilterTest
  itkBSplineScatteredDataPointSetToImageFilterTest2
  itkBSplineScatteredDataPointSetToImageFilterTest3

  itkCoxDeBoorBSplineKernelFunctionTest
  itkCoxDeBoorBSplineKernelFunctionTest2

  itkKappaSigmaThresholdImageCalculatorTest
  itkKappaSigmaThresholdImageFilterTest

  itkComposeRGBAImageFilterTest

  itkTransformToDeformationFieldSourceTest
  itkTransformToDeformationFieldSourceTest1

  itkBoxMeanImageFilterTest
  itkBoxSigmaImageFilterTest
  itkFastApproximateRankImageFilterTest
  itkMapMaskedRankImageFilterTest
  itkMapRankImageFilterTest
  itkMaskedRankImageFilterTest
  itkRankImageFilterTest

  itkDiscreteGaussianDerivativeImageFunctionTest
  itkDiscreteGradientMagnitudeGaussianImageFunctionTest
  itkDiscreteHessianGaussianImageFunctionTest
  itkGaussianDerivativeOperatorTest

  itkBinaryContourImageFilterTest
  itkLabelContourImageFilterTest

  itkFFTShiftImageFilterTest

  itkConvolutionImageFilterTest
  itkConvolutionImageFilterTestInt

  itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1
  itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2
  itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3

  itkRobustAutomaticThresholdImageFilterTest

  itkAreaOpeningImageFilterTest
  itkAreaClosingImageFilterTest

  itkAggregateLabelMapFilterTest
  itkAtanRegularizedHeavisideStepFunctionTest1
  itkBSplineDeformableTransformInitializerTest1
  itkBSplineDeformableTransformInitializerTest2
  itkContourExtractor2DImageFilterTest
  itkDirectFourierReconstructionImageToImageFilterTest
  itkGridImageSourceTest
  itkHeavisideStepFunctionTest1
  itkHessianToObjectnessMeasureImageFilterTest
  itkLabelImageToLabelMapFilterTest
  itkLabelMapFilterTest
  itkLabelMapTest
  itkLabelMapToLabelImageFilterTest
  itkLabelObjectLineComparatorTest
  itkLabelObjectLineTest
  itkLabelObjectTest
  itkMultiScaleHessianBasedMeasureImageFilterTest
  itkMultiScaleHessianBasedMeasureImageFilterTest
  itkMultiphaseDenseFiniteDifferenceImageFilterTest
  itkMultiphaseFiniteDifferenceImageFilterTest
  itkMultiphaseSparseFiniteDifferenceImageFilterTest
  itkRegionBasedLevelSetFunctionTest
  itkScalarRegionBasedLevelSetFunctionTest
  itkScalarToRGBColormapImageFilterTest
  itkSinRegularizedHeavisideStepFunctionTest1
  itkStochasticFractalDimensionImageFilterTest
  itkTriangleMeshToBinaryImageFilterTest3

  itkAutoCropLabelMapFilterTest1
  itkBinaryImageToLabelMapFilterTest
  itkChangeLabelLabelMapFilterTest
  itkChangeRegionLabelMapFilterTest1
  itkLabelMapToBinaryImageFilterTest
  itkRelabelLabelMapFilterTest1
  itkShiftScaleLabelMapFilterTest1

  itkCropLabelMapFilterTest1
  itkMergeLabelMapFilterTest1
  itkPadLabelMapFilterTest1
  itkRegionFromReferenceLabelMapFilterTest1

  itkBinaryImageToShapeLabelMapFilterTest1
  itkLabelImageToShapeLabelMapFilterTest1

  itkBinaryShapeKeepNObjectsImageFilterTest1
  itkBinaryShapeOpeningImageFilterTest1
  itkLabelShapeKeepNObjectsImageFilterTest1
  itkLabelShapeOpeningImageFilterTest1
  itkShapeKeepNObjectsLabelMapFilterTest1
  itkShapeLabelObjectAccessorsTest1
  itkShapeOpeningLabelMapFilterTest1
  itkShapeRelabelImageFilterTest1
  itkShapeRelabelLabelMapFilterTest1
  itkShapeUniqueLabelMapFilterTest1

  itkGaborImageSourceTest

  itkDiscreteGaussianDerivativeImageFilterTest
  itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest

  itkAttributeKeepNObjectsLabelMapFilterTest1
  itkAttributeLabelObjectAccessorsTest1
  itkAttributeOpeningLabelMapFilterTest1
  itkAttributePositionLabelMapFilterTest1
  itkAttributeRelabelLabelMapFilterTest1
  itkAttributeUniqueLabelMapFilterTest1
  itkBinaryReconstructionLabelMapFilterTest
  itkLabelMapContourOverlayImageFilterTest1
  itkLabelMapMaskImageFilterTest
  itkLabelMapOverlayImageFilterTest1
  itkLabelMapOverlayImageFilterTest2
  itkLabelMapToAttributeImageFilterTest1
  itkLabelMapToRGBImageFilterTest1
  itkLabelMapToRGBImageFilterTest2
  itkLabelSelectionLabelMapFilterTest
  itkLabelUniqueLabelMapFilterTest1
  itkObjectByObjectLabelMapFilterTest
  itkShapePositionLabelMapFilterTest1
  itkShiftLabelObjectTest
  itkStatisticsPositionLabelMapFilterTest1

  itkAttributeLabelObjectAccessorsTest1
  itkBinaryClosingByReconstructionImageFilterTest
  itkBinaryFillholeImageFilterTest1
  itkBinaryGrindPeakImageFilterTest1
  itkBinaryImageToStatisticsLabelMapFilterTest1
  itkBinaryOpeningByReconstructionImageFilterTest
  itkBinaryReconstructionByDilationImageFilterTest
  itkBinaryReconstructionByErosionImageFilterTest
  itkBinaryStatisticsKeepNObjectsImageFilterTest1
  itkBinaryStatisticsOpeningImageFilterTest1
  itkLabelGeometryImageFilterTest
  itkLabelImageToStatisticsLabelMapFilterTest1
  itkLabelStatisticsKeepNObjectsImageFilterTest1
  itkLabelStatisticsOpeningImageFilterTest1
  itkStatisticsKeepNObjectsLabelMapFilterTest1
  itkStatisticsOpeningLabelMapFilterTest1
  itkStatisticsRelabelImageFilterTest1
  itkStatisticsRelabelLabelMapFilterTest1
  itkStatisticsUniqueLabelMapFilterTest1

  itkMRCImageIOTest

  itkJPEG2000ImageIOFactoryTest01
  itkJPEG2000ImageIOTest00
  itkJPEG2000ImageIOTest01
  itkJPEG2000ImageIOTest02
  itkJPEG2000ImageIOTest03
  itkJPEG2000ImageIOTest04
  itkJPEG2000ImageIOTest05
  itkJPEG2000ImageIOTest06
  itkJPEG2000ImageIORegionOfInterest

  itkObjectToObjectMetricTest
  itkDeformationFieldTransformTest
  itkBSplineControlPointImageFilterTest
  itkBSplineControlPointImageFunctionTest
  itkN4MRIBiasFieldCorrectionImageFilterTest
  itkCompositeTransformTest

  itkScalarChanAndVeseLevelSetFunctionTest1
  itkScalarChanAndVeseLevelSetFunctionTest2
  itkScalarChanAndVeseDenseLevelSetImageFilterTest1
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  itkScalarChanAndVeseDenseLevelSetImageFilterTest3
  itkScalarChanAndVeseDenseLevelSetImageFilterTest4
  itkScalarChanAndVeseSparseLevelSetImageFilterTest1
  itkScalarChanAndVeseSparseLevelSetImageFilterTest2

  itkOptImageToImageMetricsTest2
  itkOptGrayscaleMorphologicalClosingImageFilterTest
  itkOptGrayscaleMorphologicalOpeningImageFilterTest
  itkOptGrayscaleDilateImageFilterTest
  itkOptGrayscaleErodeImageFilterTest
  itkMapOptGrayscaleMorphologicalClosingImageFilterTest
  itkMapOptGrayscaleMorphologicalOpeningImageFilterTest
  itkMapOptGrayscaleDilateImageFilterTest
  itkMapOptGrayscaleErodeImageFilterTest
)


if(ITK_USE_MINC2 AND MINC2_FOUND)
  set(KitTests ${KitTests} itkMINC2ImageIOTest)
endif(ITK_USE_MINC2 AND MINC2_FOUND)


if( USE_FFTWD OR USE_FFTWF )
  set( KitTests
    ${KitTests}
    itkFFTComplexToComplexImageFilterTest01
    itkFFTComplexToComplexImageFilterTest02
    )
endif( USE_FFTWD OR USE_FFTWF )

# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)

#############################################################
# Tests that use the Test Driver
#
add_test(itkOptImageToImageMetricsTest01
  ${TestDriver} itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

add_test(itkOptImageToImageMetricsTest02
  ${TestDriver}
  --with-threads 2
  itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

add_test(itkOptImageToImageMetricsTest03
  ${TestDriver}
  --without-threads
  itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

add_test(itkOptImageToImageMetricsTest04
  ${TestDriver} itkOptImageToImageMetricsTest2
  ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
  ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Common/itkBSplineDeformableTransformTest4.png
  )

add_test(itkOptMattesImageToImageMetricsTest01
  ${TestDriver} itkOptMattesMutualInformationImageToImageMetricThreadsTest1
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  0
  100
  )



add_test(itkFlatStructuringElementTest ${TestDriver}
  itkFlatStructuringElementTest
  )

add_test(itkDirectFourierReconstructionImageToImageFilterTest
  ${TestDriver}
  --compare ${BASELINE}/reconstruct.mha
            ${TEMP}/reconstruct.mha
  itkDirectFourierReconstructionImageToImageFilterTest
  ${INPUTDATA}/sinogram-small.mha
  ${TEMP}/reconstruct.mha
  1
  2
  0
  2
  2
  1
  3
  180
  0
  0
  0
  64
  64
  2
  0
  )

add_test(itkBSplineDeformableTransformInitializerTest1
  ${TestDriver}
  itkBSplineDeformableTransformInitializerTest1
  ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${TEMP}/itkBSplineDeformableTransformInitializerTest1.png
  ${TEMP}/itkBSplineDeformableTransformInitializerTest1DeformationField.mhd
)

add_test(itkBSplineDeformableTransformInitializerTest2
  ${TestDriver}
  itkBSplineDeformableTransformInitializerTest2
  ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${TEMP}/itkBSplineDeformableTransformInitializerTest2.png
  ${TEMP}/itkBSplineDeformableTransformInitializerTest2DeformationField.mhd
)

add_test(itkRegionBasedLevelSetFunctionTest ${TestDriver}
  itkRegionBasedLevelSetFunctionTest)
add_test(itkScalarRegionBasedLevelSetFunctionTest ${TestDriver}
  itkScalarRegionBasedLevelSetFunctionTest)
add_test( itkTriangleMeshToBinaryImageFilterTest3 ${TestDriver}
  itkTriangleMeshToBinaryImageFilterTest3
  ${INPUTDATA}/genusZeroSurface01.vtk
  ${TEMP}/itkTriangleMeshToBinaryImageFilterTest3.mha
    140   160   180   # image size X Y Z
   -0.7  -0.8  -0.9   # image origin X Y Z
   0.01  0.01  0.01   # image spacing X Y Z
  )

foreach(opacity 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
  string(REGEX REPLACE "\\." "_" op ${opacity})
  add_test(itkLabelOverlayImageFilterTest-Opacity-${op} ${TestDriver}
        --compare ${BASELINE}/cthead1-${op}Overlay.png
                  ${TEMP}/cthead1-${op}Overlay.png
                  itkLabelOverlayImageFilterTest
                  ${INPUTDATA}/cthead1.png
                  ${INPUTDATA}/cthead1Label.png
                  ${opacity}
                  ${TEMP}/cthead1-${op}Overlay.png)
endforeach(opacity)

add_test(itkLabelToRGBImageFilterTest ${TestDriver}
      --compare ${BASELINE}/cthead1Label-color.png
                ${TEMP}/cthead1Label-color.png
                itkLabelToRGBImageFilterTest
                ${INPUTDATA}/cthead1Label.png
                ${TEMP}/cthead1Label-color.png)

add_test(itkContourExtractor2DImageFilterTest ${TestDriver}
                                  itkContourExtractor2DImageFilterTest
                                  ${INPUTDATA}/ContourExtractor2DTest.tif  )

add_test(itkHeavisideStepFunctionTest1 ${TestDriver}
  itkHeavisideStepFunctionTest1
  )

add_test(itkLabelImageToLabelMapFilterTest ${TestDriver}
  itkLabelImageToLabelMapFilterTest
  )

add_test(itkLabelMapFilterTest ${TestDriver}
  itkLabelMapFilterTest
  )

add_test(itkLabelMapTest ${TestDriver}
  itkLabelMapTest
  )

add_test(itkLabelMapToLabelImageFilterTest ${TestDriver}
  itkLabelMapToLabelImageFilterTest
  )

add_test(itkLabelObjectLineComparatorTest ${TestDriver}
  itkLabelObjectLineComparatorTest
  )

add_test(itkLabelObjectLineTest ${TestDriver}
  itkLabelObjectLineTest
  )

add_test(itkLabelObjectTest ${TestDriver}
  itkLabelObjectTest
  )

add_test(itkMultiphaseFiniteDifferenceImageFilterTest ${TestDriver}
  itkMultiphaseFiniteDifferenceImageFilterTest
  )

add_test(itkMultiphaseDenseFiniteDifferenceImageFilterTest ${TestDriver}
  itkMultiphaseDenseFiniteDifferenceImageFilterTest
  )

add_test(itkMultiphaseSparseFiniteDifferenceImageFilterTest ${TestDriver}
  itkMultiphaseSparseFiniteDifferenceImageFilterTest
  )

add_test(itkAtanRegularizedHeavisideStepFunctionTest1 ${TestDriver}
  itkAtanRegularizedHeavisideStepFunctionTest1
  )

add_test(itkSinRegularizedHeavisideStepFunctionTest1 ${TestDriver}
  itkSinRegularizedHeavisideStepFunctionTest1
  )

add_test(itkStochasticFractalDimensionImageFilterTest1 ${TestDriver}
  --compare ${BASELINE}/itkStochasticFractalDimensionImageFilterTest1.mha
            ${TEMP}/itkStochasticFractalDimensionImageFilterTest1.mha
  itkStochasticFractalDimensionImageFilterTest
  2                                                         # image dimension
  ${INPUTDATA}/TreeBarkTexture.png                          # input image
  ${TEMP}/itkStochasticFractalDimensionImageFilterTest1.mha # output image
  2                                                         # radius
  )

add_test(itkStochasticFractalDimensionImageFilterTest2 ${TestDriver}
  --compare ${BASELINE}/itkStochasticFractalDimensionImageFilterTest2.mha
            ${TEMP}/itkStochasticFractalDimensionImageFilterTest2.mha
  itkStochasticFractalDimensionImageFilterTest
  2                                                         # image dimension
  ${INPUTDATA}/TreeBarkTexture.png                          # input image
  ${TEMP}/itkStochasticFractalDimensionImageFilterTest2.mha # output image
  2                                                         # radius
  ${INPUTDATA}/circle100.png                                # mask image
  255                                                       # mask label value
  )


add_test(itkNeuralNetworkIOTest ${TestDriver}
                  itkNeuralNetworkIOTest
                  ${INPUTDATA}/xornet.txt
                  ${INPUTDATA}/xortest.txt
                  ${TEMP}
                  )

add_test(itkConformalFlatteningMeshFilterTest1 ${TestDriver}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToSphere.vtk
                 1
                 )

add_test(itkConformalFlatteningQuadEdgeMeshFilterTest1 ${TestDriver}
                 itkConformalFlatteningQuadEdgeMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroQuadEdgeMesh01ConformalFlattenedToSphere.vtk
                 3
                 100.0
                 1
                 )

add_test(itkConformalFlatteningMeshFilterTest2 ${TestDriver}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToPlane.vtk
                 0
                 )

add_test(itkConformalFlatteningQuadEdgeMeshFilterTest2 ${TestDriver}
                 itkConformalFlatteningQuadEdgeMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroQuadEdgeMesh01ConformalFlattenedToPlane.vtk
                 3
                 100.0
                 0
                 )

add_test(itkValuedRegionalMinimaImageFilterTest ${TestDriver}
        --compare ${BASELINE}/cthead1ValuedRegionalMinimal-ref.png
                  ${TEMP}/cthead1ValuedRegionalMinimal.png
                  itkValuedRegionalMinimaImageFilterTest
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMinimal.png
                  ${TEMP}/cthead1ValuedRegionalMinimal-ref.png
                  )

add_test(itkValuedRegionalMinimaImageFilterTest2 ${TestDriver}
        --compare ${BASELINE}/cthead1ValuedRegionalMinimal-ref2.png
                  ${TEMP}/cthead1ValuedRegionalMinimal2.png
                  itkValuedRegionalMinimaImageFilterTest
                  0
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMinimal2.png
                  ${TEMP}/cthead1ValuedRegionalMinimal-ref2.png
                  )

add_test(itkValuedRegionalMaximaImageFilterTest ${TestDriver}
        --compare ${BASELINE}/cthead1ValuedRegionalMaximal-ref.png
                  ${TEMP}/cthead1ValuedRegionalMaximal.png
                  itkValuedRegionalMaximaImageFilterTest
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMaximal.png
                  ${TEMP}/cthead1ValuedRegionalMaximal-ref.png
                  )

add_test(itkValuedRegionalMaximaImageFilterTest2 ${TestDriver}
        --compare ${BASELINE}/cthead1ValuedRegionalMaximal-ref2.png
                  ${TEMP}/cthead1ValuedRegionalMaximal2.png
                  itkValuedRegionalMaximaImageFilterTest
                  0
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMaximal2.png
                  ${TEMP}/cthead1ValuedRegionalMaximal-ref2.png
                  )

add_test(itkRegionalMinimaImageFilterTest2_1 ${TestDriver}
        --compare ${BASELINE}/cthead1RegionalMinimal-ref2_1.png
                  ${TEMP}/cthead1RegionalMinimal2_1.png
                  itkRegionalMaximaImageFilterTest2
                  1
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMinimal2_1.png
                  ${TEMP}/cthead1RegionalMinimal-ref2_1.png
                  )
add_test(itkRegionalMinimaImageFilterTest2_2 ${TestDriver}
        --compare ${BASELINE}/cthead1RegionalMinimal-ref2_2.png
                  ${TEMP}/cthead1RegionalMinimal2_2.png
                  itkRegionalMaximaImageFilterTest2
                  0
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMinimal2_2.png
                  ${TEMP}/cthead1RegionalMinimal-ref2_2.png
                  )

add_test(itkRegionalMaximaImageFilterTest2_1 ${TestDriver}
        --compare ${BASELINE}/cthead1RegionalMaximal-ref2_1.png
                  ${TEMP}/cthead1RegionalMaximal2_1.png
                  itkRegionalMaximaImageFilterTest2
                  1
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMaximal2_1.png
                  ${TEMP}/cthead1RegionalMaximal-ref2_1.png
                  )
add_test(itkRegionalMaximaImageFilterTest2_2 ${TestDriver}
        --compare ${BASELINE}/cthead1RegionalMaximal-ref2_2.png
                  ${TEMP}/cthead1RegionalMaximal2_2.png
                  itkRegionalMaximaImageFilterTest2
                  0
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMaximal2_2.png
                  ${TEMP}/cthead1RegionalMaximal-ref2_2.png
                  )

foreach(M 0 1)
  foreach(F 0 1)
    add_test(itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F} ${TestDriver}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/button-hole.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      ${M} ${F} 0
                      )

    add_test(itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F} ${TestDriver}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/pass-values.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      ${M} ${F} 0
                      )

    add_test(itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F} ${TestDriver}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/plateau.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      ${M} ${F} 0
                      )

    add_test(itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F} ${TestDriver}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/thick-lines.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      ${M} ${F} 0
                      )

    add_test(itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F} ${TestDriver}
            --compare ${BASELINE}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      itkMorphologicalWatershedFromMarkersImageFilterTest
                      ${INPUTDATA}/cthead1.png
                      ${INPUTDATA}/cthead1-markers.png
                      ${TEMP}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      ${M} ${F}
                      )
  endforeach(F)
endforeach(M)

foreach(level 00 10 20 30 40 50)
  add_test(itkMorphologicalWatershedImageFilterTestLevel${level} ${TestDriver}
          --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    ${TEMP}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    itkMorphologicalWatershedImageFilterTest
                    ${INPUTDATA}/level.png
                    ${TEMP}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    1 0 ${level}
                    )
endforeach(level)

add_test(itkBinaryMorphologicalClosingImageFilterTest$ ${TestDriver}
        --compare ${BASELINE}/itkBinaryMorphologicalClosingImageFilterTest.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTest.png
                  itkBinaryMorphologicalClosingImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTest.png
                  40 1 200
                  )

add_test(itkBinaryMorphologicalClosingImageFilterTestUnsafe$ ${TestDriver}
        --compare ${BASELINE}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  itkBinaryMorphologicalClosingImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  40 0 200
                  )

add_test(itkBinaryMorphologicalOpeningImageFilterTest$ ${TestDriver}
        --compare ${BASELINE}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  ${TEMP}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  itkBinaryMorphologicalOpeningImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  8 150 200
                  )
add_test(itkTimeAndMemoryProbeTest1 ${TestDriver}
        itkTimeAndMemoryProbeTest
        )

add_test(itkBruker2DSEQImageIOTest1 ${TestDriver}
        itkBruker2DSEQImageIOTest
        ${ITK_DATA_ROOT}/Input/DCB120604.t61/11/pdata/1/2dseq
        ${ITK_TEST_OUTPUT_DIR}/itkBruker2DSEQImageIOTest1.mha
        )

add_test(itkPhilipsRECImageIOTest1 ${TestDriver}
        itkPhilipsRECImageIOTest
        ${ITK_DATA_ROOT}/Input/T1_MEASUREMENT_4_1.PAR
        ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOTest1.mha
        )

add_test(itkVoxBoCUBImageIOTest1 ${TestDriver}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.cub
        )

add_test(itkVoxBoCUBImageIOTest2 ${TestDriver}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub
        )

add_test(itkVoxBoCUBImageIOTest3 ${TestDriver}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume1.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub.gz
        )

add_test(itkVoxBoCUBImageIOTest4 ${TestDriver}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume1.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub.gz
        )

add_test(itkVoxBoCUBImageIOTest5 ${TestDriver}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub
        )

add_test(itkVoxBoCUBImageIOTest6 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha
        )

add_test(itkVoxBoCUBImageIOTest7 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha
        )

add_test(itkSliceBySliceImageFilterTest ${TestDriver}
        itkSliceBySliceImageFilterTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
        ${ITK_TEST_OUTPUT_DIR}/itkSliceBySliceImageFilterTest.mha
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest01 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  0  0.001  20.0
        200   0.01   0.08   5 10
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest02 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest 1  0  0.01  3.0
        400   0.2   0.9   10 70
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest03 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest 2  0  0.001  20.0
        200   0.01   0.08   5   10
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest04 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest 3  0  0.01  3.0
        200   0.2   0.8   5   70
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest05 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  1  0.01  3.0
        400   0.2   0.08   5  70
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest06 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  0  0.005  20.0
        200   0.01   0.08   5  10
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest07 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest2
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest07.mha
        0  0  0.001  0.1
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest08 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest2
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest08.mha
        1  0  0.001  0.1
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest09 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest2
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest09.mha
        2  0  0.001  0.1
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest10 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest2
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest10.mha
        3  0  0.001  0.1
        )

add_test(itkDiffeomorphicDemonsRegistrationFilterTest11 ${TestDriver}
        itkDiffeomorphicDemonsRegistrationFilterTest2
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest11.mha
        0  1  0.001  0.1
        )

add_test(itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest ${TestDriver}
        itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest
        )

add_test(itkVectorCentralDifferenceImageFunctionTest ${TestDriver}
        itkVectorCentralDifferenceImageFunctionTest
        )

add_test(itkExponentialDeformationFieldImageFilterTest ${TestDriver}
        itkExponentialDeformationFieldImageFilterTest
        )

add_test(itkFastSymmetricForcesDemonsRegistrationFilterTest ${TestDriver}
        itkFastSymmetricForcesDemonsRegistrationFilterTest
        )

add_test(itkGridForwardWarpImageFilterTest ${TestDriver}
        itkGridForwardWarpImageFilterTest
        )

add_test(itkWarpHarmonicEnergyCalculatorTest ${TestDriver}
        itkWarpHarmonicEnergyCalculatorTest
        )

add_test(itkBSplineScatteredDataPointSetToImageFilterTest01 ${TestDriver}
        --compare
          ${BASELINE}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
          ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
        itkBSplineScatteredDataPointSetToImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cake_easy.png
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
        )

add_test(itkBSplineScatteredDataPointSetToImageFilterTest02 ${TestDriver}
        itkBSplineScatteredDataPointSetToImageFilterTest2
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredApproximationDataPointsTestOutput2.txt
        )

add_test(itkBSplineScatteredDataPointSetToImageFilterTest03 ${TestDriver}
        itkBSplineScatteredDataPointSetToImageFilterTest3
        ${ITK_SOURCE_DIR}/Testing/Data/Input/BSplineScatteredApproximationDataPointsInput.txt
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredApproximationDataPointsTestOutput3.txt
        )

add_test(itkCoxDeBoorBSplineKernelFunctionTest01 ${TestDriver}
        itkCoxDeBoorBSplineKernelFunctionTest
        )

add_test(itkCoxDeBoorBSplineKernelFunctionTest02 ${TestDriver}
        itkCoxDeBoorBSplineKernelFunctionTest2
        )

add_test(itkKappaSigmaThresholdImageCalculatorTest01 ${TestDriver}
        itkKappaSigmaThresholdImageCalculatorTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence1.png
        10 3.5 19
        )

add_test(itkKappaSigmaThresholdImageCalculatorTest02 ${TestDriver}
        itkKappaSigmaThresholdImageCalculatorTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence2.png
        10 3.0 19
        )

add_test(itkKappaSigmaThresholdImageFilterTest01 ${TestDriver}
        itkKappaSigmaThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence1.png
        ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest01.mha
        10 3.5
        )

add_test(itkKappaSigmaThresholdImageFilterTest02 ${TestDriver}
        itkKappaSigmaThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence2.png
        ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest02.mha
        10 3.0
        )

add_test(itkMultiplyByConstantImageFilterTest ${TestDriver}
        itkMultiplyByConstantImageFilterTest
        )

add_test(itkDivideByConstantImageFilterTest ${TestDriver}
        itkDivideByConstantImageFilterTest
        )

add_test(itkAddConstantToImageFilterTest ${TestDriver}
        itkAddConstantToImageFilterTest
        )

add_test(itkSubtractConstantFromImageFilterTest ${TestDriver}
        itkSubtractConstantFromImageFilterTest
        )

add_test(itkImageReadRealAndImaginaryWriteComplexTest ${TestDriver}
        itkImageReadRealAndImaginaryWriteComplexTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha
        )

add_test(itkImageReadComplexWriteMagnitudeAndPhaseTest ${TestDriver}
        itkImageReadComplexWriteMagnitudeAndPhaseTest
        ${ITK_SOURCE_DIR}/Examples/Data/itkBrainSliceComplex.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexMagnitude.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexPhase.mha
        )

add_test(itkImageReadMagnitudeAndPhaseWriteComplexTest ${TestDriver}
        itkImageReadMagnitudeAndPhaseWriteComplexTest
        ${ITK_SOURCE_DIR}/Examples/Data/itkBrainSliceComplexMagnitude.mha
        ${ITK_SOURCE_DIR}/Examples/Data/itkBrainSliceComplexPhase.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha
        )

add_test(itkComposeRGBAImageFilterTest ${TestDriver}
    itkComposeRGBAImageFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkComposeRGBAImageFilterTestOutput.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number1inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number2inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number3inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/IntensityRamp64.png
)

add_test(itkTransformToDeformationFieldSourceTest01 ${TestDriver}
    itkTransformToDeformationFieldSourceTest
    Affine
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestField01.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestImage01.mha
)

add_test(itkTransformToDeformationFieldSourceTest02 ${TestDriver}
    itkTransformToDeformationFieldSourceTest
    BSpline
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestField02.mha
    ${INPUTDATA}/parametersBSpline.txt
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestImage02.mha
)

add_test(itkTransformToDeformationFieldSourceTest03 ${TestDriver}
   --compare ${TEMP}/transformedImage.nii
             ${TEMP}/warpedImage.nii
  --compareNumberOfPixelsTolerance 20
   itkTransformToDeformationFieldSourceTest1
     ${TEMP}/transformedImage.nii
     ${TEMP}/warpedImage.nii
)

add_test(itkBoxMeanImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkBoxMeanImageFilter3.png
            ${TEMP}/itkBoxMeanImageFilter3.png
  itkBoxMeanImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkBoxMeanImageFilter3.png
  3
)

add_test(itkBoxMeanImageFilterTest10 ${TestDriver}
  --compare ${BASELINE}/itkBoxMeanImageFilter10.png
            ${TEMP}/itkBoxMeanImageFilter10.png
  itkBoxMeanImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkBoxMeanImageFilter10.png
  10
)


add_test(itkBoxSigmaImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkBoxSigmaImageFilter3.png
            ${TEMP}/itkBoxSigmaImageFilter3.png
  itkBoxSigmaImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkBoxSigmaImageFilter3.png
  3
)

add_test(itkBoxSigmaImageFilterTest10 ${TestDriver}
  --compare ${BASELINE}/itkBoxSigmaImageFilter10.png
            ${TEMP}/itkBoxSigmaImageFilter10.png
  itkBoxSigmaImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkBoxSigmaImageFilter10.png
  10
)


add_test(itkRankImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkRankImageFilter3.png
            ${TEMP}/itkRankImageFilter3.png
  itkRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkRankImageFilter3.png
  3
)

add_test(itkRankImageFilterTest10 ${TestDriver}
  --compare ${BASELINE}/itkRankImageFilter10.png
            ${TEMP}/itkRankImageFilter10.png
  itkRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkRankImageFilter10.png
  10
)


add_test(itkMapRankImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkRankImageFilter3.png
            ${TEMP}/itkMapRankImageFilter3.png
  itkMapRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapRankImageFilter3.png
  3
)

add_test(itkMapRankImageFilterTest10 ${TestDriver}
  --compare ${BASELINE}/itkRankImageFilter10.png
            ${TEMP}/itkMapRankImageFilter10.png
  itkMapRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapRankImageFilter10.png
  10
)


add_test(itkMaskedRankImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkMaskedRankImageFilter3.png
            ${TEMP}/itkMaskedRankImageFilter3.png
  itkMaskedRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/itkMaskedRankImageFilter3.png
  3
)

add_test(itkMaskedRankImageFilterTest10 ${TestDriver}
  --compare ${BASELINE}/itkMaskedRankImageFilter10.png
            ${TEMP}/itkMaskedRankImageFilter10.png
  itkMaskedRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/itkMaskedRankImageFilter10.png
  10
)


add_test(itkMapMaskedRankImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkMaskedRankImageFilter3.png
            ${TEMP}/itkMapMaskedRankImageFilter3.png
  itkMapMaskedRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/itkMapMaskedRankImageFilter3.png
  3
)

add_test(itkMapMaskedRankImageFilterTest10 ${TestDriver}
  --compare ${BASELINE}/itkMaskedRankImageFilter10.png
            ${TEMP}/itkMapMaskedRankImageFilter10.png
  itkMapMaskedRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/itkMapMaskedRankImageFilter10.png
  10
)


add_test(itkFastApproximateRankImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkFastApproximateRankImageFilter3.png
            ${TEMP}/itkFastApproximateRankImageFilter3.png
  itkFastApproximateRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkFastApproximateRankImageFilter3.png
  3
)

add_test(itkFastApproximateRankImageFilterTest10 ${TestDriver}
  --compare ${BASELINE}/itkFastApproximateRankImageFilter10.png
            ${TEMP}/itkFastApproximateRankImageFilter10.png
  itkFastApproximateRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkFastApproximateRankImageFilter10.png
  10
)

add_test(itkOptGrayscaleMorphologicalClosingImageFilterTest ${TestDriver}
  --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
  --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
  --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
  itkOptGrayscaleMorphologicalClosingImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
  ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
  ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
  ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
  0
)
add_test(itkOptGrayscaleMorphologicalClosingImageFilterTestSafeBorder ${TestDriver}
  --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
  --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
  --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
  itkOptGrayscaleMorphologicalClosingImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
  ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
  ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
  ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
  1
)
add_test(itkOptGrayscaleMorphologicalOpeningImageFilterTest ${TestDriver}
  --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
  --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
  --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
  itkOptGrayscaleMorphologicalOpeningImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
  ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
  ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
  ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
  0
)
add_test(itkOptGrayscaleMorphologicalOpeningImageFilterTestSafeBorder ${TestDriver}
  --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
  --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
  --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
  itkOptGrayscaleMorphologicalOpeningImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
  ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
  ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
  ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
  1
)
add_test(itkOptGrayscaleDilateImageFilterTest ${TestDriver}
  --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleDilateImageFilterTestHisto.png
  --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleDilateImageFilterTestVHGW.png
  --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleDilateImageFilterTestAnchor.png
  itkOptGrayscaleDilateImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
  ${TEMP}/itkOptGrayscaleDilateImageFilterTestHisto.png
  ${TEMP}/itkOptGrayscaleDilateImageFilterTestVHGW.png
  ${TEMP}/itkOptGrayscaleDilateImageFilterTestAnchor.png
)
add_test(itkOptGrayscaleErodeImageFilterTest ${TestDriver}
  --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleErodeImageFilterTestHisto.png
  --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleErodeImageFilterTestVHGW.png
  --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkOptGrayscaleErodeImageFilterTestAnchor.png
  itkOptGrayscaleErodeImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
  ${TEMP}/itkOptGrayscaleErodeImageFilterTestHisto.png
  ${TEMP}/itkOptGrayscaleErodeImageFilterTestVHGW.png
  ${TEMP}/itkOptGrayscaleErodeImageFilterTestAnchor.png
)

add_test(itkMapOptGrayscaleMorphologicalClosingImageFilterTest ${TestDriver}
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
  itkMapOptGrayscaleMorphologicalClosingImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
  0
)
add_test(itkMapOptGrayscaleMorphologicalClosingImageFilterTestSafeBorder ${TestDriver}
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
  itkMapOptGrayscaleMorphologicalClosingImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
  1
)
add_test(itkMapOptGrayscaleMorphologicalOpeningImageFilterTest ${TestDriver}
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
  itkMapOptGrayscaleMorphologicalOpeningImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
  0
)
add_test(itkMapOptGrayscaleMorphologicalOpeningImageFilterTestSafeBorder ${TestDriver}
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
  --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
  itkMapOptGrayscaleMorphologicalOpeningImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
  ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
  1
)
add_test(itkMapOptGrayscaleDilateImageFilterTest ${TestDriver}
  --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestHisto.png
  --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestVHGW.png
  --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestAnchor.png
  itkMapOptGrayscaleDilateImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
  ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestHisto.png
  ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestVHGW.png
  ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestAnchor.png
)
add_test(itkMapOptGrayscaleErodeImageFilterTest ${TestDriver}
  --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestHisto.png
  --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestVHGW.png
  --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
            ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestAnchor.png
  itkMapOptGrayscaleErodeImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
  ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestHisto.png
  ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestVHGW.png
  ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestAnchor.png
)

add_test(itkGaussianDerivativeOperatorTest ${TestDriver}
  itkGaussianDerivativeOperatorTest
)

add_test(itkDiscreteGaussianDerivativeImageFunctionTest01 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFunctionTest01.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFunctionTest01.png
  itkDiscreteGaussianDerivativeImageFunctionTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFunctionTest01.png
  1
  2.0
  0.01
  32
)

add_test(itkDiscreteGradientMagnitudeGaussianImageFunctionTest01 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
            ${TEMP}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
  itkDiscreteGradientMagnitudeGaussianImageFunctionTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
  2.0
  0.01
  32
)

add_test(itkDiscreteHessianGaussianImageFunctionTest01 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteHessianGaussianImageFunctionTest01_0.mha
            ${TEMP}/itkDiscreteHessianGaussianImageFunctionTest01_0.mha
  --compare ${BASELINE}/itkDiscreteHessianGaussianImageFunctionTest01_1.mha
            ${TEMP}/itkDiscreteHessianGaussianImageFunctionTest01_1.mha
  --compare ${BASELINE}/itkDiscreteHessianGaussianImageFunctionTest01_2.mha
            ${TEMP}/itkDiscreteHessianGaussianImageFunctionTest01_2.mha
  itkDiscreteHessianGaussianImageFunctionTest
  ${INPUTDATA}/HeadMRVolume.mhd
  ${TEMP}/itkDiscreteHessianGaussianImageFunctionTest01_%i.mha
  2.0
  0.01
  20
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest01 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest01.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest01.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest01.png
  0    # orderX
  1    # orderY
  3.0  # sigma
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest10 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest10.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest10.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest10.png
  1    # orderX
  0    # orderY
  3.0  # sigma
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest11 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest11.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest11.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest11.png
  1    # orderX
  1    # orderY
  3.0  # sigma
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest20 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest20.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest20.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest20.png
  2    # orderX
  0    # orderY
  3.0  # sigma
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest02 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest02.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest02.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest02.png
  0    # orderX
  2    # orderY
  3.0  # sigma
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest21 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest21.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest21.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest21.png
  2    # orderX
  1    # orderY
  3.0  # sigma
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest12 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest12.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest12.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest12.png
  1    # orderX
  2    # orderY
  3.0  # sigma
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest30 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest30.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest30.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest30.png
  3    # orderX
  0    # orderY
  3.0  # sigma
)


add_test(itkDiscreteGaussianDerivativeImageFilterTest03 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest03.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest03.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest03.png
  0    # orderX
  3    # orderY
  3.0  # sigma
)

add_test(itkDiscreteGaussianDerivativeImageFilterTest01 ${TestDriver}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest01.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest01.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest01.png
  0    # orderX
  1    # orderY
  3.0  # sigma
)


add_test(itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest ${TestDriver}
  itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest
)


add_test(itkBinaryContourImageFilterTest0 ${TestDriver}
  --compare ${BASELINE}/itkBinaryContourImageFilterTest0.png
            ${TEMP}/itkBinaryContourImageFilterTest0.png
   itkBinaryContourImageFilterTest
   ${INPUTDATA}/2th_cthead1.png
   ${TEMP}/itkBinaryContourImageFilterTest0.png
   0 200 0
)

add_test(itkBinaryContourImageFilterTest1 ${TestDriver}
  --compare ${BASELINE}/itkBinaryContourImageFilterTest1.png
            ${TEMP}/itkBinaryContourImageFilterTest1.png
   itkBinaryContourImageFilterTest
   ${INPUTDATA}/2th_cthead1.png
   ${TEMP}/itkBinaryContourImageFilterTest1.png
   1 200 0
)

add_test(itkLabelContourImageFilterTest0 ${TestDriver}
  --compare ${BASELINE}/itkLabelContourImageFilterTest0.png
            ${TEMP}/itkLabelContourImageFilterTest0.png
   itkLabelContourImageFilterTest
   ${INPUTDATA}/2th_cthead1.png
   ${TEMP}/itkLabelContourImageFilterTest0.png
   0 0
)

add_test(itkLabelContourImageFilterTest1 ${TestDriver}
  --compare ${BASELINE}/itkLabelContourImageFilterTest1.png
            ${TEMP}/itkLabelContourImageFilterTest1.png
   itkLabelContourImageFilterTest
   ${INPUTDATA}/2th_cthead1.png
   ${TEMP}/itkLabelContourImageFilterTest1.png
   1 0
)

add_test(itkFFTShiftImageFilterTestOdd0 ${TestDriver}
  --compare ${BASELINE}/itkFFTShiftImageFilterTest0.png
            ${TEMP}/itkFFTShiftImageFilterTest0.png
   itkFFTShiftImageFilterTest
   ${INPUTDATA}/BrainSliceBinary.png
   ${TEMP}/itkFFTShiftImageFilterTest0.png
   0
)

add_test(itkFFTShiftImageFilterTestOdd1 ${TestDriver}
  --compare ${INPUTDATA}/BrainSliceBinary.png
            ${TEMP}/itkFFTShiftImageFilterTest1.png
   itkFFTShiftImageFilterTest
   ${BASELINE}/itkFFTShiftImageFilterTest0.png
   ${TEMP}/itkFFTShiftImageFilterTest1.png
   1
)

add_test(itkFFTShiftImageFilterTestEven0 ${TestDriver}
  --compare ${BASELINE}/itkFFTShiftImageFilterTestEven0.png
            ${TEMP}/itkFFTShiftImageFilterTestEven0.png
   itkFFTShiftImageFilterTest
   ${INPUTDATA}/cthead1.png
   ${TEMP}/itkFFTShiftImageFilterTestEven0.png
   0
)

add_test(itkFFTShiftImageFilterTestEven1 ${TestDriver}
  --compare ${INPUTDATA}/cthead1.png
            ${TEMP}/itkFFTShiftImageFilterTestEven1.png
   itkFFTShiftImageFilterTest
   ${BASELINE}/itkFFTShiftImageFilterTestEven0.png
   ${TEMP}/itkFFTShiftImageFilterTestEven1.png
   1
)

add_test(itkConvolutionImageFilterTest5x5Mean ${TestDriver}
  --compare ${BASELINE}/itkConvolutionImageFilterTest5x5Mean.png
            ${TEMP}/itkConvolutionImageFilterTest5x5Mean.png
   itkConvolutionImageFilterTestInt
   ${INPUTDATA}/cthead1.png
   ${INPUTDATA}/5x5-constant.png
   ${TEMP}/itkConvolutionImageFilterTest5x5Mean.png
   1
)

add_test(itkConvolutionImageFilterTestSobelX ${TestDriver}
  --compare ${BASELINE}/itkConvolutionImageFilterTestSobelX.nii.gz
            ${TEMP}/itkConvolutionImageFilterTestSobelX.nii.gz
   itkConvolutionImageFilterTest
   ${INPUTDATA}/cthead1.png
   ${INPUTDATA}/sobel_x.nii.gz
   ${TEMP}/itkConvolutionImageFilterTestSobelX.nii.gz
)

add_test(itkConvolutionImageFilterTestSobelY ${TestDriver}
  --compare ${BASELINE}/itkConvolutionImageFilterTestSobelY.nii.gz
            ${TEMP}/itkConvolutionImageFilterTestSobelY.nii.gz
   itkConvolutionImageFilterTest
   ${INPUTDATA}/cthead1.png
   ${INPUTDATA}/sobel_y.nii.gz
   ${TEMP}/itkConvolutionImageFilterTestSobelY.nii.gz
)

add_test(itkRobustAutomaticThresholdImageFilterTest ${TestDriver}
  --compare ${BASELINE}/itkRobustAutomaticThresholdImageFilterTest.png
            ${TEMP}/itkRobustAutomaticThresholdImageFilterTest.png
  itkRobustAutomaticThresholdImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkRobustAutomaticThresholdImageFilterTest.png
  2
)

add_test( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1 ${TestDriver}
   itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1
   ${INPUTDATA}/8ConnectedImage.bmp )
add_test( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2
          ${TestDriver}
          itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2 )
add_test( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3
          ${TestDriver}
          itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3 )

add_test(itkAreaOpeningImageFilterTest0 ${TestDriver}
  --compare ${BASELINE}/itkAreaOpeningImageFilterTest0.png
            ${TEMP}/itkAreaOpeningImageFilterTest0.png
  itkAreaOpeningImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkAreaOpeningImageFilterTest0.png
  1000 0 1
)

add_test(itkAreaClosingImageFilterTest0 ${TestDriver}
  --compare ${BASELINE}/itkAreaClosingImageFilterTest0.png
            ${TEMP}/itkAreaClosingImageFilterTest0.png
  itkAreaClosingImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkAreaClosingImageFilterTest0.png
  1000 0 0
)

add_test(itkHessianToObjectnessMeasureImageFilterTest ${TestDriver}
  --compare ${BASELINE}/itkHessianToObjectnessMeasureImageFilterTest.mha
            ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput.mha
            --compareIntensityTolerance .01
  itkHessianToObjectnessMeasureImageFilterTest
  ${INPUTDATA}/DSA.png
  ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput.mha
  1 0)

add_test(itkHessianToObjectnessMeasureImageFilterTest2 ${TestDriver}
  --compare ${BASELINE}/itkHessianToObjectnessMeasureImageFilterTest2.mha
            ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput2.mha
            --compareIntensityTolerance .01
  itkHessianToObjectnessMeasureImageFilterTest
  ${INPUTDATA}/DSA.png
  ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput2.mha
  0 0)


add_test(itkMultiScaleHessianBasedMeasureImageFilterTest ${TestDriver}
  itkMultiScaleHessianBasedMeasureImageFilterTest
  ${INPUTDATA}/DSA.png
  ${TEMP}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha
  ${TEMP}/itkMultiScaleHessianBasedMeasureImageFilterTestScalesOutput.mha
   5  # Minimum Sigma
  10  # Maximum Sigma
  10  # Number of Sigma steps
   1  # Object Dimension (search for tubes)
   0  # Search for Dark objects over bright background
  ${TEMP}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput2.mha # Output image with equispaced sigma steps
  )

add_test(itkAggregateLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-labelAggregate.mha
            ${TEMP}/cthead1-labelAggregate.mha
  itkAggregateLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-labelAggregate.mha
  )

add_test(itkBinaryImageToLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-labeled.mha
            ${TEMP}/cthead1-labeled.mha
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/cthead1-labeled.mha
  1     # Fully connected
  255   # Input foreground
  0     # Output background
  0     # Expect failure
  )

add_test(itkBinaryImageToLabelMapFilterTest2
  ${TestDriver}
  --compare ${BASELINE}/connected0lines-0.png
            ${TEMP}/connected0lines-0.png
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/connected0lines.png
  ${TEMP}/connected0lines-0.png
  0     # Not fully connected
  255   # Input foreground
  0     # Output background
  0     # Expect failure
  )

add_test(itkBinaryImageToLabelMapFilterTest3
  ${TestDriver}
  --compare ${BASELINE}/connected0lines-1.png
            ${TEMP}/connected0lines-1.png
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/connected0lines.png
  ${TEMP}/connected0lines-1.png
  1     # Fully connected
  255   # Input foreground
  0     # Output background
  0     # Expect failure
  )

add_test(itkBinaryImageToLabelMapFilterTest4
  ${TestDriver}
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/289.png
  ${TEMP}/tooManyObjects.png
  0     # Not fully connected
  255   # Input foreground
  0     # Output background
  1     # Expect failure
  )

add_test(itkBinaryImageToLabelMapFilterTest5
  ${TestDriver}
  --compare ${BASELINE}/nonConnected3DLines-0.tif
            ${TEMP}/nonConnected3DLines-0.tif
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/nonConnected3DLines.tif
  ${TEMP}/nonConnected3DLines-0.tif
  0     # Not fully connected
  255   # Input foreground
  0     # Output background
  0     # Expect failure
  )

add_test(itkLabelMapToBinaryImageFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-binary.mha
            ${TEMP}/cthead1-label-binary.mha
  itkLabelMapToBinaryImageFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-binary.mha
  255   # Input foreground
  0     # Output background
  )

add_test(itkChangeLabelLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-changed.mha
            ${TEMP}/cthead1-label-changed.mha
  itkChangeLabelLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-changed.mha
   94  150  # ( Old, New ) Label pair
   92  163  # ( Old, New ) Label pair
  176  176  # ( Old, New ) Label pair
  )

add_test(itkChangeLabelLabelMapFilterTest2
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-changed2.mha
            ${TEMP}/cthead1-label-changed2.mha
  itkChangeLabelLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-changed2.mha
   94  100  # ( Old, New ) Label pair
    0   50  # ( Old, New ) Label pair background
   92    0
  )

add_test(itkChangeLabelLabelMapFilterTest3
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-changed3.png
            ${TEMP}/cthead1-label-changed3.png
  itkChangeLabelLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-changed3.png
    0   94  # ( Old, New ) Label pair background
  )

add_test(itkRelabelLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-relabeled.mha
            ${TEMP}/cthead1-label-relabeled.mha
  itkRelabelLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-relabeled.mha
  )

add_test(itkShiftScaleLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-shiftscaled.mha
            ${TEMP}/cthead1-label-shiftscaled.mha
  itkShiftScaleLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-shiftscaled.mha
   10   # shift value
   0.5  # scale value
   true # change background (boolean)
  )

add_test(itkAutoCropLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-autocrop.mha
            ${TEMP}/cthead1-label-autocrop.mha
  itkAutoCropLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-autocrop.mha
  176  # Value to use as background of input image
  4    # border size
  4    # border size
  )

add_test(itkChangeRegionLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-changeregion.mha
            ${TEMP}/cthead1-label-changeregion.mha
  itkChangeRegionLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-changeregion.mha
  50  100  # Start Index
  120 150  # Size
  )

add_test(itkMergeLabelMapFilterTest1a
  ${TestDriver}
  --compare ${BASELINE}/simple-label-merge-keep.mha
            ${TEMP}/simple-label-merge-keep.mha
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-c.png
  ${INPUTDATA}/simple-label-d.png
  ${TEMP}/simple-label-merge-keep.mha
  50   # Background value 1
  50   # Background value 2
  0    # Keep Method
  0    # Expect success
  )

add_test(itkMergeLabelMapFilterTest1b
  ${TestDriver}
  --compare ${BASELINE}/simple-label-merge-aggregate.mha
            ${TEMP}/simple-label-merge-aggregate.mha
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-c.png
  ${INPUTDATA}/simple-label-d.png
  ${TEMP}/simple-label-merge-aggregate.mha
  50   # Background value 1
  50   # Background value 2
  1    # Aggregate Method
  0    # Expect success
  )

add_test(itkMergeLabelMapFilterTest1c
  ${TestDriver}
  --compare ${BASELINE}/simple-label-merge-pack.mha
            ${TEMP}/simple-label-merge-pack.mha
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-c.png
  ${INPUTDATA}/simple-label-d.png
  ${TEMP}/simple-label-merge-pack.mha
  50   # Background value 1
  50   # Background value 2
  2    # Pack Method
  0    # Expect success
  )


add_test(itkMergeLabelMapFilterTest1d
  ${TestDriver}
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-c.png
  ${INPUTDATA}/simple-label-d.png
  ${TEMP}/thisFileShouldNotExist.mha
  50   # Background value 1
  50   # Background value 2
  3    # Strict Method
  1    # Expect FAILURE
  )

add_test(itkMergeLabelMapFilterTest1e
  ${TestDriver}
  --compare ${BASELINE}/simple-label-merge-strict.mha
            ${TEMP}/simple-label-merge-strict.mha
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-e.png
  ${INPUTDATA}/simple-label-d.png
  ${TEMP}/simple-label-merge-strict.mha
  50   # Background value 1
  50   # Background value 2
  3    # Strict Method
  0    # Expect success
  )

add_test(itkMergeLabelMapFilterTest1f
  ${TestDriver}
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-e.png
  ${INPUTDATA}/simple-label-d.png
  ${TEMP}/simple-label-merge-wrong.mha
  50   # Background value 1
  50   # Background value 2
  10   # wrong Method
  1    # Expect failure
  )


add_test(itkLabelImageToShapeLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/simple-label-to-shapelabelmap.mha
            ${TEMP}/simple-label-to-shapelabelmap.mha
  itkLabelImageToShapeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-b.png
  ${TEMP}/simple-label-to-shapelabelmap.mha
  90    #output background value
  1     #compute Feret Diameter (bool)
  1     #computer Perimeter (bool)
  )

add_test(itkCropLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-crop.mha
            ${TEMP}/cthead1-label-crop.mha
  itkCropLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-crop.mha
  40    # Horizontal left and right crop border
  50    # Vertical upper and lower crop border
  )

add_test(itkPadLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-pad.mha
            ${TEMP}/cthead1-label-pad.mha
  itkPadLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-pad.mha
  40    # Horizontal left and right pad border
  50    # Vertical upper and lower pad border
  )

add_test(itkRegionFromReferenceLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-label-regionreference.mha
            ${TEMP}/cthead1-label-regionreference.mha
  itkRegionFromReferenceLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${INPUTDATA}/circle.png  #TODO replace with reference image
  ${TEMP}/cthead1-label-regionreference.mha
  )

add_test(itkBinaryImageToShapeLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/Spots-binaryimage-to-shapelabel.mha
            ${TEMP}/Spots-binaryimage-to-shapelabel.mha
  itkBinaryImageToShapeLabelMapFilterTest1
  ${INPUTDATA}/Spots.png
  ${TEMP}/Spots-binaryimage-to-shapelabel.mha
  1     # Fully connected (bool)
  255   # Input image foreground
  0     # Output image background
  1     # Compute Feret Diameter (bool)
  1     # Compute Perimeter (bool)
  )

add_test(itkShapeKeepNObjectsLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-keep-n-objects.mha
            ${TEMP}/cthead1-shape-keep-n-objects.mha
  itkShapeKeepNObjectsLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-shape-keep-n-objects.mha
  0         # Reverse Ordering (bool) - Objects with values smaller than Lambda are removed
  0         # Attribute
  2         # Number of objects to keep
  )

add_test(itkShapeOpeningLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-shape-opening.mha
            ${TEMP}/cthead1-shape-opening.mha
  itkShapeOpeningLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-shape-opening.mha
  140   # Lambda
  1     # Reverse Ordering (bool) - Objects with values smaller than Lambda are removed
  0     # Attribute
  )

add_test(itkBinaryShapeOpeningImageFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-binary-shape-opening.mha
            ${TEMP}/cthead1-binary-shape-opening.mha
  itkBinaryShapeOpeningImageFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-binary-shape-opening.mha
  255   # Input image foreground
  0     # Output image background
  140   # Lambda
  1     # Reverse Ordering (bool) - Objects with values smaller than Lambda are removed
  1     # Fully connected (bool)
  100   # Attribute (bool)
  )

add_test(itkShapeRelabelLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/cthead1-shape-relabel-labelmap.mha
            ${TEMP}/cthead1-shape-relabel-labelmap.mha
  itkShapeRelabelLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-shape-relabel-labelmap.mha
  1     # Reverse Ordering (bool) - Objects with values smaller than Lambda are removed
  0     # Attribute
  )

add_test(itkShapeLabelObjectAccessorsTest1
  ${TestDriver}
  itkShapeLabelObjectAccessorsTest1
  ${INPUTDATA}/cthead1Label.png
  )

add_test(itkGridImageSourceTest1
  ${TestDriver}
  --compare ${BASELINE}/itkGridImageSourceTest1.nhdr
            ${TEMP}/itkGridImageSourceTest1.nhdr
  itkGridImageSourceTest ${TEMP}/itkGridImageSourceTest1.nhdr 0)

add_test(itkGridImageSourceTest2
  ${TestDriver}
  --compare ${BASELINE}/itkGridImageSourceTest2.nhdr
            ${TEMP}/itkGridImageSourceTest2.nhdr
  itkGridImageSourceTest ${TEMP}/itkGridImageSourceTest2.nhdr 1)

add_test(itkGridImageSourceTest3
  ${TestDriver}
  --compare ${BASELINE}/itkGridImageSourceTest3.nhdr
            ${TEMP}/itkGridImageSourceTest3.nhdr
  itkGridImageSourceTest ${TEMP}/itkGridImageSourceTest3.nhdr 2)

add_test(itkGridImageSourceTest4
  ${TestDriver}
  --compare ${BASELINE}/itkGridImageSourceTest4.nhdr
  ${TEMP}/itkGridImageSourceTest4.nhdr
  itkGridImageSourceTest ${TEMP}/itkGridImageSourceTest4.nhdr 3)

add_test(itkGaborImageSourceTest0
  ${TestDriver}
  --compare ${BASELINE}/GaborImageSourceTest0.mha
  ${TEMP}/GaborImageSourceTest0.mha
  itkGaborImageSourceTest ${TEMP}/GaborImageSourceTest0.mha 0)

add_test(itkGaborImageSourceTest1
  ${TestDriver}
  --compare ${BASELINE}/GaborImageSourceTest1.mha
  ${TEMP}/GaborImageSourceTest1.mha
  itkGaborImageSourceTest ${TEMP}/GaborImageSourceTest1.mha 1)

add_test(itkAttributeLabelObjectAccessorsTest1
  ${TestDriver}
  itkAttributeLabelObjectAccessorsTest1
  ${INPUTDATA}/cthead1Label.png
  ${INPUTDATA}/cthead1.png
  )

add_test(itkAttributeOpeningLabelMapFilterTest0
  ${TestDriver}
  --compare ${BASELINE}/itkAttributeOpeningLabelMapFilterTest0.png
            ${TEMP}/itkAttributeOpeningLabelMapFilterTest0.png
  itkAttributeOpeningLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/itkAttributeOpeningLabelMapFilterTest0.png
  2
  0
  )

add_test(itkAttributeOpeningLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkAttributeOpeningLabelMapFilterTest1.png
            ${TEMP}/itkAttributeOpeningLabelMapFilterTest1.png
  itkAttributeOpeningLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/itkAttributeOpeningLabelMapFilterTest1.png
  2
  1
  )

add_test(itkAttributeKeepNObjectsLabelMapFilterTest0
  ${TestDriver}
  --compare ${BASELINE}/itkAttributeKeepNObjectsLabelMapFilterTest0.png
            ${TEMP}/itkAttributeKeepNObjectsLabelMapFilterTest0.png
  itkAttributeKeepNObjectsLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/itkAttributeKeepNObjectsLabelMapFilterTest0.png
  2
  0
  )

add_test(itkAttributeKeepNObjectsLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkAttributeKeepNObjectsLabelMapFilterTest1.png
            ${TEMP}/itkAttributeKeepNObjectsLabelMapFilterTest1.png
  itkAttributeKeepNObjectsLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/itkAttributeKeepNObjectsLabelMapFilterTest1.png
  2
  1
  )

add_test(itkAttributeRelabelLabelMapFilterTest0
  ${TestDriver}
  --compare ${BASELINE}/itkAttributeRelabelLabelMapFilterTest0.png
            ${TEMP}/itkAttributeRelabelLabelMapFilterTest0.png
  itkAttributeRelabelLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/itkAttributeRelabelLabelMapFilterTest0.png
  0
  )

add_test(itkAttributeRelabelLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkAttributeRelabelLabelMapFilterTest1.png
            ${TEMP}/itkAttributeRelabelLabelMapFilterTest1.png
  itkAttributeRelabelLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/itkAttributeRelabelLabelMapFilterTest1.png
  1
  )

add_test(itkLabelSelectionLabelMapFilterTest0
  ${TestDriver}
  --compare ${BASELINE}/itkLabelSelectionLabelMapFilterTest0.png
            ${TEMP}/itkLabelSelectionLabelMapFilterTest0.png
  itkLabelSelectionLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/itkLabelSelectionLabelMapFilterTest0.png
  0
  94
  )

add_test(itkLabelSelectionLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkLabelSelectionLabelMapFilterTest1.png
            ${TEMP}/itkLabelSelectionLabelMapFilterTest1.png
  itkLabelSelectionLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/itkLabelSelectionLabelMapFilterTest1.png
  1
  94
  )

FOREACH(label 0 100)
  FOREACH(bg 0 1)
    FOREACH(neg 0 1)
      ADD_TEST(itkLabelMapMaskImageFilterTest-${label}-${bg}-${neg}
        ${TestDriver}
        --compare ${BASELINE}/itkLabelMapMaskImageFilterTest-${label}-${bg}-${neg}.png
                  ${TEMP}/itkLabelMapMaskImageFilterTest-${label}-${bg}-${neg}.png
        itkLabelMapMaskImageFilterTest
        ${INPUTDATA}/2th_cthead1.png ${INPUTDATA}/cthead1.png
        ${TEMP}/itkLabelMapMaskImageFilterTest-${label}-${bg}-${neg}.png
        ${label} ${bg} ${neg} 0 0
      )

      FOREACH(border 0 10)
        ADD_TEST(itkLabelMapMaskImageFilterTestCrop-${label}-${bg}-${neg}-${border}
          ${TestDriver}
          --compare ${BASELINE}/itkLabelMapMaskImageFilterTestCrop-${label}-${bg}-${neg}-${border}.png
                    ${TEMP}/itkLabelMapMaskImageFilterTestCrop-${label}-${bg}-${neg}-${border}.png
          itkLabelMapMaskImageFilterTest
          ${INPUTDATA}/2th_cthead1.png ${INPUTDATA}/cthead1.png
          ${TEMP}/itkLabelMapMaskImageFilterTestCrop-${label}-${bg}-${neg}-${border}.png
          ${label} ${bg} ${neg} 1 ${border}
        )
      ENDFOREACH(border)
    ENDFOREACH(neg)
  ENDFOREACH(bg)
ENDFOREACH(label)

add_test(itkObjectByObjectLabelMapFilterTest0
  ${TestDriver}
  --compare ${BASELINE}/itkObjectByObjectLabelMapFilterTest0.png
            ${TEMP}/itkObjectByObjectLabelMapFilterTest0.png
  itkObjectByObjectLabelMapFilterTest
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkObjectByObjectLabelMapFilterTest0.png
  0
  )

add_test(itkObjectByObjectLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkObjectByObjectLabelMapFilterTest1.png
            ${TEMP}/itkObjectByObjectLabelMapFilterTest1.png
  itkObjectByObjectLabelMapFilterTest
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkObjectByObjectLabelMapFilterTest1.png
  1
  )

add_test(itkAttributeUniqueLabelMapFilterTest0
  ${TestDriver}
  --compare ${BASELINE}/itkAttributeUniqueLabelMapFilterTest0.png
            ${TEMP}/itkAttributeUniqueLabelMapFilterTest0.png
  itkAttributeUniqueLabelMapFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkAttributeUniqueLabelMapFilterTest0.png
  0
  )

add_test(itkAttributeUniqueLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkAttributeUniqueLabelMapFilterTest1.png
            ${TEMP}/itkAttributeUniqueLabelMapFilterTest1.png
  itkAttributeUniqueLabelMapFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkAttributeUniqueLabelMapFilterTest1.png
  1
  )

add_test(itkLabelUniqueLabelMapFilterTest0
  ${TestDriver}
  --compare ${BASELINE}/itkLabelUniqueLabelMapFilterTest0.png
            ${TEMP}/itkLabelUniqueLabelMapFilterTest0.png
  itkLabelUniqueLabelMapFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkLabelUniqueLabelMapFilterTest0.png
  0
  )

add_test(itkLabelUniqueLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkLabelUniqueLabelMapFilterTest1.png
            ${TEMP}/itkLabelUniqueLabelMapFilterTest1.png
  itkLabelUniqueLabelMapFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkLabelUniqueLabelMapFilterTest1.png
  1
  )

add_test(itkLabelMapToAttributeImageFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkLabelMapToAttributeImageFilterTest1.png
            ${TEMP}/itkLabelMapToAttributeImageFilterTest1.png
  itkLabelMapToAttributeImageFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkLabelMapToAttributeImageFilterTest1.png
  )

add_test(itkLabelMapToRGBImageFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkLabelMapToRGBImageFilterTest1.png
            ${TEMP}/itkLabelMapToRGBImageFilterTest1.png
  itkLabelMapToRGBImageFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkLabelMapToRGBImageFilterTest1.png
  )

add_test(itkLabelMapToRGBImageFilterTest2
  ${TestDriver}
  --compare ${BASELINE}/itkLabelMapToRGBImageFilterTest1.png
            ${TEMP}/itkLabelMapToRGBImageFilterTest1.png
  itkLabelMapToRGBImageFilterTest2
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkLabelMapToRGBImageFilterTest1.png
  )


add_test(itkLabelMapOverlayImageFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkLabelMapOverlayImageFilterTest1.png
            ${TEMP}/itkLabelMapOverlayImageFilterTest1.png
  itkLabelMapOverlayImageFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkLabelMapOverlayImageFilterTest1.png
  0.3
  )

add_test(itkLabelMapOverlayImageFilterTest2
  ${TestDriver}
  --compare ${BASELINE}/itkLabelMapOverlayImageFilterTest1.png
            ${TEMP}/itkLabelMapOverlayImageFilterTest1.png
  itkLabelMapOverlayImageFilterTest2
  ${INPUTDATA}/cthead1Label.png
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkLabelMapOverlayImageFilterTest1.png
  0.3
  )

add_test(itkLabelMapContourOverlayImageFilterTest0
  ${TestDriver}
  --compare ${BASELINE}/itkLabelMapContourOverlayImageFilterTest0.png
            ${TEMP}/itkLabelMapContourOverlayImageFilterTest0.png
  itkLabelMapContourOverlayImageFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${INPUTDATA}/SpotsInverted.png
  ${TEMP}/itkLabelMapContourOverlayImageFilterTest0.png
  0.6  # opacity
  0    # type (plain)
  0    # thickness (irrelevant with that type)
  10   # dilation radius
  1    # priority
  )

add_test(itkLabelMapContourOverlayImageFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkLabelMapContourOverlayImageFilterTest1.png
            ${TEMP}/itkLabelMapContourOverlayImageFilterTest1.png
  itkLabelMapContourOverlayImageFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${INPUTDATA}/SpotsInverted.png
  ${TEMP}/itkLabelMapContourOverlayImageFilterTest1.png
  0.7  # opacity
  1    # type (contour)
  2    # thickness
  15   # dilation radius
  0    # priority
  )

add_test(itkLabelMapContourOverlayImageFilterTest2
  ${TestDriver}
  --compare ${BASELINE}/itkLabelMapContourOverlayImageFilterTest2.png
            ${TEMP}/itkLabelMapContourOverlayImageFilterTest2.png
  itkLabelMapContourOverlayImageFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${INPUTDATA}/SpotsInverted.png
  ${TEMP}/itkLabelMapContourOverlayImageFilterTest2.png
  0.8  # opacity
  2    # type (slice contour)
  3    # thickness (irrelevant with that type)
  12   # dilation radius
  1    # priority
  )

add_test(itkBinaryFillholeImageFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkBinaryFillholeImageFilterTest1.png
            ${TEMP}/itkBinaryFillholeImageFilterTest1.png
  itkBinaryFillholeImageFilterTest1
  ${INPUTDATA}/Spots.png
  ${TEMP}/itkBinaryFillholeImageFilterTest1.png
  1      # connectivity
  255    # fg
  )

add_test(itkBinaryGrindPeakImageFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkBinaryGrindPeakImageFilterTest1.png
            ${TEMP}/itkBinaryGrindPeakImageFilterTest1.png
  itkBinaryGrindPeakImageFilterTest1
  ${INPUTDATA}/SpotsInverted.png
  ${TEMP}/itkBinaryGrindPeakImageFilterTest1.png
  1      # connectivity
  255    # fg
  )

add_test(itkShapePositionLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkShapePositionLabelMapFilterTest1.png
            ${TEMP}/itkShapePositionLabelMapFilterTest1.png
  itkShapePositionLabelMapFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkShapePositionLabelMapFilterTest1.png
  Centroid
  )

add_test(itkStatisticsPositionLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkShapePositionLabelMapFilterTest1.png
            ${TEMP}/itkStatisticsPositionLabelMapFilterTest1.png
  itkStatisticsPositionLabelMapFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkStatisticsPositionLabelMapFilterTest1.png
  CenterOfGravity
  )

add_test(itkAttributePositionLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/itkAttributePositionLabelMapFilterTest1.png
            ${TEMP}/itkAttributePositionLabelMapFilterTest1.png
  itkAttributePositionLabelMapFilterTest1
  ${INPUTDATA}/SpotsLabeled.png
  ${TEMP}/itkAttributePositionLabelMapFilterTest1.png
  )

add_test(itkBinaryReconstructionLabelMapFilterTest
  ${TestDriver}
  --compare ${BASELINE}/itkBinaryReconstructionLabelMapFilterTest.png
            ${TEMP}/itkBinaryReconstructionLabelMapFilterTest.png
  itkBinaryReconstructionLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${INPUTDATA}/cthead1-markers.png
  ${TEMP}/itkBinaryReconstructionLabelMapFilterTest.png
  94
  )

add_test(itkBinaryReconstructionByDilationImageFilterTest
  ${TestDriver}
  --compare ${BASELINE}/itkBinaryReconstructionByDilationImageFilterTest.png
            ${TEMP}/itkBinaryReconstructionByDilationImageFilterTest.png
  itkBinaryReconstructionByDilationImageFilterTest
  ${INPUTDATA}/SpotsInverted.png
  ${INPUTDATA}/Spots-markers.png
  ${TEMP}/itkBinaryReconstructionByDilationImageFilterTest.png
  255
  100
  )

add_test(itkBinaryReconstructionByErosionImageFilterTest
  ${TestDriver}
  --compare ${BASELINE}/itkBinaryReconstructionByErosionImageFilterTest.png
            ${TEMP}/itkBinaryReconstructionByErosionImageFilterTest.png
  itkBinaryReconstructionByErosionImageFilterTest
  ${INPUTDATA}/SpotsLabeled.png
  ${INPUTDATA}/Spots-markers.png
  ${TEMP}/itkBinaryReconstructionByErosionImageFilterTest.png
  0
  255
  )

add_test(itkBinaryClosingByReconstructionImageFilterTest
  ${TestDriver}
  --compare ${BASELINE}/itkBinaryClosingByReconstructionImageFilterTest.png
            ${TEMP}/itkBinaryClosingByReconstructionImageFilterTest.png
  itkBinaryClosingByReconstructionImageFilterTest
  ${INPUTDATA}/cthead1-bin.png
  ${TEMP}/itkBinaryClosingByReconstructionImageFilterTest.png
  0   # connectivity
  255 # bg
  10  # kernel size
  )

add_test(itkBinaryOpeningByReconstructionImageFilterTest
  ${TestDriver}
  --compare ${BASELINE}/itkBinaryOpeningByReconstructionImageFilterTest.png
            ${TEMP}/itkBinaryOpeningByReconstructionImageFilterTest.png
  itkBinaryOpeningByReconstructionImageFilterTest
  ${INPUTDATA}/cthead1-bin.png
  ${TEMP}/itkBinaryOpeningByReconstructionImageFilterTest.png
  0   # connectivity
  255 # bg
  10  # kernel size
  )

add_test(itkBinaryImageToStatisticsLabelMapFilterTest1
  ${TestDriver}
  --compare ${BASELINE}/Spots-binaryimage-to-statisticslabel.mha
            ${TEMP}/Spots-binaryimage-to-statisticslabel.mha
  itkBinaryImageToStatisticsLabelMapFilterTest1
  ${INPUTDATA}/Spots.png
  ${INPUTDATA}/Spots.png
  ${TEMP}/Spots-binaryimage-to-statisticslabel.mha
  1     # Fully connected (bool)
  0     # Input image foreground
  0     # Output image background
  1     # Compute Feret Diameter (bool)
  1     # Compute Perimeter (bool)
  1     # Compute Histogram (bool)
  128   # Number Of Bins
  )

add_test(itkLabelImageToStatisticsLabelMapFilterTest1
  ${TestDriver}
  --compare ${INPUTDATA}/Spots.png
            ${TEMP}/Spots-labelimage-to-statisticslabel.png
  itkLabelImageToStatisticsLabelMapFilterTest1
  ${INPUTDATA}/Spots.png
  ${INPUTDATA}/Spots.png
  ${TEMP}/Spots-labelimage-to-statisticslabel.png
  0     # Image background
  1     # Compute Feret Diameter (bool)
  1     # Compute Perimeter (bool)
  1     # Compute Histogram (bool)
  128   # Number Of Bins
  )

add_test(itkShiftLabelObjectTest
  ${TestDriver}
  itkShiftLabelObjectTest
  )

add_test(itkLabelGeometryImageFilterTest2
  ${TestDriver}
  --compare ${BASELINE}/itkLabelGeometryImageFilterTest2Image.png
            ${TEMP}/itkLabelGeometryImageFilterTest2Image.png
  itkLabelGeometryImageFilterTest
  ${INPUTDATA}/BinaryImageWithVariousShapes01.png
  ${TEMP}/itkLabelGeometryImageFilterTest2Image.png 2
  ${INPUTDATA}/GrayscaleImageWithVariousShapes01.png
  )

add_test(itkObjectToObjectMetricTest ${TestDriver}
         itkObjectToObjectMetricTest)
add_test(itkDeformationFieldTransformTest ${TestDriver}
  itkDeformationFieldTransformTest)
add_test(itkCompositeTransformTest ${TestDriver} itkCompositeTransformTest)

add_test(itkBSplineControlPointImageFilterTest1
  ${TestDriver}
  --compare
  ${TEMP}/N4ControlPoints_2D_output.nii.gz
  ${TEMP}/N4ControlPoints_2D_outputRefined.nii.gz
  itkBSplineControlPointImageFilterTest
  2
  ${BASELINE}/N4ControlPoints_2D.nii.gz                       # control point lattice
  ${TEMP}/N4ControlPoints_2D_output.nii.gz
  ${TEMP}/N4ControlPoints_2D_outputRefined.nii.gz
  )

add_test(itkBSplineControlPointImageFilterTest2
  ${TestDriver}
  --compare
  ${TEMP}/N4ControlPoints_3D_output.nii.gz
  ${TEMP}/N4ControlPoints_3D_outputRefined.nii.gz
  itkBSplineControlPointImageFilterTest
  3
  ${BASELINE}/N4ControlPoints_3D.nii.gz                       # control point lattice
  ${TEMP}/N4ControlPoints_3D_output.nii.gz
  ${TEMP}/N4ControlPoints_3D_outputRefined.nii.gz
  )

add_test(itkBSplineControlPointImageFunctionTest
  ${TestDriver}
  itkBSplineControlPointImageFunctionTest
  )

add_test(itkN4MRIBiasFieldCorrectionImageFilterTest1
  ${TestDriver}
  --compare
  ${BASELINE}/N4ControlPoints_2D.nii.gz
  ${TEMP}/N4ControlPoints_2D.nii.gz
  itkN4MRIBiasFieldCorrectionImageFilterTest
  2
  ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png    # input
  ${TEMP}/N4ControlPoints_2D.nii.gz                           # control point lattice
  4                                                           # shrink factor
  20x20                                                       # number of iterations
  none                                                        # mask
  200                                                         # spline distance
  )

add_test(itkN4MRIBiasFieldCorrectionImageFilterTest2
  ${TestDriver}
  --compare
  ${BASELINE}/N4ControlPoints_3D.nii.gz
  ${TEMP}/N4ControlPoints_3D.nii.gz
  itkN4MRIBiasFieldCorrectionImageFilterTest
  3
  ${ITK_SOURCE_DIR}/Testing/Data/Input/HeadMRVolumeCompressed.mha    # input
  ${TEMP}/N4ControlPoints_3D.nii.gz                                  # control point lattice
  3                                                                  # shrink factor
  10x10x10                                                           # number of iterations
  none                                                               # mask
  150                                                                # spline distance
  )

add_test(itkScalarChanAndVeseLevelSetFunctionTest1 ${TestDriver}
  itkScalarChanAndVeseLevelSetFunctionTest1)
add_test(itkScalarChanAndVeseLevelSetFunctionTest2 ${TestDriver}
  itkScalarChanAndVeseLevelSetFunctionTest2)
add_test(itkScalarChanAndVeseDenseLevelSetImageFilterTest1 ${TestDriver}
  itkScalarChanAndVeseDenseLevelSetImageFilterTest1)
add_test(itkScalarChanAndVeseSparseLevelSetImageFilterTest1 ${TestDriver}
  itkScalarChanAndVeseSparseLevelSetImageFilterTest1)
add_test(itkScalarChanAndVeseSparseLevelSetImageFilterTest2 ${TestDriver}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
            ${TEMP}/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
  itkScalarChanAndVeseSparseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
)
add_test(itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0 ${TestDriver}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
            ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
  1 1 0 0 0
)
add_test(itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1 ${TestDriver}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
            ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
  1 1 1 1 200
)
add_test(itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2 ${TestDriver}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
            ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
  0 0 1 0 0
)
add_test(itkScalarChanAndVeseDenseLevelSetImageFilterTest2_3 ${TestDriver}
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_3.mha
  0 0 0 0 0
)

add_test(itkScalarChanAndVeseDenseLevelSetImageFilterTest3 ${TestDriver}
  itkScalarChanAndVeseDenseLevelSetImageFilterTest3)

add_test(itkScalarChanAndVeseDenseLevelSetImageFilterTest4 ${TestDriver}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
            ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest4.mha
  itkScalarChanAndVeseDenseLevelSetImageFilterTest4
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest4.mha
  1 1 0 0 0
)

add_test(itkMRCImageIOTest ${TestDriver} itkMRCImageIOTest ${TEMP})

add_test(itkJPEG2000Test00
  ${TestDriver} itkJPEG2000ImageIORegionOfInterest
  ${INPUTDATA}/Bretagne1.j2k
  ${TEMP}/Bretagne1_RegionTest01.tif
  160 120
  320 240
  )

# tests for reading 3 component data with 8bits per component
add_test(itkJPEG2000Test01  ${TestDriver} itkJPEG2000ImageIOTest03 ${INPUTDATA}/Bretagne1.j2k ${TEMP}/Bretagne1_01.png )
add_test(itkJPEG2000Test02  ${TestDriver} itkJPEG2000ImageIOTest03 ${INPUTDATA}/Cevennes2.jp2 ${TEMP}/Cevennes2_01.png )

# tests for writing 3 component data with 8bits per component
add_test(itkJPEG2000Test03  ${TestDriver} itkJPEG2000ImageIOTest03 ${TEMP}/Bretagne1_RegionTest01.tif ${TEMP}/Bretagne1_02.j2k )
set_tests_properties( itkJPEG2000Test03 PROPERTIES DEPENDS itkJPEG2000Test00 )

add_test(itkJPEG2000Test05  ${TestDriver} itkJPEG2000ImageIOTest05 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
  ${ITK_TEST_OUTPUT_DIR} jp2 )

add_test(itkJPEG2000Test06 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
  itkJPEG2000ImageIOTest06
            ${ITK_DATA_ROOT}/Input/cthead1.j2k
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)


#
# Function colormaps
#
foreach( COLORMAP red green blue hot cool spring summer autumn winter copper hot hsv jet overunder )
  add_test( RGBColormapTest_${COLORMAP}
     ${TestDriver}
     --compare ${BASELINE}/RGBColormapTest_${COLORMAP}.png ${TEMP}/RGBColormapTest_${COLORMAP}.png
     itkScalarToRGBColormapImageFilterTest
     ${INPUTDATA}/Colormap_Grey.png ${TEMP}/RGBColormapTest_${COLORMAP}.png ${COLORMAP} )
endforeach(COLORMAP)


add_executable(itkReviewPrintTest itkReviewPrintTest.cxx)
target_link_libraries(itkReviewPrintTest
  ITKAlgorithms
  ITKFEM
  ITKCommon
  )

add_executable(itkReviewHeaderTest itkReviewHeaderTest.cxx)
target_link_libraries(itkReviewHeaderTest
  ITKAlgorithms
  ITKFEM
  ITKCommon
  )

add_test(itkReviewHeaderTest ${REVIEW_HEADER_TESTS})

endif(NOT ITK_DISABLE_CXX_TESTING)
