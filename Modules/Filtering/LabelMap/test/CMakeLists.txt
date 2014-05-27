itk_module_test()
set(ITKLabelMapTests
itkAggregateLabelMapFilterTest.cxx
itkAttributeKeepNObjectsLabelMapFilterTest1.cxx
itkAttributeLabelObjectAccessorsTest1.cxx
itkAttributeOpeningLabelMapFilterTest1.cxx
itkAttributePositionLabelMapFilterTest1.cxx
itkAttributeRelabelLabelMapFilterTest1.cxx
itkAttributeUniqueLabelMapFilterTest1.cxx
itkAutoCropLabelMapFilterTest1.cxx
itkAutoCropLabelMapFilterTest2
itkBinaryFillholeImageFilterTest1.cxx
itkBinaryGrindPeakImageFilterTest1.cxx
itkBinaryImageToLabelMapFilterTest.cxx
itkBinaryImageToShapeLabelMapFilterTest1.cxx
itkBinaryImageToStatisticsLabelMapFilterTest1.cxx
itkBinaryReconstructionByDilationImageFilterTest.cxx
itkBinaryReconstructionByErosionImageFilterTest.cxx
itkBinaryReconstructionLabelMapFilterTest.cxx
itkBinaryShapeKeepNObjectsImageFilterTest1.cxx
itkBinaryShapeOpeningImageFilterTest1.cxx
itkBinaryStatisticsKeepNObjectsImageFilterTest1.cxx
itkBinaryStatisticsOpeningImageFilterTest1.cxx
itkChangeLabelLabelMapFilterTest.cxx
itkChangeRegionLabelMapFilterTest1.cxx
itkConvertLabelMapFilterTest1.cxx
itkCropLabelMapFilterTest1.cxx
itkLabelImageToLabelMapFilterTest.cxx
itkLabelImageToShapeLabelMapFilterTest1.cxx
itkLabelImageToStatisticsLabelMapFilterTest1.cxx
itkLabelMapFilterTest.cxx
itkLabelMapMaskImageFilterTest.cxx
itkLabelMapTest.cxx
itkLabelMapTest2.cxx
itkLabelMapToAttributeImageFilterTest1.cxx
itkLabelMapToBinaryImageFilterTest.cxx
itkLabelMapToLabelImageFilterTest.cxx
itkLabelObjectLineComparatorTest.cxx
itkLabelObjectLineTest.cxx
itkLabelObjectTest.cxx
itkLabelSelectionLabelMapFilterTest.cxx
itkLabelShapeKeepNObjectsImageFilterTest1.cxx
itkLabelShapeOpeningImageFilterTest1.cxx
itkLabelStatisticsKeepNObjectsImageFilterTest1.cxx
itkLabelStatisticsOpeningImageFilterTest1.cxx
itkLabelUniqueLabelMapFilterTest1.cxx
itkMergeLabelMapFilterTest1.cxx
itkObjectByObjectLabelMapFilterTest.cxx
itkPadLabelMapFilterTest1.cxx
itkRegionFromReferenceLabelMapFilterTest1.cxx
itkRelabelLabelMapFilterTest1.cxx
itkShapeKeepNObjectsLabelMapFilterTest1.cxx
itkShapeLabelObjectAccessorsTest1.cxx
itkShapeOpeningLabelMapFilterTest1.cxx
itkShapePositionLabelMapFilterTest1.cxx
itkShapeRelabelImageFilterTest1.cxx
itkShapeRelabelLabelMapFilterTest1.cxx
itkShapeUniqueLabelMapFilterTest1.cxx
itkShiftLabelObjectTest.cxx
itkShiftScaleLabelMapFilterTest1.cxx
itkStatisticsKeepNObjectsLabelMapFilterTest1.cxx
itkStatisticsOpeningLabelMapFilterTest1.cxx
itkStatisticsPositionLabelMapFilterTest1.cxx
itkStatisticsRelabelImageFilterTest1.cxx
itkStatisticsRelabelLabelMapFilterTest1.cxx
itkStatisticsUniqueLabelMapFilterTest1.cxx
)

CreateTestDriver(ITKLabelMap  "${ITKLabelMap-Test_LIBRARIES}" "${ITKLabelMapTests}")

itk_add_test(NAME itkShiftLabelObjectTest
      COMMAND ITKLabelMapTestDriver itkShiftLabelObjectTest)
itk_add_test(NAME itkAggregateLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-labelAggregate.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-labelAggregate.mha
    itkAggregateLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-labelAggregate.mha)
itk_add_test(NAME itkAttributeKeepNObjectsLabelMapFilterTest0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributeKeepNObjectsLabelMapFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeKeepNObjectsLabelMapFilterTest0.png
    itkAttributeKeepNObjectsLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributeKeepNObjectsLabelMapFilterTest0.png 2 0)
itk_add_test(NAME itkAttributeKeepNObjectsLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributeKeepNObjectsLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeKeepNObjectsLabelMapFilterTest1.png
    itkAttributeKeepNObjectsLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributeKeepNObjectsLabelMapFilterTest1.png 2 1)
itk_add_test(NAME itkAttributeLabelObjectAccessorsTest1
      COMMAND ITKLabelMapTestDriver itkAttributeLabelObjectAccessorsTest1
              DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png})
itk_add_test(NAME itkAttributeOpeningLabelMapFilterTest0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributeOpeningLabelMapFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeOpeningLabelMapFilterTest0.png
    itkAttributeOpeningLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributeOpeningLabelMapFilterTest0.png 2 0)
itk_add_test(NAME itkAttributeOpeningLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributeOpeningLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeOpeningLabelMapFilterTest1.png
    itkAttributeOpeningLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributeOpeningLabelMapFilterTest1.png 2 1)
itk_add_test(NAME itkAttributePositionLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributePositionLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributePositionLabelMapFilterTest1.png
    itkAttributePositionLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributePositionLabelMapFilterTest1.png)
itk_add_test(NAME itkAttributeRelabelLabelMapFilterTest0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributeRelabelLabelMapFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeRelabelLabelMapFilterTest0.png
    itkAttributeRelabelLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributeRelabelLabelMapFilterTest0.png 0)
itk_add_test(NAME itkAttributeRelabelLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributeRelabelLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeRelabelLabelMapFilterTest1.png
    itkAttributeRelabelLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributeRelabelLabelMapFilterTest1.png 1)
itk_add_test(NAME itkAttributeUniqueLabelMapFilterTest0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributeUniqueLabelMapFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeUniqueLabelMapFilterTest0.png
    itkAttributeUniqueLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributeUniqueLabelMapFilterTest0.png 0)
itk_add_test(NAME itkAttributeUniqueLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAttributeUniqueLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAttributeUniqueLabelMapFilterTest1.png
    itkAttributeUniqueLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkAttributeUniqueLabelMapFilterTest1.png 1)
itk_add_test(NAME itkAutoCropLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-autocrop.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-autocrop.mha
    itkAutoCropLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-autocrop.mha 176 4 4)
itk_add_test(NAME itkBinaryFillholeImageFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkBinaryFillholeImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest1.png
    itkBinaryFillholeImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/Spots.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryFillholeImageFilterTest1.png 1 255)
itk_add_test(NAME itkBinaryGrindPeakImageFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkBinaryGrindPeakImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryGrindPeakImageFilterTest1.png
    itkBinaryGrindPeakImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryGrindPeakImageFilterTest1.png 1 255)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-labeled.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-labeled.mha
    itkBinaryImageToLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1-mask.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-labeled.mha 1 255 0 0)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest2
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/connected0lines-0.png}
              ${ITK_TEST_OUTPUT_DIR}/connected0lines-0.png
    itkBinaryImageToLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/connected0lines.png} ${ITK_TEST_OUTPUT_DIR}/connected0lines-0.png 0 255 0 0)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest3
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/connected0lines-1.png}
              ${ITK_TEST_OUTPUT_DIR}/connected0lines-1.png
    itkBinaryImageToLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/connected0lines.png} ${ITK_TEST_OUTPUT_DIR}/connected0lines-1.png 1 255 0 0)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest4
      COMMAND ITKLabelMapTestDriver itkBinaryImageToLabelMapFilterTest
              DATA{${ITK_DATA_ROOT}/Input/289.png} ${ITK_TEST_OUTPUT_DIR}/tooManyObjects.png 0 255 0 1)
itk_add_test(NAME itkBinaryImageToLabelMapFilterTest5
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/nonConnected3DLines-0.tif}
              ${ITK_TEST_OUTPUT_DIR}/nonConnected3DLines-0.tif
    itkBinaryImageToLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/nonConnected3DLines.tif} ${ITK_TEST_OUTPUT_DIR}/nonConnected3DLines-0.tif 0 255 0 0)
itk_add_test(NAME itkBinaryImageToShapeLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/Spots-binaryimage-to-shapelabel.mha}
              ${ITK_TEST_OUTPUT_DIR}/Spots-binaryimage-to-shapelabel.mha
    itkBinaryImageToShapeLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/Spots.png} ${ITK_TEST_OUTPUT_DIR}/Spots-binaryimage-to-shapelabel.mha 1 255 0 1 1)
itk_add_test(NAME itkBinaryImageToStatisticsLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/Spots-binaryimage-to-statisticslabel.mha}
              ${ITK_TEST_OUTPUT_DIR}/Spots-binaryimage-to-statisticslabel.mha
    itkBinaryImageToStatisticsLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/Spots.png} DATA{${ITK_DATA_ROOT}/Input/Spots.png} ${ITK_TEST_OUTPUT_DIR}/Spots-binaryimage-to-statisticslabel.mha 1 0 0 1 1 1 128)
itk_add_test(NAME itkBinaryReconstructionByDilationImageFilterTest
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkBinaryReconstructionByDilationImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionByDilationImageFilterTest.png
    itkBinaryReconstructionByDilationImageFilterTest DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} DATA{${ITK_DATA_ROOT}/Input/Spots-markers.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionByDilationImageFilterTest.png 255 100)
itk_add_test(NAME itkBinaryReconstructionByErosionImageFilterTest
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkBinaryReconstructionByErosionImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionByErosionImageFilterTest.png
    itkBinaryReconstructionByErosionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} DATA{${ITK_DATA_ROOT}/Input/Spots-markers.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionByErosionImageFilterTest.png 0 255)
itk_add_test(NAME itkBinaryReconstructionLabelMapFilterTest
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkBinaryReconstructionLabelMapFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionLabelMapFilterTest.png
    itkBinaryReconstructionLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-markers.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryReconstructionLabelMapFilterTest.png 94)
itk_add_test(NAME itkBinaryShapeOpeningImageFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-binary-shape-opening.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-binary-shape-opening.mha
    itkBinaryShapeOpeningImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-binary-shape-opening.mha 255 0 140 1 1 100)
itk_add_test(NAME itkBinaryStatisticsOpeningImageFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{Baseline/cthead1-statistics-object-removed-baseline.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryStatisticsOpeningImageFilterTest1.png
    itkBinaryStatisticsOpeningImageFilterTest1 DATA{Input/cthead1-bin.png} DATA{Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBinaryStatisticsOpeningImageFilterTest1.png 1 0 1 0 0 202)
itk_add_test(NAME itkChangeLabelLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-changed.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed.mha
    itkChangeLabelLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed.mha 94 150 92 163 176 176)
itk_add_test(NAME itkChangeLabelLabelMapFilterTest2
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-changed2.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed2.mha
    itkChangeLabelLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed2.mha 94 100 0 50 92 0)
itk_add_test(NAME itkChangeLabelLabelMapFilterTest3
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-changed3.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed3.png
    itkChangeLabelLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changed3.png 0 94)
itk_add_test(NAME itkChangeRegionLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-changeregion.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changeregion.mha
    itkChangeRegionLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-changeregion.mha 50 100 120 150)
itk_add_test(NAME itkConvertLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
  --compare DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png}
            ${ITK_TEST_OUTPUT_DIR}/cthead1-convert.png
  itkConvertLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png}
  ${ITK_TEST_OUTPUT_DIR}/cthead1-convert.png
  )
itk_add_test(NAME itkCropLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-crop.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-crop.mha
    itkCropLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-crop.mha 40 50)
itk_add_test(NAME itkLabelImageToLabelMapFilterTest
      COMMAND ITKLabelMapTestDriver itkLabelImageToLabelMapFilterTest)
itk_add_test(NAME itkLabelImageToShapeLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/simple-label-to-shapelabelmap.mha}
              ${ITK_TEST_OUTPUT_DIR}/simple-label-to-shapelabelmap.mha
    itkLabelImageToShapeLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/simple-label-b.png} ${ITK_TEST_OUTPUT_DIR}/simple-label-to-shapelabelmap.mha 90 1 1)
itk_add_test(NAME itkLabelImageToStatisticsLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Input/Spots.png}
              ${ITK_TEST_OUTPUT_DIR}/Spots-labelimage-to-statisticslabel.png
    itkLabelImageToStatisticsLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/Spots.png} DATA{${ITK_DATA_ROOT}/Input/Spots.png} ${ITK_TEST_OUTPUT_DIR}/Spots-labelimage-to-statisticslabel.png 0 1 1 1 128)
itk_add_test(NAME itkLabelMapFilterTest
      COMMAND ITKLabelMapTestDriver itkLabelMapFilterTest)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-0-0-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-0-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-0-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-0-0.png 0 0 0 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-0-0-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-0-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-0-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-0-0.png 0 0 0 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-0-0-10
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-0-0-10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-0-10.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-0-10.png 0 0 0 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-0-0-1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-0-0-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-0-1.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-0-1.png 0 0 1 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-0-1-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-0-1-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-1-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-1-0.png 0 0 1 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-0-1-10
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-0-1-10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-1-10.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-0-1-10.png 0 0 1 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-0-1-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-0-1-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-1-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-1-0.png 0 1 0 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-1-0-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-1-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-0-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-0-0.png 0 1 0 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-1-0-10
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-1-0-10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-0-10.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-0-10.png 0 1 0 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-0-1-1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-0-1-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-1-1.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-0-1-1.png 0 1 1 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-1-1-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-1-1-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-1-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-1-0.png 0 1 1 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-0-1-1-10
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-0-1-1-10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-1-10.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-0-1-1-10.png 0 1 1 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-100-0-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-100-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-0-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-0-0.png 100 0 0 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-0-0-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-0-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-0-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-0-0.png 100 0 0 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-0-0-10
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-0-0-10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-0-10.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-0-10.png 100 0 0 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-100-0-1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-100-0-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-0-1.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-0-1.png 100 0 1 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-0-1-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-0-1-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-1-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-1-0.png 100 0 1 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-0-1-10
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-0-1-10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-1-10.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-0-1-10.png 100 0 1 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-100-1-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-100-1-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-1-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-1-0.png 100 1 0 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-1-0-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-1-0-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-0-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-0-0.png 100 1 0 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-1-0-10
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-1-0-10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-0-10.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-0-10.png 100 1 0 1 10)
itk_add_test(NAME itkLabelMapMaskImageFilterTest-100-1-1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTest-100-1-1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-1-1.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTest-100-1-1.png 100 1 1 0 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-1-1-0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-1-1-0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-1-0.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-1-0.png 100 1 1 1 0)
itk_add_test(NAME itkLabelMapMaskImageFilterTestCrop-100-1-1-10
      COMMAND ITKLabelMapTestDriver
    --compare  DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapMaskImageFilterTestCrop-100-1-1-10.png}
               ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-1-10.png
    itkLabelMapMaskImageFilterTest DATA{${ITK_DATA_ROOT}/Input/2th_cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapMaskImageFilterTestCrop-100-1-1-10.png 100 1 1 1 10)
itk_add_test(NAME itkLabelMapTest
      COMMAND ITKLabelMapTestDriver itkLabelMapTest)
itk_add_test(NAME itkLabelMapTest2
      COMMAND ITKLabelMapTestDriver itkLabelMapTest2)
itk_add_test(NAME itkLabelMapToAttributeImageFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelMapToAttributeImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToAttributeImageFilterTest1.png
    itkLabelMapToAttributeImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToAttributeImageFilterTest1.png)
itk_add_test(NAME itkLabelMapToBinaryImageFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-binary.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-binary.mha
    itkLabelMapToBinaryImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-binary.mha 255 0)
itk_add_test(NAME itkLabelMapToLabelImageFilterTest
      COMMAND ITKLabelMapTestDriver itkLabelMapToLabelImageFilterTest)
itk_add_test(NAME itkLabelObjectLineComparatorTest
      COMMAND ITKLabelMapTestDriver itkLabelObjectLineComparatorTest)
itk_add_test(NAME itkLabelObjectLineTest
      COMMAND ITKLabelMapTestDriver itkLabelObjectLineTest)
itk_add_test(NAME itkLabelObjectTest
      COMMAND ITKLabelMapTestDriver itkLabelObjectTest)
itk_add_test(NAME itkLabelSelectionLabelMapFilterTest0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelSelectionLabelMapFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelSelectionLabelMapFilterTest0.png
    itkLabelSelectionLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelSelectionLabelMapFilterTest0.png 0 94)
itk_add_test(NAME itkLabelSelectionLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelSelectionLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelSelectionLabelMapFilterTest1.png
    itkLabelSelectionLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelSelectionLabelMapFilterTest1.png 1 94)
itk_add_test(NAME itkLabelUniqueLabelMapFilterTest0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelUniqueLabelMapFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelUniqueLabelMapFilterTest0.png
    itkLabelUniqueLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelUniqueLabelMapFilterTest0.png 0)
itk_add_test(NAME itkLabelUniqueLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkLabelUniqueLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelUniqueLabelMapFilterTest1.png
    itkLabelUniqueLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelUniqueLabelMapFilterTest1.png 1)
itk_add_test(NAME itkMergeLabelMapFilterTest1a
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/simple-label-merge-keep.mha}
              ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-keep.mha
    itkMergeLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/simple-label-c.png} DATA{${ITK_DATA_ROOT}/Input/simple-label-d.png} ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-keep.mha 50 50 0 0)
itk_add_test(NAME itkMergeLabelMapFilterTest1b
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/simple-label-merge-aggregate.mha}
              ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-aggregate.mha
    itkMergeLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/simple-label-c.png} DATA{${ITK_DATA_ROOT}/Input/simple-label-d.png} ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-aggregate.mha 50 50 1 0)
itk_add_test(NAME itkMergeLabelMapFilterTest1c
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/simple-label-merge-pack.mha}
              ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-pack.mha
    itkMergeLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/simple-label-c.png} DATA{${ITK_DATA_ROOT}/Input/simple-label-d.png} ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-pack.mha 50 50 2 0)
itk_add_test(NAME itkMergeLabelMapFilterTest1d
      COMMAND ITKLabelMapTestDriver itkMergeLabelMapFilterTest1
              DATA{${ITK_DATA_ROOT}/Input/simple-label-c.png} DATA{${ITK_DATA_ROOT}/Input/simple-label-d.png} ${ITK_TEST_OUTPUT_DIR}/thisFileShouldNotExist.mha 50 50 3 1)
itk_add_test(NAME itkMergeLabelMapFilterTest1e
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/simple-label-merge-strict.mha}
              ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-strict.mha
    itkMergeLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/simple-label-e.png} DATA{${ITK_DATA_ROOT}/Input/simple-label-d.png} ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-strict.mha 50 50 3 0)
itk_add_test(NAME itkMergeLabelMapFilterTest1f
      COMMAND ITKLabelMapTestDriver itkMergeLabelMapFilterTest1
              DATA{${ITK_DATA_ROOT}/Input/simple-label-e.png} DATA{${ITK_DATA_ROOT}/Input/simple-label-d.png} ${ITK_TEST_OUTPUT_DIR}/simple-label-merge-wrong.mha 50 50 10 1)
itk_add_test(NAME itkObjectByObjectLabelMapFilterTest0
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkObjectByObjectLabelMapFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkObjectByObjectLabelMapFilterTest0.png
    itkObjectByObjectLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkObjectByObjectLabelMapFilterTest0.png 0)
itk_add_test(NAME itkObjectByObjectLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkObjectByObjectLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkObjectByObjectLabelMapFilterTest1.png
    itkObjectByObjectLabelMapFilterTest DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkObjectByObjectLabelMapFilterTest1.png 1)
itk_add_test(NAME itkPadLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-pad.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-pad.mha
    itkPadLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-pad.mha 40 50)
itk_add_test(NAME itkRegionFromReferenceLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-regionreference.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-regionreference.mha
    itkRegionFromReferenceLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} DATA{${ITK_DATA_ROOT}/Input/circle.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-regionreference.mha)
itk_add_test(NAME itkRelabelLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-relabeled.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-relabeled.mha
    itkRelabelLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-relabeled.mha)
itk_add_test(NAME itkShapeKeepNObjectsLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-keep-n-objects.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-keep-n-objects.mha
    itkShapeKeepNObjectsLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-keep-n-objects.mha 0 0 2)
itk_add_test(NAME itkShapeLabelObjectAccessorsTest1
      COMMAND ITKLabelMapTestDriver itkShapeLabelObjectAccessorsTest1
              DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png})
itk_add_test(NAME itkShapeOpeningLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-shape-opening.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-opening.mha
    itkShapeOpeningLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-opening.mha 140 1 0)
itk_add_test(NAME itkShapePositionLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkShapePositionLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkShapePositionLabelMapFilterTest1.png
    itkShapePositionLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkShapePositionLabelMapFilterTest1.png Centroid)
itk_add_test(NAME itkShapeRelabelLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-shape-relabel-labelmap.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-relabel-labelmap.mha
    itkShapeRelabelLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-relabel-labelmap.mha 1 0)
itk_add_test(NAME itkShapeUniqueLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-shape-unique-labelmap.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-unique-labelmap.mha
    itkShapeUniqueLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-shape-unique-labelmap.mha 1 0)
itk_add_test(NAME itkShiftScaleLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/cthead1-label-shiftscaled.mha}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-label-shiftscaled.mha
    itkShiftScaleLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1-label-shiftscaled.mha 10 0.5 true)
itk_add_test(NAME itkStatisticsPositionLabelMapFilterTest1
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkShapePositionLabelMapFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkStatisticsPositionLabelMapFilterTest1.png
    itkStatisticsPositionLabelMapFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkStatisticsPositionLabelMapFilterTest1.png CenterOfGravity)
itk_add_test(NAME itkAutoCropLabelMapFilterTest2
      COMMAND ITKLabelMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAutoCropLabelMapFilterTest2-92.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAutoCropLabelMapFilterTest2-92.png
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Review/itkAutoCropLabelMapFilterTest2-163.png}
              ${ITK_TEST_OUTPUT_DIR}/itkAutoCropLabelMapFilterTest2-163.png
    itkAutoCropLabelMapFilterTest2
    DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png}
    ${ITK_TEST_OUTPUT_DIR}/itkAutoCropLabelMapFilterTest2-92.png
    ${ITK_TEST_OUTPUT_DIR}/itkAutoCropLabelMapFilterTest2-163.png
    92
    163
    )
