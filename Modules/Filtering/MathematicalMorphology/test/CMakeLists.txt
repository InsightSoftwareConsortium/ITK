itk_module_test()
set(ITKMathematicalMorphologyTests
itkClosingByReconstructionImageFilterTest.cxx
itkFlatStructuringElementTest.cxx
itkFlatStructuringElementTest2.cxx
itkFlatStructuringElementTest3.cxx
itkGrayscaleConnectedClosingImageFilterTest.cxx
itkGrayscaleConnectedOpeningImageFilterTest.cxx
itkGrayscaleFillholeImageFilterTest.cxx
itkGrayscaleFunctionDilateImageFilterTest.cxx
itkGrayscaleFunctionErodeImageFilterTest.cxx
itkGrayscaleMorphologicalClosingImageFilterTest.cxx
itkGrayscaleMorphologicalOpeningImageFilterTest.cxx
itkGrayscaleGeodesicErodeDilateImageFilterTest.cxx
itkHConcaveImageFilterTest.cxx
itkHConvexImageFilterTest.cxx
itkHConvexConcaveImageFilterTest.cxx
itkHMaximaImageFilterTest.cxx
itkHMinimaImageFilterTest.cxx
itkHMaximaMinimaImageFilterTest.cxx
itkMorphologicalGradientImageFilterTest.cxx
itkOpeningByReconstructionImageFilterTest.cxx
itkOpeningByReconstructionImageFilterTest2.cxx
itkDoubleThresholdImageFilterTest.cxx
itkRemoveBoundaryObjectsTest.cxx
itkRemoveBoundaryObjectsTest2.cxx
itkShapedIteratorFromStructuringElementTest.cxx
itkTopHatImageFilterTest.cxx
itkMapGrayscaleDilateImageFilterTest.cxx
itkMapGrayscaleErodeImageFilterTest.cxx
itkMapGrayscaleMorphologicalClosingImageFilterTest.cxx
itkMapGrayscaleMorphologicalOpeningImageFilterTest.cxx
itkGrayscaleDilateImageFilterTest.cxx
itkGrayscaleErodeImageFilterTest.cxx
itkGrayscaleMorphologicalClosingImageFilterTest2.cxx
itkGrayscaleMorphologicalOpeningImageFilterTest2.cxx
itkMorphologicalGradientImageFilterTest2.cxx
itkRegionalMaximaImageFilterTest.cxx
itkRegionalMinimaImageFilterTest.cxx
itkValuedRegionalMaximaImageFilterTest.cxx
itkValuedRegionalMinimaImageFilterTest.cxx
itkMaskedRankImageFilterTest.cxx
itkRankImageFilterTest.cxx
itkMapMaskedRankImageFilterTest.cxx
itkMapRankImageFilterTest.cxx
)

CreateTestDriver(ITKMathematicalMorphology  "${ITKMathematicalMorphology-Test_LIBRARIES}" "${ITKMathematicalMorphologyTests}")

itk_add_test(NAME itkClosingByReconstructionImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ClosingByReconstructionImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest.png
    itkClosingByReconstructionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/closerec1.jpg} ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest.png 4 0 ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTestSubtract.png)
itk_add_test(NAME itkClosingByReconstructionImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/ClosingByReconstructionImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest2.png
    itkClosingByReconstructionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/closerec1.jpg} ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest2.png 4 1 ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTestSubtract2.png)
itk_add_test(NAME itkFlatStructuringElementTest
      COMMAND ITKMathematicalMorphologyTestDriver
              --redirectOutput ${ITK_TEST_OUTPUT_DIR}/itkFlatStructuringElementTest.txt
    itkFlatStructuringElementTest)
set_tests_properties(itkFlatStructuringElementTest PROPERTIES ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/itkFlatStructuringElementTest.txt)

itk_add_test(NAME itkFlatStructuringElementTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/FlatStructuringElementImageTest.png}
              ${ITK_TEST_OUTPUT_DIR}/FlatStructuringElementImageTest.png
      itkFlatStructuringElementTest2 DATA{Baseline/FlatStructuringElementImageTest.png} ${ITK_TEST_OUTPUT_DIR}/FlatStructuringElementImageTest.png
      )

itk_add_test(NAME itkFlatStructuringElementTest3_1
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/FlatStructuringPoly4.png "c91b17423272830b10faebb438b67621"
    itkFlatStructuringElementTest3
      ${ITK_TEST_OUTPUT_DIR}/FlatStructuringPoly4.png 10 4 2)
itk_add_test(NAME itkFlatStructuringElementTest3_2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/FlatStructuringPoly6.png "c332474ae094a8270b62e655a7c6b6ed"
    itkFlatStructuringElementTest3
    ${ITK_TEST_OUTPUT_DIR}/FlatStructuringPoly6.png 10 6 2)

itk_add_test(NAME itkGrayscaleConnectedClosingImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png
    itkGrayscaleConnectedClosingImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png 174 214)
itk_add_test(NAME itkGrayscaleConnectedClosingImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png
    itkGrayscaleConnectedClosingImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png 150 169)
itk_add_test(NAME itkGrayscaleConnectedOpeningImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png
    itkGrayscaleConnectedOpeningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png 174 214)
itk_add_test(NAME itkGrayscaleConnectedOpeningImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png
    itkGrayscaleConnectedOpeningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png 150 169)
itk_add_test(NAME itkGrayscaleFillholeImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleFillholeImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png
    itkGrayscaleFillholeImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png)
itk_add_test(NAME itkGrayscaleFunctionDilateImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkGrayscaleFunctionDilateImageFilterTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleFunctionDilateImageFilterTest.mha
    itkGrayscaleFunctionDilateImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleFunctionDilateImageFilterTest.mha)
itk_add_test(NAME itkGrayscaleFunctionErodeImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkGrayscaleFunctionErodeImageFilterTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleFunctionErodeImageFilterTest.mha
    itkGrayscaleFunctionErodeImageFilterTest ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleFunctionErodeImageFilterTest.mha)
itk_add_test(NAME itkGrayscaleMorphologicalClosingImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleMorphologicalClosingImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalClosingImageFilterTest.png
    itkGrayscaleMorphologicalClosingImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalClosingImageFilterTest.png)
itk_add_test(NAME itkGrayscaleMorphologicalOpeningImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleMorphologicalOpeningImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalOpeningImageFilterTest.png
    itkGrayscaleMorphologicalOpeningImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalOpeningImageFilterTest.png)
itk_add_test(NAME itkGrayscaleGeodesicErodeDilateImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png
    itkGrayscaleGeodesicErodeDilateImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png 35)
itk_add_test(NAME itkHConcaveImageFilterTestFullyConnectedOff
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkHConcaveImageFilterTestFullyConnectedOff.png
                  bd1b5ab47f54cd97b5c6b454bee130e2
    itkHConcaveImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHConcaveImageFilterTestFullyConnectedOff.png 2000 0)
itk_add_test(NAME itkHConcaveImageFilterTestFullyConnectedOn
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkHConcaveImageFilterTestFullyConnectedOn.png
                  c7116406ded975955965226f6a69e28d
    itkHConcaveImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHConcaveImageFilterTestFullyConnectedOn.png 2000 1)
itk_add_test(NAME itkHConvexImageFilterTestFullyConnectedOff
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkHConvexImageFilterTestFullyConnectedOff.png
                  d77fb8b2aeac6f302cc2582e12831eb4
    itkHConvexImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHConvexImageFilterTestFullyConnectedOff.png 2000 0)
itk_add_test(NAME itkHConvexImageFilterTestFullyConnectedOn
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkHConvexImageFilterTestFullyConnectedOn.png
                  d8b22af69787337021748e00f82bc100
    itkHConvexImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHConvexImageFilterTestFullyConnectedOn.png 2000 1)
itk_add_test(NAME itkHConvexConcaveImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HConvexConcaveImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png
    itkHConvexConcaveImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png 50)
itk_add_test(NAME itkHMaximaImageFilterTestFullyConnectedOff
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkHMaximaImageFilterTestFullyConnectedOff.png
                  9e2e9272501fa665b0622329dc70b121
    itkHMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHMaximaImageFilterTestFullyConnectedOff.png 2000 0)
itk_add_test(NAME itkHMaximaImageFilterTestFullyConnectedOn
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkHMaximaImageFilterTestFullyConnectedOn.png
                  934b9aec16da6cc9171a3074cbf9f242
    itkHMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHMaximaImageFilterTestFullyConnectedOn.png 2000 1)
itk_add_test(NAME itkHMinimaImageFilterTestFullyConnectedOff
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkHMinimaImageFilterTestFullyConnectedOff.png
                  9e2e9272501fa665b0622329dc70b121
    itkHMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHMinimaImageFilterTestFullyConnectedOff.png 2000 0)
itk_add_test(NAME itkHMinimaImageFilterTestFullyConnectedOn
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkHMinimaImageFilterTestFullyConnectedOn.png
                  69e285fb1db3f8f83a0b6469b4f24f7f
    itkHMinimaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Input-RA-Short.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkHMinimaImageFilterTestFullyConnectedOn.png 2000 1)
itk_add_test(NAME itkHMaximaMinimaImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png
    itkHMaximaMinimaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png 35)
itk_add_test(NAME itkHMaximaMinimaImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png
    itkHMaximaMinimaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cake_hard.png} ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png 35)
itk_add_test(NAME itkMorphologicalGradientImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/MorphologicalGradientImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/MorphologicalGradientImageFilterTest.png
    itkMorphologicalGradientImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/MorphologicalGradientImageFilterTest.png)
itk_add_test(NAME itkMorphologicalGradientImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/MorphologicalGradientImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/MorphologicalGradientImageFilterTest2.png
    itkMorphologicalGradientImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/MorphologicalGradientImageFilterTest2.png)
itk_add_test(NAME itkOpeningByReconstructionImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/OpeningByReconstructionImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest.png
    itkOpeningByReconstructionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/chondt.png} ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest.png 4 0 ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestSubtract.png)
itk_add_test(NAME itkOpeningByReconstructionImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/OpeningByReconstructionImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest2.png
    itkOpeningByReconstructionImageFilterTest DATA{${ITK_DATA_ROOT}/Input/chondt.png} ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest2.png 4 1 ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestSubtract2.png)
itk_add_test(NAME itkOpeningByReconstructionImageFilterTestNoInput
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/OpeningByReconstructionImageFilterTestNoInput.png}
              ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestNoInput.png
    itkOpeningByReconstructionImageFilterTest2 ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestNoInput.png 4 0 0 0 0.5 0.5 ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestSubtractNoInput.png)
itk_add_test(NAME itkOpeningByReconstructionImageFilterTestNoInput2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/OpeningByReconstructionImageFilterTestNoInput2.png}
              ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestNoInput2.png
    itkOpeningByReconstructionImageFilterTest2 ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestNoInput2.png 4 1 0 0 0.5 0.5 ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestSubtractNoInput2.png)
itk_add_test(NAME itkDoubleThresholdImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
  --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest.png}
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png itkDoubleThresholdImageFilterTest
            ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png 217 246 255 255)
itk_add_test(NAME itkDoubleThresholdImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
  --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest2.png}
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png itkDoubleThresholdImageFilterTest
            ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png 150 164 164 180)
itk_add_test(NAME itkRemoveBoundaryObjectsTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest.png}
              ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png
    itkRemoveBoundaryObjectsTest DATA{${ITK_DATA_ROOT}/Input/Spots.png} ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png)
itk_add_test(NAME itkRemoveBoundaryObjectsTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png
    itkRemoveBoundaryObjectsTest2 DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png)
itk_add_test(NAME itkShapedIteratorFromStructuringElementTest
      COMMAND ITKMathematicalMorphologyTestDriver itkShapedIteratorFromStructuringElementTest)
itk_add_test(NAME itkBlackTopHatImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBlackTopHatImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png
    itkTopHatImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png 0 3)
itk_add_test(NAME itkWhiteTopHatImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkWhiteTopHatImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png
    itkTopHatImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png 1 3)
itk_add_test(NAME itkGrayscaleMorphologicalClosingImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2Basic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2Histo.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2Basic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2VHGW.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2Basic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2Anchor.png
    itkGrayscaleMorphologicalClosingImageFilterTest2
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2Basic.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2Histo.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2VHGW.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2Anchor.png
    0)
itk_add_test(NAME itkGrayscaleMorphologicalClosingImageFilterTest2SafeBorder
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2BasicSafeBorder.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2HistoSafeBorder.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2BasicSafeBorder.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2VHGWSafeBorder.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2BasicSafeBorder.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2AnchorSafeBorder.png
    itkGrayscaleMorphologicalClosingImageFilterTest2
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2BasicSafeBorder.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2HistoSafeBorder.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2VHGWSafeBorder.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalClosingImageFilterTest2AnchorSafeBorder.png
    1)
itk_add_test(NAME itkGrayscaleMorphologicalOpeningImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2Basic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2Histo.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2Basic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2VHGW.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2Basic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2Anchor.png
    itkGrayscaleMorphologicalOpeningImageFilterTest2
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2Basic.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2Histo.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2VHGW.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2Anchor.png
    0)
itk_add_test(NAME itkGrayscaleMorphologicalOpeningImageFilterTest2SafeBorder
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2BasicSafeBorder.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2HistoSafeBorder.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2BasicSafeBorder.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2VHGWSafeBorder.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2BasicSafeBorder.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2AnchorSafeBorder.png
    itkGrayscaleMorphologicalOpeningImageFilterTest2
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2BasicSafeBorder.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2HistoSafeBorder.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2VHGWSafeBorder.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleMorphologicalOpeningImageFilterTest2AnchorSafeBorder.png
    1)
itk_add_test(NAME itkGrayscaleDilateImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestBasic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestHisto.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestBasic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestVHGW.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestBasic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestAnchor.png
    itkGrayscaleDilateImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestBasic.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestHisto.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestVHGW.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleDilateImageFilterTestAnchor.png)
itk_add_test(NAME itkGrayscaleErodeImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestBasic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestHisto.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestBasic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestVHGW.png
    --compare ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestBasic.png
              ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestAnchor.png
    itkGrayscaleErodeImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestBasic.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestHisto.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestVHGW.png
    ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleErodeImageFilterTestAnchor.png)

itk_add_test(NAME itkMapGrayscaleMorphologicalClosingImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestHisto.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestVHGW.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestAnchor.png
  itkMapGrayscaleMorphologicalClosingImageFilterTest
  DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestBasic.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestHisto.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestVHGW.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestAnchor.png
  0
)
itk_add_test(NAME itkMapGrayscaleMorphologicalClosingImageFilterTestSafeBorder
      COMMAND ITKMathematicalMorphologyTestDriver
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
  itkMapGrayscaleMorphologicalClosingImageFilterTest
  DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
  1
)
itk_add_test(NAME itkMapGrayscaleMorphologicalOpeningImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestHisto.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
  itkMapGrayscaleMorphologicalOpeningImageFilterTest
  DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestBasic.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestHisto.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
  0
)
itk_add_test(NAME itkMapGrayscaleMorphologicalOpeningImageFilterTestSafeBorder
      COMMAND ITKMathematicalMorphologyTestDriver
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
  itkMapGrayscaleMorphologicalOpeningImageFilterTest
  DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
  1
)
itk_add_test(NAME itkMapGrayscaleDilateImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestHisto.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestVHGW.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestAnchor.png
  itkMapGrayscaleDilateImageFilterTest
  DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestBasic.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestHisto.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestVHGW.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleDilateImageFilterTestAnchor.png
)
itk_add_test(NAME itkMapGrayscaleErodeImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestHisto.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestVHGW.png
  --compare ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestBasic.png
            ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestAnchor.png
  itkMapGrayscaleErodeImageFilterTest
  DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestBasic.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestHisto.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestVHGW.png
  ${ITK_TEST_OUTPUT_DIR}/itkMapGrayscaleErodeImageFilterTestAnchor.png
)
itk_add_test(NAME itkRegionalMinimaImageFilterTest1
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/cthead1RegionalMinimal-ref1.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal1.png
    itkRegionalMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal1.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal-ref1.png 2 1 1)
itk_add_test(NAME itkRegionalMinimaImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/cthead1RegionalMinimal-ref2.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal2.png
    itkRegionalMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal2.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMinimal-ref2.png 2 0 1)

itk_add_test(NAME itkRegionalMaximaImageFilterTest1
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/cthead1RegionalMaximal-ref1.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal1.png
    itkRegionalMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal1.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal-ref1.png 2 1 1)
itk_add_test(NAME itkRegionalMaximaImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/cthead1RegionalMaximal-ref2.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal2.png
    itkRegionalMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal2.png ${ITK_TEST_OUTPUT_DIR}/cthead1RegionalMaximal-ref2.png 2 0 1)

itk_add_test(NAME itkValuedRegionalMaximaImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/cthead1ValuedRegionalMaximal-ref.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal.png
    itkValuedRegionalMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal-ref.png 1)
itk_add_test(NAME itkValuedRegionalMaximaImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/cthead1ValuedRegionalMaximal-ref2.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal2.png
    itkValuedRegionalMaximaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal2.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMaximal-ref2.png 0)
itk_add_test(NAME itkValuedRegionalMinimaImageFilterTest
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/cthead1ValuedRegionalMinimal-ref.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal.png
    itkValuedRegionalMinimaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal-ref.png 1)
itk_add_test(NAME itkValuedRegionalMinimaImageFilterTest2
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/cthead1ValuedRegionalMinimal-ref2.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal2.png
    itkValuedRegionalMinimaImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal2.png ${ITK_TEST_OUTPUT_DIR}/cthead1ValuedRegionalMinimal-ref2.png 0)
itk_add_test(NAME itkMapMaskedRankImageFilterTest3
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkMaskedRankImageFilter3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMapMaskedRankImageFilter3.png
    itkMapMaskedRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-mask.png} ${ITK_TEST_OUTPUT_DIR}/itkMapMaskedRankImageFilter3.png 3)
itk_add_test(NAME itkMapMaskedRankImageFilterTest10
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkMaskedRankImageFilter10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMapMaskedRankImageFilter10.png
    itkMapMaskedRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-mask.png} ${ITK_TEST_OUTPUT_DIR}/itkMapMaskedRankImageFilter10.png 10)
itk_add_test(NAME itkMapRankImageFilterTest3
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkRankImageFilter3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMapRankImageFilter3.png
    itkMapRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMapRankImageFilter3.png 3)
itk_add_test(NAME itkMapRankImageFilterTest10
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkRankImageFilter10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMapRankImageFilter10.png
    itkMapRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkMapRankImageFilter10.png 10)
itk_add_test(NAME itkMaskedRankImageFilterTest3
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkMaskedRankImageFilter3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedRankImageFilter3.png
    itkMaskedRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-mask.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedRankImageFilter3.png 3)
itk_add_test(NAME itkMaskedRankImageFilterTest10
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkMaskedRankImageFilter10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMaskedRankImageFilter10.png
    itkMaskedRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-mask.png} ${ITK_TEST_OUTPUT_DIR}/itkMaskedRankImageFilter10.png 10)
itk_add_test(NAME itkRankImageFilterTest3
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkRankImageFilter3.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRankImageFilter3.png
    itkRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkRankImageFilter3.png 3)
itk_add_test(NAME itkRankImageFilterTest10
      COMMAND ITKMathematicalMorphologyTestDriver
    --compare DATA{Baseline/itkRankImageFilter10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkRankImageFilter10.png
    itkRankImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkRankImageFilter10.png 10)
