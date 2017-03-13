itk_module_test()
set(ITKWatershedsTests
  itkTobogganImageFilterTest.cxx
  itkIsolatedWatershedImageFilterTest.cxx
  itkWatershedImageFilterTest.cxx
  itkMorphologicalWatershedFromMarkersImageFilterTest.cxx
  itkMorphologicalWatershedImageFilterTest.cxx
  )

CreateTestDriver(ITKWatersheds  "${ITKWatersheds-Test_LIBRARIES}" "${ITKWatershedsTests}")

itk_add_test(NAME itkTobogganImageFilterTest
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkTobogganImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkTobogganImageFilterTest.png
    itkTobogganImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkTobogganImageFilterTest.png)
itk_add_test(NAME itkIsolatedWatershedImageFilterTest
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkIsolatedWatershedImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkIsolatedWatershedImageFilterTest.png
    itkIsolatedWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIsolatedWatershedImageFilterTest.png 113 84 120 99 0.001 .0001)
itk_add_test(NAME itkIsolatedWatershedImageFilterTestCloseThresholds
      COMMAND ITKWatershedsTestDriver
    --compare-MD5 DATA{Baseline/itkIsolatedWatershedImageFilterTestCloseThresholds.png}
              b53800712fb3b5a376c2f8234e1ebac5
    itkIsolatedWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkIsolatedWatershedImageFilterTestCloseThresholds.png 113 84 120 99 0.1 1.0)
itk_add_test(NAME itkWatershedImageFilterTest
      COMMAND ITKWatershedsTestDriver itkWatershedImageFilterTest)


itk_add_test(NAME itkMorphologicalWatershedFromMarkersImageFilterTestM0F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedFromMarkersImageFilterTestM0F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM0F0.png
    itkMorphologicalWatershedFromMarkersImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-markers.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM0F0.png 0 0)
itk_add_test(NAME itkMorphologicalWatershedFromMarkersImageFilterTestM0F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedFromMarkersImageFilterTestM0F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM0F1.png
    itkMorphologicalWatershedFromMarkersImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-markers.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM0F1.png 0 1)
itk_add_test(NAME itkMorphologicalWatershedFromMarkersImageFilterTestM1F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedFromMarkersImageFilterTestM1F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM1F0.png
    itkMorphologicalWatershedFromMarkersImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-markers.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM1F0.png 1 0)
itk_add_test(NAME itkMorphologicalWatershedFromMarkersImageFilterTestM1F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedFromMarkersImageFilterTestM1F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM1F1.png
    itkMorphologicalWatershedFromMarkersImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1-markers.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedFromMarkersImageFilterTestM1F1.png 1 1)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestButtonHoleM0F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestButtonHoleM0F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM0F0.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/button-hole.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM0F0.png 0 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPassValuesM0F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestPassValuesM0F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM0F0.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/pass-values.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM0F0.png 0 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPlateauM0F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestPlateauM0F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM0F0.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/plateau.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM0F0.png 0 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestThickLinesM0F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestThickLinesM0F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM0F0.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/thick-lines.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM0F0.png 0 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestButtonHoleM0F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestButtonHoleM0F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM0F1.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/button-hole.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM0F1.png 0 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPassValuesM0F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestPassValuesM0F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM0F1.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/pass-values.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM0F1.png 0 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPlateauM0F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestPlateauM0F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM0F1.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/plateau.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM0F1.png 0 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestThickLinesM0F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestThickLinesM0F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM0F1.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/thick-lines.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM0F1.png 0 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestButtonHoleM1F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestButtonHoleM1F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM1F0.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/button-hole.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM1F0.png 1 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPassValuesM1F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestPassValuesM1F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM1F0.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/pass-values.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM1F0.png 1 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPlateauM1F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestPlateauM1F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM1F0.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/plateau.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM1F0.png 1 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestThickLinesM1F0
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestThickLinesM1F0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM1F0.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/thick-lines.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM1F0.png 1 0 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestButtonHoleM1F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestButtonHoleM1F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM1F1.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/button-hole.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestButtonHoleM1F1.png 1 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPassValuesM1F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestPassValuesM1F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM1F1.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/pass-values.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPassValuesM1F1.png 1 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestPlateauM1F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestPlateauM1F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM1F1.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/plateau.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestPlateauM1F1.png 1 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestThickLinesM1F1
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestThickLinesM1F1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM1F1.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/thick-lines.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestThickLinesM1F1.png 1 1 0)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel00
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestLevel00.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel00.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/level.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel00.png 1 0 00)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel10
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestLevel10.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel10.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/level.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel10.png 1 0 10)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel20
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestLevel20.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel20.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/level.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel20.png 1 0 20)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel30
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestLevel30.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel30.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/level.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel30.png 1 0 30)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel40
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestLevel40.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel40.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/level.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel40.png 1 0 40)
itk_add_test(NAME itkMorphologicalWatershedImageFilterTestLevel50
      COMMAND ITKWatershedsTestDriver
    --compare DATA{Baseline/itkMorphologicalWatershedImageFilterTestLevel50.png}
              ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel50.png
    itkMorphologicalWatershedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/level.png} ${ITK_TEST_OUTPUT_DIR}/itkMorphologicalWatershedImageFilterTestLevel50.png 1 0 50)
