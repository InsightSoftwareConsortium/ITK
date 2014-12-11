itk_module_test()
set(ITKTransformTests
itkFixedCenterOfRotationAffineTransformTest.cxx
itkAffineTransformTest.cxx
itkScaleSkewVersor3DTransformTest.cxx
itkEuler3DTransformTest.cxx
itkCenteredRigid2DTransformTest.cxx
itkEuler2DTransformTest.cxx
itkRigid3DTransformTest.cxx
itkScaleVersor3DTransformTest.cxx
itkTransformTest.cxx
itkRigid3DPerspectiveTransformTest.cxx
itkSimilarity2DTransformTest.cxx
itkTranslationTransformTest.cxx
itkIdentityTransformTest.cxx
itkv3Rigid3DTransformTest.cxx
itkCenteredAffineTransformTest.cxx
itkRigid2DTransformTest.cxx
itkScaleLogarithmicTransformTest.cxx
itkQuaternionRigidTransformTest.cxx
itkScaleTransformTest.cxx
itkSimilarity3DTransformTest.cxx
itkAzimuthElevationToCartesianTransformTest.cxx
itkCenteredEuler3DTransformTest.cxx
itkTransformsSetParametersTest.cxx
itkBSplineDeformableTransformTest.cxx
itkBSplineDeformableTransformTest2.cxx
itkBSplineDeformableTransformTest3.cxx
itkBSplineTransformTest.cxx
itkBSplineTransformTest2.cxx
itkBSplineTransformTest3.cxx
itkBSplineTransformInitializerTest1.cxx
itkBSplineTransformInitializerTest2.cxx
itkVersorRigid3DTransformTest.cxx
itkVersorTransformTest.cxx
itkSplineKernelTransformTest.cxx
itkCompositeTransformTest.cxx
itkTransformCloneTest.cxx
itkMultiTransformTest.cxx
itkTestTransformGetInverse.cxx
)

CreateTestDriver(ITKTransform  "${ITKTransform-Test_LIBRARIES}" "${ITKTransformTests}")
itk_add_test(NAME itkFixedCenterOfRotationAffineTransformTest
      COMMAND ITKTransformTestDriver itkFixedCenterOfRotationAffineTransformTest)
itk_add_test(NAME itkAffineTransformTest
      COMMAND ITKTransformTestDriver itkAffineTransformTest)
itk_add_test(NAME itkScaleSkewVersor3DTransformTest
      COMMAND ITKTransformTestDriver itkScaleSkewVersor3DTransformTest)
itk_add_test(NAME itkEuler3DTransformTest
      COMMAND ITKTransformTestDriver itkEuler3DTransformTest)
itk_add_test(NAME itkCenteredRigid2DTransformTest
      COMMAND ITKTransformTestDriver itkCenteredRigid2DTransformTest)
itk_add_test(NAME itkEuler2DTransformTest
      COMMAND ITKTransformTestDriver itkEuler2DTransformTest)
itk_add_test(NAME itkRigid3DTransformTest
      COMMAND ITKTransformTestDriver itkRigid3DTransformTest)
itk_add_test(NAME itkScaleVersor3DTransformTest
      COMMAND ITKTransformTestDriver itkScaleVersor3DTransformTest)
itk_add_test(NAME itkTransformTest
      COMMAND ITKTransformTestDriver itkTransformTest)
itk_add_test(NAME itkRigid3DPerspectiveTransformTest
      COMMAND ITKTransformTestDriver itkRigid3DPerspectiveTransformTest)
itk_add_test(NAME itkSimilarity2DTransformTest
      COMMAND ITKTransformTestDriver itkSimilarity2DTransformTest)
itk_add_test(NAME itkTranslationTransformTest
      COMMAND ITKTransformTestDriver itkTranslationTransformTest)
itk_add_test(NAME itkIdentityTransformTest
      COMMAND ITKTransformTestDriver itkIdentityTransformTest)
itk_add_test(NAME itkv3Rigid3DTransformTest
      COMMAND ITKTransformTestDriver itkv3Rigid3DTransformTest)
itk_add_test(NAME itkCenteredAffineTransformTest
      COMMAND ITKTransformTestDriver itkCenteredAffineTransformTest)
itk_add_test(NAME itkRigid2DTransformTest
      COMMAND ITKTransformTestDriver itkRigid2DTransformTest)
itk_add_test(NAME itkScaleLogarithmicTransformTest
      COMMAND ITKTransformTestDriver itkScaleLogarithmicTransformTest)
itk_add_test(NAME itkQuaternionRigidTransformTest
      COMMAND ITKTransformTestDriver itkQuaternionRigidTransformTest)
itk_add_test(NAME itkScaleTransformTest
      COMMAND ITKTransformTestDriver itkScaleTransformTest)
itk_add_test(NAME itkSimilarity3DTransformTest
      COMMAND ITKTransformTestDriver itkSimilarity3DTransformTest)
itk_add_test(NAME itkAzimuthElevationToCartesianTransformTest
      COMMAND ITKTransformTestDriver itkAzimuthElevationToCartesianTransformTest)
itk_add_test(NAME itkCenteredEuler3DTransformTest
      COMMAND ITKTransformTestDriver itkCenteredEuler3DTransformTest)
itk_add_test(NAME itkTransformsSetParametersTest
      COMMAND ITKTransformTestDriver itkTransformsSetParametersTest)

## Tests for ITKv3 version of BSplineDeformableTransforms
itk_add_test(NAME itkBSplineDeformableTransformTest
      COMMAND ITKTransformTestDriver itkBSplineDeformableTransformTest)
itk_add_test(NAME itkBSplineDeformableTransformTest2
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest2PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2PixelCentered.png
    itkBSplineDeformableTransformTest2 DATA{Baseline/BSplineDeformationTransformDisplacements5.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2DeformationFieldPixelCentered.mhd 1)
itk_add_test(NAME itkBSplineDeformableTransformTest3
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest3PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3PixelCentered.png
    itkBSplineDeformableTransformTest2 DATA{Baseline/BSplineDeformationTransformDisplacements3.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3DeformationFieldPixelCentered.mhd 2)
itk_add_test(NAME itkBSplineDeformableTransformTest4
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest4PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4PixelCentered.png
    itkBSplineDeformableTransformTest2 DATA{Baseline/BSplineDeformationTransformDisplacements1.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4DeformationFieldPixelCentered.mhd 3)
itk_add_test(NAME itkBSplineDeformableTransformTest5
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest5PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5PixelCentered.png
    itkBSplineDeformableTransformTest3 DATA{Baseline/BSplineDeformationTransformDisplacements1.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5DeformationFieldPixelCentered.mhd 0)
itk_add_test(NAME itkBSplineDeformableTransformTest6
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest5PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6PixelCentered.png
    itkBSplineDeformableTransformTest3 DATA{Baseline/BSplineDeformationTransformDisplacements1.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6DeformationFieldPixelCentered.mhd 1)
itk_add_test(NAME itkBSplineDeformableTransformTest7
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineDeformableTransformTest5PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7PixelCentered.png
    itkBSplineDeformableTransformTest3 DATA{Baseline/BSplineDeformationTransformDisplacements1.txt} ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7DeformationFieldPixelCentered.mhd 2)

## Tests for ITKv4 version of BSplineTransforms
itk_add_test(NAME itkBSplineTransformTest
      COMMAND ITKTransformTestDriver itkBSplineTransformTest)
itk_add_test(NAME itkBSplineTransformTest2
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineTransformTest2PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest2PixelCentered.png
    itkBSplineTransformTest2 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements5.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest2PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest2DeformationFieldPixelCentered.mhd 1)
itk_add_test(NAME itkBSplineTransformTest3
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineTransformTest3PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest3PixelCentered.png
    itkBSplineTransformTest2 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements3.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest3PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest3DeformationFieldPixelCentered.mhd 2)
itk_add_test(NAME itkBSplineTransformTest4
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineTransformTest4PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest4PixelCentered.png
    itkBSplineTransformTest2 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest4PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest4DeformationFieldPixelCentered.mhd 3)
itk_add_test(NAME itkBSplineTransformTest5
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineTransformTest4PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest5PixelCentered.png
    itkBSplineTransformTest3 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest5PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest5DeformationFieldPixelCentered.mhd 0)
itk_add_test(NAME itkBSplineTransformTest6
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineTransformTest4PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest6PixelCentered.png
    itkBSplineTransformTest3 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest6PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest6DeformationFieldPixelCentered.mhd 1)
itk_add_test(NAME itkBSplineTransformTest7
      COMMAND ITKTransformTestDriver
    --compare DATA{Baseline/itkBSplineTransformTest4PixelCentered.png}
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest7PixelCentered.png
    itkBSplineTransformTest3 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest7PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformTest7DeformationFieldPixelCentered.mhd 2)
itk_add_test(NAME itkBSplineTransformInitializerTest1
      COMMAND ITKTransformTestDriver itkBSplineTransformInitializerTest1
              ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformInitializerTest1.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransformInitializerTest1DeformationField.mhd)
itk_add_test(NAME itkBSplineTransformInitializerTest2
      COMMAND ITKTransformTestDriver itkBSplineTransformInitializerTest2
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png )
itk_add_test(NAME itkVersorRigid3DTransformTest
      COMMAND ITKTransformTestDriver itkVersorRigid3DTransformTest)
itk_add_test(NAME itkVersorTransformTest
      COMMAND ITKTransformTestDriver itkVersorTransformTest)
itk_add_test(NAME itkSplineKernelTransformTest
      COMMAND ITKTransformTestDriver itkSplineKernelTransformTest)
itk_add_test(NAME itkCompositeTransformTest
      COMMAND ITKTransformTestDriver itkCompositeTransformTest)
itk_add_test(NAME itkTransformCloneTest
      COMMAND ITKTransformTestDriver itkTransformCloneTest)
itk_add_test(NAME itkMultiTransformTest
      COMMAND ITKTransformTestDriver itkMultiTransformTest)
itk_add_test(NAME itkTestTransformGetInverse
  COMMAND ITKTransformTestDriver itkTestTransformGetInverse)
