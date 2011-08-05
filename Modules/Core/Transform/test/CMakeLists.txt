itk_module_test()
set(ITKTransformTests
itkFixedCenterOfRotationAffineTransformTest.cxx
itkAffineTransformTest.cxx
itkLandmarkBasedTransformInitializerTest.cxx
itkScaleSkewVersor3DTransformTest.cxx
itkEuler3DTransformTest.cxx
itkRigid3DTransformTest.cxx
itkScaleVersor3DTransformTest.cxx
itkTransformTest.cxx
itkRigid3DPerspectiveTransformTest.cxx
itkSimilarity2DTransformTest.cxx
itkTranslationTransformTest.cxx
itkCenteredRigid2DTransformTest.cxx
itkIdentityTransformTest.cxx
itkv3Rigid3DTransformTest.cxx
itkTransformFactoryBaseTest.cxx
itkEuler2DTransformTest.cxx
itkCenteredAffineTransformTest.cxx
itkRigid2DTransformTest.cxx
itkScaleLogarithmicTransformTest.cxx
itkQuaternionRigidTransformTest.cxx
itkScaleTransformTest.cxx
itkSimilarity3DTransformTest.cxx
itkAzimuthElevationToCartesianTransformTest.cxx
itkCenteredEuler3DTransformTest.cxx
itkTransformFileReaderWriterTest.cxx
itkTransformIOTest.cxx
itkTransformsSetParametersTest.cxx
itkBSplineTransformTest.cxx
itkBSplineTransformTest2.cxx
itkBSplineTransformTest3.cxx
itkBSplineTransformInitializerTest1.cxx
itkBSplineTransformInitializerTest2.cxx
itkVersorRigid3DTransformTest.cxx
itkVersorTransformTest.cxx
itkSplineKernelTransformTest.cxx
)

CreateTestDriver(ITKTransform  "${ITKTransform-Test_LIBRARIES}" "${ITKTransformTests}")

itk_add_test(NAME itkFixedCenterOfRotationAffineTransformTest
      COMMAND ITKTransformTestDriver itkFixedCenterOfRotationAffineTransformTest)
itk_add_test(NAME itkAffineTransformTest
      COMMAND ITKTransformTestDriver itkAffineTransformTest)
itk_add_test(NAME itkLandmarkBasedTransformInitializerTest
      COMMAND ITKTransformTestDriver itkLandmarkBasedTransformInitializerTest)
itk_add_test(NAME itkScaleSkewVersor3DTransformTest
      COMMAND ITKTransformTestDriver itkScaleSkewVersor3DTransformTest)
itk_add_test(NAME itkEuler3DTransformTest
      COMMAND ITKTransformTestDriver itkEuler3DTransformTest)
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
itk_add_test(NAME itkCenteredRigid2DTransformTest
      COMMAND ITKTransformTestDriver itkCenteredRigid2DTransformTest
              ${ITK_TEST_OUTPUT_DIR}/testCenteredTransform.txt)
itk_add_test(NAME itkIdentityTransformTest
      COMMAND ITKTransformTestDriver itkIdentityTransformTest)
itk_add_test(NAME itkv3Rigid3DTransformTest
      COMMAND ITKTransformTestDriver itkv3Rigid3DTransformTest)
itk_add_test(NAME itkTransformFactoryBaseTest
      COMMAND ITKTransformTestDriver itkTransformFactoryBaseTest)
itk_add_test(NAME itkEuler2DTransformTest
      COMMAND ITKTransformTestDriver itkEuler2DTransformTest
              ${ITK_TEST_OUTPUT_DIR}/testEulerTransform.txt)
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
itk_add_test(NAME itkTransformFileReaderWriterTest0
      COMMAND ITKTransformTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform00.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest0.txt)
itk_add_test(NAME itkTransformFileReaderWriterTest1
      COMMAND ITKTransformTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform01.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest1.txt)
itk_add_test(NAME itkTransformFileReaderWriterTest2
      COMMAND ITKTransformTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform01.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest1.mat)
itk_add_test(NAME itkTransformFileReaderWriterTest3
      COMMAND ITKTransformTestDriver itkTransformFileReaderWriterTest
              DATA{Baseline/itkBSplineTransform00.txt} ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransform00.mat)
itk_add_test(NAME itkTransformIOTest
      COMMAND ITKTransformTestDriver itkTransformIOTest
              ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkTransformsSetParametersTest
      COMMAND ITKTransformTestDriver itkTransformsSetParametersTest)
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
