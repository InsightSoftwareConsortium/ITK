itk_module_test()
set(ITK-TransformTests
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
itkTransformHeaderTest.cxx
itkAzimuthElevationToCartesianTransformTest.cxx
itkCenteredEuler3DTransformTest.cxx
itkTransformFileReaderWriterTest.cxx
itkTransformIOTest.cxx
itkTransformsSetParametersTest.cxx
itkBSplineDeformableTransformTest.cxx
itkBSplineDeformableTransformTest2.cxx
itkBSplineDeformableTransformTest3.cxx
itkVersorRigid3DTransformTest.cxx
itkVersorTransformTest.cxx
itkCenteredTransformInitializerTest.cxx
itkCenteredVersorTransformInitializerTest.cxx
itkSplineKernelTransformTest.cxx
)

CreateTestDriver(ITK-Transform  "${ITK-Transform-Test_LIBRARIES}" "${ITK-TransformTests}")

add_test(NAME itkTransformHeaderTest
      COMMAND ITK-TransformTestDriver itkTransformHeaderTest)
add_test(NAME itkFixedCenterOfRotationAffineTransformTest
      COMMAND ITK-TransformTestDriver itkFixedCenterOfRotationAffineTransformTest)
add_test(NAME itkAffineTransformTest
      COMMAND ITK-TransformTestDriver itkAffineTransformTest)
add_test(NAME itkLandmarkBasedTransformInitializerTest
      COMMAND ITK-TransformTestDriver itkLandmarkBasedTransformInitializerTest)
add_test(NAME itkScaleSkewVersor3DTransformTest
      COMMAND ITK-TransformTestDriver itkScaleSkewVersor3DTransformTest)
add_test(NAME itkEuler3DTransformTest
      COMMAND ITK-TransformTestDriver itkEuler3DTransformTest)
add_test(NAME itkRigid3DTransformTest
      COMMAND ITK-TransformTestDriver itkRigid3DTransformTest)
add_test(NAME itkScaleVersor3DTransformTest
      COMMAND ITK-TransformTestDriver itkScaleVersor3DTransformTest)
add_test(NAME itkTransformTest
      COMMAND ITK-TransformTestDriver itkTransformTest)
add_test(NAME itkRigid3DPerspectiveTransformTest
      COMMAND ITK-TransformTestDriver itkRigid3DPerspectiveTransformTest)
add_test(NAME itkSimilarity2DTransformTest
      COMMAND ITK-TransformTestDriver itkSimilarity2DTransformTest)
add_test(NAME itkTranslationTransformTest
      COMMAND ITK-TransformTestDriver itkTranslationTransformTest)
add_test(NAME itkCenteredRigid2DTransformTest
      COMMAND ITK-TransformTestDriver itkCenteredRigid2DTransformTest
              ${ITK_TEST_OUTPUT_DIR}/testCenteredTransform.txt)
add_test(NAME itkIdentityTransformTest
      COMMAND ITK-TransformTestDriver itkIdentityTransformTest)
add_test(NAME itkv3Rigid3DTransformTest
      COMMAND ITK-TransformTestDriver itkv3Rigid3DTransformTest)
add_test(NAME itkTransformFactoryBaseTest
      COMMAND ITK-TransformTestDriver itkTransformFactoryBaseTest)
add_test(NAME itkEuler2DTransformTest
      COMMAND ITK-TransformTestDriver itkEuler2DTransformTest
              ${ITK_TEST_OUTPUT_DIR}/testEulerTransform.txt)
add_test(NAME itkCenteredAffineTransformTest
      COMMAND ITK-TransformTestDriver itkCenteredAffineTransformTest)
add_test(NAME itkRigid2DTransformTest
      COMMAND ITK-TransformTestDriver itkRigid2DTransformTest)
add_test(NAME itkScaleLogarithmicTransformTest
      COMMAND ITK-TransformTestDriver itkScaleLogarithmicTransformTest)
add_test(NAME itkQuaternionRigidTransformTest
      COMMAND ITK-TransformTestDriver itkQuaternionRigidTransformTest)
add_test(NAME itkScaleTransformTest
      COMMAND ITK-TransformTestDriver itkScaleTransformTest)
add_test(NAME itkSimilarity3DTransformTest
      COMMAND ITK-TransformTestDriver itkSimilarity3DTransformTest)
add_test(NAME itkAzimuthElevationToCartesianTransformTest
      COMMAND ITK-TransformTestDriver itkAzimuthElevationToCartesianTransformTest)
add_test(NAME itkCenteredEuler3DTransformTest
      COMMAND ITK-TransformTestDriver itkCenteredEuler3DTransformTest)
add_test(NAME itkTransformFileReaderWriterTest0
      COMMAND ITK-TransformTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform00.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest0.txt)
add_test(NAME itkTransformFileReaderWriterTest1
      COMMAND ITK-TransformTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform01.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest1.txt)
add_test(NAME itkTransformFileReaderWriterTest2
      COMMAND ITK-TransformTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkAffineTransform01.txt ${ITK_TEST_OUTPUT_DIR}/itkTransformFileReaderWriterTest1.mat)
add_test(NAME itkTransformFileReaderWriterTest3
      COMMAND ITK-TransformTestDriver itkTransformFileReaderWriterTest
              ${ITK_DATA_ROOT}/Input/itkBSplineTransform00.txt ${ITK_TEST_OUTPUT_DIR}/itkBSplineTransform00.mat)
add_test(NAME itkTransformIOTest
      COMMAND ITK-TransformTestDriver itkTransformIOTest
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkTransformsSetParametersTest
      COMMAND ITK-TransformTestDriver itkTransformsSetParametersTest)
add_test(NAME itkBSplineDeformableTransformTest
      COMMAND ITK-TransformTestDriver itkBSplineDeformableTransformTest)
add_test(NAME itkBSplineDeformableTransformTest2
      COMMAND ITK-TransformTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest2PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2PixelCentered.png
    itkBSplineDeformableTransformTest2 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements5.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest2DeformationFieldPixelCentered.mhd 1)
add_test(NAME itkBSplineDeformableTransformTest3
      COMMAND ITK-TransformTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest3PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3PixelCentered.png
    itkBSplineDeformableTransformTest2 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements3.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest3DeformationFieldPixelCentered.mhd 2)
add_test(NAME itkBSplineDeformableTransformTest4
      COMMAND ITK-TransformTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest4PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4PixelCentered.png
    itkBSplineDeformableTransformTest2 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest4DeformationFieldPixelCentered.mhd 3)
add_test(NAME itkBSplineDeformableTransformTest5
      COMMAND ITK-TransformTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest5PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5PixelCentered.png
    itkBSplineDeformableTransformTest3 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest5DeformationFieldPixelCentered.mhd 0)
add_test(NAME itkBSplineDeformableTransformTest6
      COMMAND ITK-TransformTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest5PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6PixelCentered.png
    itkBSplineDeformableTransformTest3 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest6DeformationFieldPixelCentered.mhd 1)
add_test(NAME itkBSplineDeformableTransformTest7
      COMMAND ITK-TransformTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Common/itkBSplineDeformableTransformTest5PixelCentered.png
              ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7PixelCentered.png
    itkBSplineDeformableTransformTest3 ${ITK_EXAMPLE_DATA_ROOT}/BSplineDisplacements1.txt ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_EXAMPLE_DATA_ROOT}/DiagonalLines.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7PixelCentered.png ${ITK_TEST_OUTPUT_DIR}/itkBSplineDeformableTransformTest7DeformationFieldPixelCentered.mhd 2)
add_test(NAME itkVersorRigid3DTransformTest
      COMMAND ITK-TransformTestDriver itkVersorRigid3DTransformTest)
add_test(NAME itkVersorTransformTest
      COMMAND ITK-TransformTestDriver itkVersorTransformTest)
add_test(NAME itkCenteredTransformInitializerTest
      COMMAND ITK-TransformTestDriver itkCenteredTransformInitializerTest)
add_test(NAME itkCenteredVersorTransformInitializerTest
      COMMAND ITK-TransformTestDriver itkCenteredVersorTransformInitializerTest)
add_test(NAME itkSplineKernelTransformTest
      COMMAND ITK-TransformTestDriver itkSplineKernelTransformTest)
