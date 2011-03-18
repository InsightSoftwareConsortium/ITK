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
