itk_module_test()
set(ITKFEMRegistrationTests
itkFEMFiniteDifferenceFunctionLoadTest.cxx
itkFEMRegistrationFilterTest.cxx
itkFEMRegistrationFilter2DTest.cxx
itkMIRegistrationFunctionTest.cxx
itkFEMRegistrationFilterTest2.cxx
itkPhysicsBasedNonRigidRegistrationMethodTest.cxx
)

# For itkPhysicsBasedNonRigidRegistrationMethodTest.cxx
include_directories( ${ITKFEMRegistration_SOURCE_DIR}/test )

CreateTestDriver(ITKFEMRegistration  "${ITKFEMRegistration-Test_LIBRARIES}" "${ITKFEMRegistrationTests}")

itk_add_test(NAME itkFEMFiniteDifferenceFunctionLoadTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMFiniteDifferenceFunctionLoadTest
      ${ITK_TEST_OUTPUT_DIR}/FiniteDifferenceFunctionLoadTest)
itk_add_test(NAME itkFEMRegistrationFilterTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationFilterTest
      ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField3D ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage3D)
itk_add_test(NAME itkFEMRegistrationFilter2DTest
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationFilter2DTest
      ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField2D ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage2D)
itk_add_test(NAME itkMIRegistrationFunctionTest
      COMMAND ITKFEMRegistrationTestDriver itkMIRegistrationFunctionTest)
itk_add_test(NAME itkFEMRegistrationFilterTest2
      COMMAND ITKFEMRegistrationTestDriver itkFEMRegistrationFilterTest2
      ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField3DMultiRes ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage3DMultiRes)
itk_add_test(NAME itkPhysicsBasedNonRigidRegistrationMethodTest
      COMMAND ITKFEMRegistrationTestDriver
    --compareRadiusTolerance 5
    --compareNumberOfPixelsTolerance 0
    --compareIntensityTolerance 9
    --compare DATA{${CMAKE_CURRENT_SOURCE_DIR}/Baseline/itkPhysicsBasedNonRigidRegistrationMethodTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/itkPhysicsBasedNonRigidRegistrationMethodTest.mha
    itkPhysicsBasedNonRigidRegistrationMethodTest DATA{${CMAKE_CURRENT_SOURCE_DIR}/Input/PBNRRFixedImage.nii.gz}
              DATA{${CMAKE_CURRENT_SOURCE_DIR}/Input/PBNRRMovingImage.nii.gz}
              DATA{${CMAKE_CURRENT_SOURCE_DIR}/Input/PBNRRMaskImage.nii.gz}
              DATA{${CMAKE_CURRENT_SOURCE_DIR}/Input/PBNRRMesh.vtk}
              ${ITK_TEST_OUTPUT_DIR}/itkPhysicsBasedNonRigidRegistrationMethodTest.mha)
