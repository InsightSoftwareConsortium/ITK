itk_module_test()
set(
  ITKFEMRegistrationTests
  itkFEMFiniteDifferenceFunctionLoadTest.cxx
  itkFEMRegistrationFilter2DTest.cxx
  itkFEMRegistrationFilterTest.cxx
  itkFEMRegistrationFilterTest2.cxx
  itkMIRegistrationFunctionTest.cxx
  itkPhysicsBasedNonRigidRegistrationMethodTest.cxx
)

# For itkPhysicsBasedNonRigidRegistrationMethodTest.cxx
include_directories(${ITKFEMRegistration_SOURCE_DIR}/test)

createtestdriver(ITKFEMRegistration "${ITKFEMRegistration-Test_LIBRARIES}" "${ITKFEMRegistrationTests}")

itk_add_test(
  NAME itkFEMFiniteDifferenceFunctionLoadTest
  COMMAND
    ITKFEMRegistrationTestDriver
    itkFEMFiniteDifferenceFunctionLoadTest
    ${ITK_TEST_OUTPUT_DIR}/FiniteDifferenceFunctionLoadTest
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/FiniteDifferenceFunctionLoadTest*Image.mha
    ${ITK_TEST_OUTPUT_DIR}/FiniteDifferenceFunctionLoadTest*Image.raw
    ${ITK_TEST_OUTPUT_DIR}/FiniteDifferenceFunctionLoadTest*ForcesWithMetric[0-3].vtk
)
itk_add_test(
  NAME itkFEMRegistrationFilterTest
  COMMAND
    ITKFEMRegistrationTestDriver
    itkFEMRegistrationFilterTest
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField3D
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage3D
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField3D[0-3].{mhd,raw}
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage3D[0-3].{mhd,raw}
)
itk_add_test(
  NAME itkFEMRegistrationFilter2DTest
  COMMAND
    ITKFEMRegistrationTestDriver
    itkFEMRegistrationFilter2DTest
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField2D
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage2D
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField2D[0-3].{mhd,raw}
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage2D[0-3].{mhd,raw}
)
itk_add_test(
  NAME itkMIRegistrationFunctionTest
  COMMAND
    ITKFEMRegistrationTestDriver
    itkMIRegistrationFunctionTest
)
itk_add_test(
  NAME itkFEMRegistrationFilterTest2
  COMMAND
    ITKFEMRegistrationTestDriver
    itkFEMRegistrationFilterTest2
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField3DMultiRes
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage3DMultiRes
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformationField3DMultiRes[0-3].{mhd,raw}
    ${ITK_TEST_OUTPUT_DIR}/FEMDeformedMovingImage3DMultiRes[0-3].{mhd,raw}
)
itk_add_test(
  NAME itkPhysicsBasedNonRigidRegistrationMethodTest
  COMMAND
    ITKFEMRegistrationTestDriver
    --compareRadiusTolerance
    5
    --compareNumberOfPixelsTolerance
    0
    --compareIntensityTolerance
    9
    --compare
    DATA{${CMAKE_CURRENT_SOURCE_DIR}/Baseline/itkPhysicsBasedNonRigidRegistrationMethodTest.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkPhysicsBasedNonRigidRegistrationMethodTest.mha
    itkPhysicsBasedNonRigidRegistrationMethodTest
    DATA{${CMAKE_CURRENT_SOURCE_DIR}/Input/PBNRRFixedImage.nii.gz}
    DATA{${CMAKE_CURRENT_SOURCE_DIR}/Input/PBNRRMovingImage.nii.gz}
    DATA{${CMAKE_CURRENT_SOURCE_DIR}/Input/PBNRRMaskImage.nii.gz}
    DATA{${CMAKE_CURRENT_SOURCE_DIR}/Input/PBNRRMesh.vtk}
    ${ITK_TEST_OUTPUT_DIR}/itkPhysicsBasedNonRigidRegistrationMethodTest.mha
    0.05
    0
    2
    5
    10
    10
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPhysicsBasedNonRigidRegistrationMethodTest.mha
)
