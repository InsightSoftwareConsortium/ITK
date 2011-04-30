itk_module_test()
set(ITK-PDEDeformableRegistrationTests
itkMultiResolutionPDEDeformableRegistrationTest.cxx
itkPDEDeformableRegistrationHeaderTest.cxx
itkDemonsRegistrationFilterTest.cxx
itkLevelSetMotionRegistrationFilterTest.cxx
itkSymmetricForcesDemonsRegistrationFilterTest.cxx
)
 # Define some convenient locations
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Algorithms)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITK-PDEDeformableRegistration  "${ITK-PDEDeformableRegistration-Test_LIBRARIES}" "${ITK-PDEDeformableRegistrationTests}")

itk_add_test(NAME itkPDEDeformableRegistrationHeaderTest
      COMMAND ITK-PDEDeformableRegistrationTestDriver itkPDEDeformableRegistrationHeaderTest)
itk_add_test(NAME itkDemonsRegistrationFilterTest
      COMMAND ITK-PDEDeformableRegistrationTestDriver itkDemonsRegistrationFilterTest)
itk_add_test(NAME itkLevelSetMotionRegistrationFilterTest
      COMMAND ITK-PDEDeformableRegistrationTestDriver itkLevelSetMotionRegistrationFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestFixedImage.mha ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestMovingImage.mha ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestResampledImage.mha)
itk_add_test(NAME itkSymmetricForcesDemonsRegistrationFilterTest
      COMMAND ITK-PDEDeformableRegistrationTestDriver itkSymmetricForcesDemonsRegistrationFilterTest)
itk_add_test(NAME itkMultiResolutionPDEDeformableRegistrationTestD ${TestDriver}
      COMMAND ITK-PDEDeformableRegistrationTestDriver
            --compare ${BASELINE}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png
  --compareNumberOfPixelsTolerance 20
  itkMultiResolutionPDEDeformableRegistrationTest
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png)
