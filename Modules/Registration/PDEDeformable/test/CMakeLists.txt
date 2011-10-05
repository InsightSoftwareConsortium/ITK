itk_module_test()
set(ITKPDEDeformableRegistrationTests
itkMultiResolutionPDEDeformableRegistrationTest.cxx
itkDemonsRegistrationFilterTest.cxx
itkLevelSetMotionRegistrationFilterTest.cxx
itkSymmetricForcesDemonsRegistrationFilterTest.cxx
)
 # Define some convenient locations
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Algorithms)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKPDEDeformableRegistration  "${ITKPDEDeformableRegistration-Test_LIBRARIES}" "${ITKPDEDeformableRegistrationTests}")

itk_add_test(NAME itkDemonsRegistrationFilterTest
      COMMAND ITKPDEDeformableRegistrationTestDriver itkDemonsRegistrationFilterTest)
itk_add_test(NAME itkLevelSetMotionRegistrationFilterTest
      COMMAND ITKPDEDeformableRegistrationTestDriver itkLevelSetMotionRegistrationFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestFixedImage.mha ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestMovingImage.mha ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestResampledImage.mha)
itk_add_test(NAME itkSymmetricForcesDemonsRegistrationFilterTest
      COMMAND ITKPDEDeformableRegistrationTestDriver itkSymmetricForcesDemonsRegistrationFilterTest)
itk_add_test(NAME itkMultiResolutionPDEDeformableRegistrationTestD ${TestDriver}
      COMMAND ITKPDEDeformableRegistrationTestDriver
            --compare DATA{${BASELINE}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png}
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png
  --compareNumberOfPixelsTolerance 20
  itkMultiResolutionPDEDeformableRegistrationTest
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png)
