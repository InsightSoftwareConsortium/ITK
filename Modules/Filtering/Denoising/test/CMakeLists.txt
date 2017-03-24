itk_module_test()
set(ITKDenoisingTests
itkPatchBasedDenoisingImageFilterTest.cxx
itkPatchBasedDenoisingImageFilterDefaultTest.cxx
)

CreateTestDriver(ITKDenoising  "${ITKDenoising-Test_LIBRARIES}" "${ITKDenoisingTests}")

itk_add_test(NAME itkPatchBasedDenoisingImageFilterDefaultTest
      COMMAND ITKDenoisingTestDriver
    --compare DATA{Baseline/PatchBasedDenoisingImageFilterDefaultTest.mha}
              ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterDefaultTest.mha
    itkPatchBasedDenoisingImageFilterDefaultTest
      DATA{Input/noisy_checkerboard.mha}
      ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterDefaultTest.mha
      2)
itk_add_test(NAME itkPatchBasedDenoisingImageFilterTest0
      COMMAND ITKDenoisingTestDriver
    --compare DATA{Baseline/PatchBasedDenoisingImageFilterTest0.mha}
              ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTest0.mha
    itkPatchBasedDenoisingImageFilterTest
      DATA{Input/noisy_checkerboard.mha}
      ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTest0.mha
      2 1 14.093142951003070 2 2 200)
itk_add_test(NAME itkPatchBasedDenoisingImageFilterTestGaussian
      COMMAND ITKDenoisingTestDriver
    --compare DATA{Baseline/PatchBasedDenoisingImageFilterTestGaussian.mha}
              ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTestGaussian.mha
    itkPatchBasedDenoisingImageFilterTest
      DATA{Input/checkerboard_noise10Gaussian.mha}
      ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTestGaussian.mha
      2 1 14.418708309529121 2 2 200 0 1 GAUSSIAN 0.1)
itk_add_test(NAME itkPatchBasedDenoisingImageFilterTestRician
      COMMAND ITKDenoisingTestDriver
    --compare DATA{Baseline/PatchBasedDenoisingImageFilterTestRician.mha}
              ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTestRician.mha
    itkPatchBasedDenoisingImageFilterTest
      DATA{Input/checkerboard_noise10Rician.mha}
      ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTestRician.mha
      2 1 12.283962106667094 2 2 200 0 1 RICIAN 0.1)
itk_add_test(NAME itkPatchBasedDenoisingImageFilterTestPoisson
      COMMAND ITKDenoisingTestDriver
    --compare DATA{Baseline/PatchBasedDenoisingImageFilterTestPoisson.mha}
              ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTestPoisson.mha
    itkPatchBasedDenoisingImageFilterTest
      DATA{Input/checkerboard_noise10Poisson.mha}
      ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTestPoisson.mha
      2 1 9.9250532200729378 2 2 200 0 1 POISSON 0.1)
itk_add_test(NAME itkPatchBasedDenoisingImageFilterTestTensors
      COMMAND ITKDenoisingTestDriver
    --compare
      DATA{Baseline/PatchBasedDenoisingImageFilterTestTensors.nrrd}
      ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTestTensors.nrrd
    itkPatchBasedDenoisingImageFilterTest
      DATA{Input/noisyDiffusionTensors.nrrd}
      ${ITK_TEST_OUTPUT_DIR}/PatchBasedDenoisingImageFilterTestTensors.nrrd
      2 6 5.4377394641246628 2 2 100 0 2)
