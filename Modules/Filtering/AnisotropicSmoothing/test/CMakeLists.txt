itk_module_test()
set(ITK-AnisotropicSmoothingTests
itkGradientAnisotropicDiffusionImageFilterTest.cxx
itkCurvatureAnisotropicDiffusionImageFilterTest.cxx
itkMinMaxCurvatureFlowImageFilterTest.cxx
itkAnisotropicSmoothingHeaderTest.cxx
itkVectorAnisotropicDiffusionImageFilterTest.cxx
itkGradientAnisotropicDiffusionImageFilterTest2.cxx
)

CreateTestDriver(ITK-AnisotropicSmoothing  "${ITK-AnisotropicSmoothing-Test_LIBRARIES}" "${ITK-AnisotropicSmoothingTests}")

itk_add_test(NAME itkAnisotropicSmoothingHeaderTest
      COMMAND ITK-AnisotropicSmoothingTestDriver itkAnisotropicSmoothingHeaderTest)
itk_add_test(NAME itkGradientAnisotropicDiffusionImageFilterTest
      COMMAND ITK-AnisotropicSmoothingTestDriver itkGradientAnisotropicDiffusionImageFilterTest)
itk_add_test(NAME itkCurvatureAnisotropicDiffusionImageFilterTest
      COMMAND ITK-AnisotropicSmoothingTestDriver itkCurvatureAnisotropicDiffusionImageFilterTest)
itk_add_test(NAME itkMinMaxCurvatureFlowImageFilterTest
      COMMAND ITK-AnisotropicSmoothingTestDriver itkMinMaxCurvatureFlowImageFilterTest)
itk_add_test(NAME itkVectorAnisotropicDiffusionImageFilterTest
      COMMAND ITK-AnisotropicSmoothingTestDriver itkVectorAnisotropicDiffusionImageFilterTest)
itk_add_test(NAME itkGradientAnisotropicDiffusionImageFilterTest2
      COMMAND ITK-AnisotropicSmoothingTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GradientAnisotropicDiffusionImageFilterTest2.png
              ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png
    itkGradientAnisotropicDiffusionImageFilterTest2 ${ITK_DATA_ROOT}/Input/cake_easy.png ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png)
