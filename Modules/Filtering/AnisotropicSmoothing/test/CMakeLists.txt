itk_module_test()
set(ITKAnisotropicSmoothingTests
itkGradientAnisotropicDiffusionImageFilterTest.cxx
itkCurvatureAnisotropicDiffusionImageFilterTest.cxx
itkMinMaxCurvatureFlowImageFilterTest.cxx
itkVectorAnisotropicDiffusionImageFilterTest.cxx
itkGradientAnisotropicDiffusionImageFilterTest2.cxx
)

CreateTestDriver(ITKAnisotropicSmoothing  "${ITKAnisotropicSmoothing-Test_LIBRARIES}" "${ITKAnisotropicSmoothingTests}")

itk_add_test(NAME itkGradientAnisotropicDiffusionImageFilterTest
      COMMAND ITKAnisotropicSmoothingTestDriver itkGradientAnisotropicDiffusionImageFilterTest)
itk_add_test(NAME itkCurvatureAnisotropicDiffusionImageFilterTest
      COMMAND ITKAnisotropicSmoothingTestDriver itkCurvatureAnisotropicDiffusionImageFilterTest)
itk_add_test(NAME itkMinMaxCurvatureFlowImageFilterTest
      COMMAND ITKAnisotropicSmoothingTestDriver itkMinMaxCurvatureFlowImageFilterTest)
itk_add_test(NAME itkVectorAnisotropicDiffusionImageFilterTest
      COMMAND ITKAnisotropicSmoothingTestDriver itkVectorAnisotropicDiffusionImageFilterTest)
itk_add_test(NAME itkGradientAnisotropicDiffusionImageFilterTest2
      COMMAND ITKAnisotropicSmoothingTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/GradientAnisotropicDiffusionImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png
    itkGradientAnisotropicDiffusionImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/cake_easy.png} ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png)
