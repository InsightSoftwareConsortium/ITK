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

list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonCurvatureAnisotropicDiffusionImageFilterTest
    COMMAND CurvatureAnisotropicDiffusionImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/CurvatureAnisotropicDiffusionImageFilterTest.png
      5 0.125 3
    )
  itk_python_add_test(NAME PythonSmoothingRecursiveGaussianImageFilterTest
    COMMAND SmoothingRecursiveGaussianImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/SmoothingRecursiveGaussianImageFilterTest.png
      3
    )
  itk_python_add_test(NAME PythonGradientAnisotropicDiffusionImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonGradientAnisotropicDiffusionImageFilterTest.png
                DATA{Baseline/PythonGradientAnisotropicDiffusionImageFilterTest.png}
    COMMAND GradientAnisotropicDiffusionImageFilterTest.py
      DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonGradientAnisotropicDiffusionImageFilterTest.png
      5 0.125 1.0
    )
endif()
