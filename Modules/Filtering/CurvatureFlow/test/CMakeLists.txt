itk_module_test()
set(ITKCurvatureFlowTests
itkBinaryMinMaxCurvatureFlowImageFilterTest.cxx
itkCurvatureFlowTest.cxx
)

CreateTestDriver(ITKCurvatureFlow  "${ITKCurvatureFlow-Test_LIBRARIES}" "${ITKCurvatureFlowTests}")

itk_add_test(NAME itkBinaryMinMaxCurvatureFlowImageFilterTest
      COMMAND ITKCurvatureFlowTestDriver itkBinaryMinMaxCurvatureFlowImageFilterTest)
itk_add_test(NAME itkCurvatureFlowTesti
      COMMAND ITKCurvatureFlowTestDriver itkCurvatureFlowTest ${ITK_TEST_OUTPUT_DIR}/itkCurvatureFlowTest.vtk)

list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonCurvatureFlowImageFilterTest
    COMMAND CurvatureFlowImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/CurvatureFlowImageFilterTest.png
      5 0.125 1.0
    )
endif()
