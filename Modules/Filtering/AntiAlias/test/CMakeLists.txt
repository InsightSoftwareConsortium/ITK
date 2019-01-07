itk_module_test()
set(ITKAntiAliasTests
itkAntiAliasBinaryImageFilterTest.cxx
)

CreateTestDriver(ITKAntiAlias  "${ITKAntiAlias-Test_LIBRARIES}" "${ITKAntiAliasTests}")

itk_add_test(NAME itkAntiAliasBinaryImageFilterTest
  COMMAND ITKAntiAliasTestDriver
  --compare DATA{Baseline/itkAntiAliasBinaryImageFilterTestBaseline.mha}
            ${ITK_TEST_OUTPUT_DIR}/itkAntiAliasBinaryImageFilterTest.mha
  itkAntiAliasBinaryImageFilterTest
  ${ITK_TEST_OUTPUT_DIR}/itkAntiAliasBinaryImageFilterTest.mha
  )

list(FIND ITK_WRAP_IMAGE_DIMS 3 wrap_3_index)
if(ITK_WRAP_float AND wrap_3_index GREATER -1)
  itk_python_add_test(NAME PythonAntiAliasBinaryImageFilterTest
    TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonAntiAliasBinaryImageFilterTest.png
                DATA{Baseline/PythonAntiAliasBinaryImageFilterTest.png}
    COMMAND AntiAliasBinaryImageFilterTest.py
      DATA{Input/VentricleModel.png}
      ${ITK_TEST_OUTPUT_DIR}/PythonAntiAliasBinaryImageFilterTest.png
      .001 50
    )
endif()
