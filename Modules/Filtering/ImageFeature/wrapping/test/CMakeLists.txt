if(ITK_WRAP_PYTHON)
  set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)
  set(test_baseline_dir ${itk-module_SOURCE_DIR}/test/Baseline)

  itk_python_add_test(NAME itkGradientVectorFlowImageFilterPythonTest
      TEST_DRIVER_ARGS
        --compare ${ITK_TEST_OUTPUT_DIR}/itkGradientVectorFlowImageFilterPythonTestX.png
                  DATA{${test_baseline_dir}/itkGradientVectorFlowImageFilterPythonTestX.png}
        --compare ${ITK_TEST_OUTPUT_DIR}/itkGradientVectorFlowImageFilterPythonTestY.png
                  DATA{${test_baseline_dir}/itkGradientVectorFlowImageFilterPythonTestY.png}
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkGradientVectorFlowImageFilterTest.py
        DATA{${test_input_dir}/Contours.png}
        ${ITK_TEST_OUTPUT_DIR}/itkGradientVectorFlowImageFilterPythonTestX.png
        ${ITK_TEST_OUTPUT_DIR}/itkGradientVectorFlowImageFilterPythonTestY.png
        50 50000.0 0.001)

  list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
  if(ITK_WRAP_float AND wrap_2_index GREATER -1)
    itk_python_add_test(NAME PythonLaplacianImageFilterTest
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/LaplacianImageFilterTest.py
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/LaplacianImageFilterTest.png
      )
    itk_python_add_test(NAME PythonCannyEdgeDetectionImageFilterTest
      TEST_DRIVER_ARGS
        --compareNumberOfPixelsTolerance 1
        --compare ${ITK_TEST_OUTPUT_DIR}/PythonCannyEdgeDetectionImageFilterTest.png
          DATA{Baseline/PythonCannyEdgeDetectionImageFilterTest.png}
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/CannyEdgeDetectionImageFilterTest.py
        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/PythonCannyEdgeDetectionImageFilterTest.png
        9
      )
    itk_python_add_test(NAME PythonHoughTransform2DLinesImageFilterTest
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/HoughTransform2DLinesImageFilterTest.py
        DATA{Baseline/itkHoughTransform2DLinesImageFilterTest.png}
      )

  endif()
endif()
