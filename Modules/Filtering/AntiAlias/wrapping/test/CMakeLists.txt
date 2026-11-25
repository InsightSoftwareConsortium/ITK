# Test disabled because input to generate baseline is not known or parameters
# have changed.

#list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
#if(ITK_WRAP_PYTHON AND ITK_WRAP_float AND wrap_2_index GREATER -1)
#  itk_python_add_test(NAME PythonAntiAliasBinaryImageFilterTest
#    TEST_DRIVER_ARGS
#      --compare ${ITK_TEST_OUTPUT_DIR}/PythonAntiAliasBinaryImageFilter.png DATA{Baseline/PythonAntiAliasBinaryImageFilterTest.png}
#    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/AntiAliasBinaryImageFilterTest.py
#      DATA{Input/VentricleModel.png}
#      ${ITK_TEST_OUTPUT_DIR}/PythonAntiAliasBinaryImageFilter.png
#  )
#endif()
