list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_PYTHON AND ITK_WRAP_float AND wrap_2_index GREATER -1)
  itk_python_add_test(NAME PythonSpatialObjectTest
    TEST_DRIVER_ARGS
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/SpatialObjectTest.py
    )
endif()
