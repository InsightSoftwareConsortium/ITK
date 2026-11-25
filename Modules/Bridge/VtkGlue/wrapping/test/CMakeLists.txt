list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(
  ITK_WRAP_PYTHON
  AND
    VTK_WRAP_PYTHON
  AND
    ITK_WRAP_float
  AND
    wrap_2_index
      GREATER
      -1
)
  itk_python_add_test(
    NAME PythonVtkGlueABI3EncodingTest
    COMMAND
      ${CMAKE_CURRENT_SOURCE_DIR}/VtkGlueABI3EncodingTest.py
  )
  itk_python_add_test(
    NAME PythonVtkGlueRoundTripTest
    COMMAND
      ${CMAKE_CURRENT_SOURCE_DIR}/VtkGlueRoundTripTest.py
  )
  itk_python_add_test(
    NAME PythonVtkGlueTypemapCoverageTest
    COMMAND
      ${CMAKE_CURRENT_SOURCE_DIR}/VtkGlueTypemapCoverageTest.py
  )
  # itkTestDriver prepends ITK's own entries to whatever PYTHONPATH it inherits.
  set_property(
    TEST
      PythonVtkGlueABI3EncodingTest
      PythonVtkGlueRoundTripTest
      PythonVtkGlueTypemapCoverageTest
    PROPERTY
      ENVIRONMENT
        "PYTHONPATH=${VTK_PREFIX_PATH}/${VTK_PYTHONPATH}"
  )
endif()
