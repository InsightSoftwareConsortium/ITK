itk_python_expression_add_test(
  NAME itkCuberilleImageToMeshFilterPythonTest
  EXPRESSION "filter = itk.CuberilleImageToMeshFilter.New()"
)

set(test_input_dir ${CMAKE_CURRENT_SOURCE_DIR}/../../test/Input)
# cmake_path(CONVERT ${test_input_dir} TO_CMAKE_PATH_LIST test_input_dir NORMALIZE)  # CMake 3.20+

list(FIND ITK_WRAP_IMAGE_DIMS 3 wrap_3_index)
if(wrap_3_index GREATER -1)
  itk_python_add_test(NAME Cuberille_blob0_python
    TEST_DRIVER_ARGS
    COMMAND cuberille_test.py
      DATA{${test_input_dir}/blob0.mha}
      ${ITK_TEST_OUTPUT_DIR}/blob0.vtk
    )

  itk_python_add_test(NAME Cuberille_blob0_100_python
    TEST_DRIVER_ARGS
    COMMAND cuberille_test.py
      DATA{${test_input_dir}/blob0.mha}
      ${ITK_TEST_OUTPUT_DIR}/blob0_100.vtk
        100  # iso-surface value
    )

  itk_python_add_test(NAME Cuberille_hydrogenAtom_35_python
    TEST_DRIVER_ARGS
    COMMAND cuberille_test.py
      DATA{${test_input_dir}/hydrogenAtom.mha}
      ${ITK_TEST_OUTPUT_DIR}/hydrogenAtom35.vtk
        35  # iso-surface value
    )

  itk_python_add_test(NAME Cuberille_hydrogenAtom_50_python
    TEST_DRIVER_ARGS
    COMMAND cuberille_test.py
      DATA{${test_input_dir}/hydrogenAtom.mha}
      ${ITK_TEST_OUTPUT_DIR}/hydrogenAtom50.vtk
        50  # iso-surface value
    )
endif()
