if(ITK_WRAP_PYTHON)
  itk_python_add_test(NAME itkRansacMeshTestPython COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkRansacMeshTest.py 0.3 100 100 DATA{${CMAKE_SOURCE_DIR}/test/Baseline/movingFeatureMesh.vtk} DATA{${CMAKE_SOURCE_DIR}/test/Baseline/fixedFeatureMesh.vtk} DATA{${CMAKE_SOURCE_DIR}/test/Baseline/movingMesh.vtk} DATA{${CMAKE_SOURCE_DIR}/test/Baseline/fixedMesh.vtk} )
endif()
