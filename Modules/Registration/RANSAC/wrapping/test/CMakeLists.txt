if(ITK_WRAP_PYTHON)
  itk_python_add_test(NAME itkRansacMeshTestPython COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkRansacMeshTest.py 3 100 100 DATA{Baseline/movingFeatureMesh.vtk} DATA{Baseline/fixedFeatureMesh.vtk} DATA{Baseline/movingMesh.vtk} DATA{Baseline/fixedMesh.vtk} )
endif()
