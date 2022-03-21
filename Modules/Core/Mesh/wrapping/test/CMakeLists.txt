list(FIND ITK_WRAP_IMAGE_DIMS 3 wrap_3_index)
if(ITK_WRAP_PYTHON)
  if(wrap_3_index GREATER -1)
    itk_python_expression_add_test(NAME itkTriangleMeshToBinaryImageFilterPythonTest
      EXPRESSION "instance = itk.TriangleMeshToBinaryImageFilter.New()")
  endif()

  itk_python_expression_add_test(NAME itkBYUMeshIOPythonTest
   EXPRESSION "instance = itk.BYUMeshIO.New()")
  itk_python_expression_add_test(NAME itkFreeSurferAsciiMeshIOPythonTest
   EXPRESSION "instance = itk.FreeSurferAsciiMeshIO.New()")
  itk_python_expression_add_test(NAME itkFreeSurferBinaryMeshIOPythonTest
   EXPRESSION "instance = itk.FreeSurferBinaryMeshIO.New()")
  itk_python_expression_add_test(NAME itkMeshFileWriterPythonTest
   EXPRESSION "instance = itk.MeshFileWriter.New()")
  itk_python_expression_add_test(NAME itkOBJMeshIOPythonTest
   EXPRESSION "instance = itk.OBJMeshIO.New()")
  itk_python_expression_add_test(NAME itkOFFMeshIOPythonTest
   EXPRESSION "instance = itk.OFFMeshIO.New()")
  itk_python_expression_add_test(NAME itkVTKPolyDataMeshIOPythonTest
   EXPRESSION "instance = itk.VTKPolyDataMeshIO.New()")
  itk_python_expression_add_test(NAME itkTransformMeshFilterPythonTest
   EXPRESSION "instance = itk.TransformMeshFilter.New()")
  itk_python_expression_add_test(NAME itkRegularSphereMeshSourcePythonTest
   EXPRESSION "instance = itk.RegularSphereMeshSource.New()")
  itk_python_expression_add_test(NAME itkSphereMeshSourcePythonTest
   EXPRESSION "instance = itk.SphereMeshSource.New()")

  itk_python_add_test(NAME itkMeshArrayPixelTypePythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkMeshArrayPixelTypeTest.py)
  itk_python_add_test(NAME itkConnectedRegionsMeshFilterPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkConnectedRegionsMeshFilterTest.py DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} 15 2154)
  itk_python_add_test(NAME itkMeshSerializationTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkMeshSerializationTest.py DATA{${ITK_DATA_ROOT}/Input/mushroom.vtk})
endif()
