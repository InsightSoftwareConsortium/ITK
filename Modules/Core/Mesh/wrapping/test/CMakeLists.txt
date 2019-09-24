list(FIND ITK_WRAP_IMAGE_DIMS 3 wrap_3_index)
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
