itk_module_test()
set(ITKMeshTests
itkAutomaticTopologyMeshSourceTest.cxx
itkImageToParametricSpaceFilterTest.cxx
itkInteriorExteriorMeshFilterTest.cxx
itkParametricSpaceToImageSpaceMeshFilterTest.cxx
itkRegularSphereMeshSourceTest.cxx
itkRegularSphereMeshSourceTest2.cxx
itkSimplexMeshTest.cxx
itkSimplexMeshAdaptTopologyFilterTest.cxx
itkSimplexMeshToTriangleMeshFilterTest.cxx
itkSimplexMeshVolumeCalculatorTest.cxx
itkSphereMeshSourceTest.cxx
itkTransformMeshFilterTest.cxx
itkTriangleMeshToBinaryImageFilterTest.cxx
itkTriangleMeshToBinaryImageFilterTest1.cxx
itkTriangleMeshToBinaryImageFilterTest2.cxx
itkTriangleMeshToBinaryImageFilterTest3.cxx
itkTriangleMeshToBinaryImageFilterTest4.cxx
itkTriangleMeshToSimplexMeshFilterTest.cxx
itkVTKPolyDataReaderTest.cxx
itkVTKPolyDataWriterTest01.cxx
itkVTKPolyDataWriterTest02.cxx
itkWarpMeshFilterTest.cxx
itkMeshTest.cxx
itkBinaryMask3DMeshSourceTest.cxx
itkDynamicMeshTest.cxx
itkExtractMeshConnectedRegionsTest.cxx
itkMeshFstreamTest.cxx
itkMeshSourceGraftOutputTest.cxx
itkMeshSpatialObjectIOTest.cxx
itkTriangleMeshToSimplexMeshFilter2Test.cxx
itkCellInterfaceTest.cxx
itkNewTest.cxx
itkQuadrilateralCellTest.cxx
itkTriangleCellTest.cxx
)

set(ITKMesh-Test_LIBRARIES ${ITKMesh-Test_LIBRARIES})
CreateTestDriver(ITKMesh  "${ITKMesh-Test_LIBRARIES}" "${ITKMeshTests}")

itk_add_test(NAME itkMeshTest
      COMMAND ITKMeshTestDriver itkMeshTest)
itk_add_test(NAME itkSimplexMeshTest
      COMMAND ITKMeshTestDriver itkSimplexMeshTest)
itk_add_test(NAME itkAutomaticTopologyMeshSourceTest
      COMMAND ITKMeshTestDriver itkAutomaticTopologyMeshSourceTest)
itk_add_test(NAME itkBinaryMask3DMeshSourceTest
      COMMAND ITKMeshTestDriver itkBinaryMask3DMeshSourceTest)
itk_add_test(NAME itkImageToParametricSpaceFilterTest
      COMMAND ITKMeshTestDriver itkImageToParametricSpaceFilterTest)
itk_add_test(NAME itkInteriorExteriorMeshFilterTest
      COMMAND ITKMeshTestDriver itkInteriorExteriorMeshFilterTest)
itk_add_test(NAME itkRegularSphereMeshSourceTest
      COMMAND ITKMeshTestDriver itkRegularSphereMeshSourceTest)
itk_add_test(NAME itkRegularSphereMeshSourceTest2
      COMMAND ITKMeshTestDriver itkRegularSphereMeshSourceTest2)
itk_add_test(NAME itkSimplexMeshAdaptTopologyFilterTest
      COMMAND ITKMeshTestDriver itkSimplexMeshAdaptTopologyFilterTest)
itk_add_test(NAME itkSimplexMeshToTriangleMeshFilterTest
      COMMAND ITKMeshTestDriver itkSimplexMeshToTriangleMeshFilterTest)
itk_add_test(NAME itkSimplexMeshVolumeCalculatorTest
      COMMAND ITKMeshTestDriver itkSimplexMeshVolumeCalculatorTest)
itk_add_test(NAME itkSphereMeshSourceTest
      COMMAND ITKMeshTestDriver itkSphereMeshSourceTest)
itk_add_test(NAME itkTransformMeshFilterTest
      COMMAND ITKMeshTestDriver itkTransformMeshFilterTest)
itk_add_test(NAME itkTriangleMeshToBinaryImageFilterTest
      COMMAND ITKMeshTestDriver itkTriangleMeshToBinaryImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest.mha)
itk_add_test(NAME itkTriangleMeshToBinaryImageFilterTest1
      COMMAND ITKMeshTestDriver itkTriangleMeshToBinaryImageFilterTest1
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest1.mha)
itk_add_test(NAME itkTriangleMeshToBinaryImageFilterTest2
      COMMAND ITKMeshTestDriver
              --redirectOutput ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest2.txt
      itkTriangleMeshToBinaryImageFilterTest2
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest2.mha)
set_tests_properties(itkTriangleMeshToBinaryImageFilterTest2 PROPERTIES
      ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest2.txt)
itk_add_test(NAME itkTriangleMeshToBinaryImageFilterTest3
      COMMAND ITKMeshTestDriver itkTriangleMeshToBinaryImageFilterTest3
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk} ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest3.mha 140 160 180 -0.7 -0.8 -0.9 0.01 0.01 0.01)
itk_add_test(NAME itkTriangleMeshToBinaryImageFilterTest4
      COMMAND ITKMeshTestDriver itkTriangleMeshToBinaryImageFilterTest4
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk} ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest4.mha 140 160 180 -0.7 -0.8 -0.9 0.01 0.01 0.01)
itk_add_test(NAME itkTriangleMeshToSimplexMeshFilterTest
      COMMAND ITKMeshTestDriver itkTriangleMeshToSimplexMeshFilterTest)
itk_add_test(NAME itkVTKPolyDataReaderTest
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk})
itk_add_test(NAME itkVTKPolyDataReaderTest2
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/sphereBinary.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest0
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad0.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest1
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad1.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest2
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad2.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest3
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad3.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest4
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad4.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest5
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad5.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest6
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad6.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest7
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad7.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest8
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad8.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest9
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad9.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest10
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad10.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest11
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad11.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest12
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad12.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest13
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad13.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest14
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad14.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest15
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad15.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest16
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad16.vtk})
itk_add_test(NAME itkVTKPolyDataReaderBadTest17
      COMMAND ITKMeshTestDriver itkVTKPolyDataReaderTest
              DATA{${ITK_DATA_ROOT}/Input/vtkPolyDataBad17.vtk})
itk_add_test(NAME itkVTKPolyDataWriterTest01
      COMMAND ITKMeshTestDriver itkVTKPolyDataWriterTest01
              ${ITK_TEST_OUTPUT_DIR}/VTKPolyDataWriterTest01.vtk)
itk_add_test(NAME itkVTKPolyDataWriterTest02
      COMMAND ITKMeshTestDriver itkVTKPolyDataWriterTest02
              ${ITK_TEST_OUTPUT_DIR}/VTKPolyDataWriterTest02.vtk)
itk_add_test(NAME itkWarpMeshFilterTest
      COMMAND ITKMeshTestDriver itkWarpMeshFilterTest)
itk_add_test(NAME itkDynamicMeshTest
      COMMAND ITKMeshTestDriver itkDynamicMeshTest)
itk_add_test(NAME itkExtractMeshConnectedRegionsTest
      COMMAND ITKMeshTestDriver itkExtractMeshConnectedRegionsTest)
itk_add_test(NAME itkMeshFstreamTest
      COMMAND ITKMeshTestDriver itkMeshFstreamTest
              ${ITK_TEST_OUTPUT_DIR}/testMeshFstream.txt)
itk_add_test(NAME itkMeshSourceGraftOutputTest
      COMMAND ITKMeshTestDriver itkMeshSourceGraftOutputTest)
itk_add_test(NAME itkMeshSpatialObjectIOTest
      COMMAND ITKMeshTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest.txt)
itk_add_test(NAME itkMeshSpatialObjectIOTest1
      COMMAND ITKMeshTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest1.txt binary)
itk_add_test(NAME itkMeshSpatialObjectIOTest2
      COMMAND ITKMeshTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest2.txt DATA{${ITK_DATA_ROOT}/Input/metamesh.txt})
itk_add_test(NAME itkParametricSpaceToImageSpaceMeshFilterTest
      COMMAND ITKMeshTestDriver itkParametricSpaceToImageSpaceMeshFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png})
itk_add_test(NAME itkTriangleMeshToSimplexMeshFilter2Test
      COMMAND ITKMeshTestDriver itkTriangleMeshToSimplexMeshFilter2Test)

itk_add_test(NAME itkNewTest COMMAND ITKMeshTestDriver itkNewTest)
itk_add_test(NAME itkCellInterfaceTest COMMAND ITKMeshTestDriver itkCellInterfaceTest)
itk_add_test(NAME itkTriangleCellTest COMMAND ITKMeshTestDriver itkTriangleCellTest)
itk_add_test(NAME itkQuadrilateralCellTest COMMAND ITKMeshTestDriver itkQuadrilateralCellTest)


set_tests_properties(itkVTKPolyDataReaderTest2
   itkVTKPolyDataReaderBadTest0
   itkVTKPolyDataReaderBadTest1
   itkVTKPolyDataReaderBadTest2
   itkVTKPolyDataReaderBadTest3
   itkVTKPolyDataReaderBadTest4
   itkVTKPolyDataReaderBadTest5
   itkVTKPolyDataReaderBadTest6
   itkVTKPolyDataReaderBadTest7
   itkVTKPolyDataReaderBadTest8
   itkVTKPolyDataReaderBadTest9
   itkVTKPolyDataReaderBadTest10
   itkVTKPolyDataReaderBadTest11
   itkVTKPolyDataReaderBadTest12
   itkVTKPolyDataReaderBadTest13
   itkVTKPolyDataReaderBadTest14
   itkVTKPolyDataReaderBadTest15
   itkVTKPolyDataReaderBadTest16
   itkVTKPolyDataReaderBadTest17
   PROPERTIES WILL_FAIL true)

 itk_python_expression_add_test(NAME itkBYUMeshIOPythonTest
   EXPRESSION "instance = itk.BYUMeshIO.New()")
 itk_python_expression_add_test(NAME itkFreeSurferAsciiMeshIOPythonTest
   EXPRESSION "instance = itk.FreeSurferAsciiMeshIO.New()")
 itk_python_expression_add_test(NAME itkFreeSurferBinaryMeshIOPythonTest
   EXPRESSION "instance = itk.FreeSurferBinaryMeshIO.New()")
 itk_python_expression_add_test(NAME itkMeshFileReaderPythonTest
   EXPRESSION "instance = itk.MeshFileReader.New()")
 itk_python_expression_add_test(NAME itkMeshFileWriterPythonTest
   EXPRESSION "instance = itk.MeshFileWriter.New()")
 itk_python_expression_add_test(NAME itkOBJMeshIOPythonTest
   EXPRESSION "instance = itk.OBJMeshIO.New()")
 itk_python_expression_add_test(NAME itkOFFMeshIOPythonTest
   EXPRESSION "instance = itk.OFFMeshIO.New()")
 itk_python_expression_add_test(NAME itkVTKPolyDataMeshIOPythonTest
   EXPRESSION "instance = itk.VTKPolyDataMeshIO.New()")
