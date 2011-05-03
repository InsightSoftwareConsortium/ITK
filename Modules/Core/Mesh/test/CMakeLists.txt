itk_module_test()
set(ITK-MeshTests
itkAutomaticTopologyMeshSourceTest.cxx
itkImageToParametricSpaceFilterTest.cxx
itkInteriorExteriorMeshFilterTest.cxx
itkParametricSpaceToImageSpaceMeshFilterTest.cxx
itkRegularSphereMeshSourceTest.cxx
itkSimplexMeshTest.cxx
itkSimplexMeshAdaptTopologyFilterTest.cxx
itkSimplexMeshToTriangleMeshFilterTest.cxx
itkSimplexMeshVolumeCalculatorTest.cxx
itkSphereMeshSourceTest.cxx
itkTransformMeshFilterTest.cxx
itkTriangleMeshToBinaryImageFilterTest.cxx
itkTriangleMeshToBinaryImageFilterTest2.cxx
itkTriangleMeshToBinaryImageFilterTest3.cxx
itkTriangleMeshToSimplexMeshFilterTest.cxx
itkVTKPolyDataReaderTest.cxx
itkVTKPolyDataWriterTest01.cxx
itkVTKPolyDataWriterTest02.cxx
itkWarpMeshFilterTest.cxx
itkMeshHeaderTest.cxx
itkMeshTest.cxx
itkBinaryMask3DMeshSourceTest.cxx
itkDynamicMeshTest.cxx
itkExtractMeshConnectedRegionsTest.cxx
itkMeshFstreamTest.cxx
itkMeshSourceGraftOutputTest.cxx
itkMeshSpatialObjectIOTest.cxx
itkTriangleMeshToSimplexMeshFilter2Test.cxx
)

CreateTestDriver(ITK-Mesh  "${ITK-Mesh-Test_LIBRARIES}" "${ITK-MeshTests}")

itk_add_test(NAME itkMeshHeaderTest
      COMMAND ITK-MeshTestDriver itkMeshHeaderTest)
itk_add_test(NAME itkMeshTest
      COMMAND ITK-MeshTestDriver itkMeshTest)
itk_add_test(NAME itkSimplexMeshTest
      COMMAND ITK-MeshTestDriver itkSimplexMeshTest)
itk_add_test(NAME itkAutomaticTopologyMeshSourceTest
      COMMAND ITK-MeshTestDriver itkAutomaticTopologyMeshSourceTest)
itk_add_test(NAME itkBinaryMask3DMeshSourceTest
      COMMAND ITK-MeshTestDriver itkBinaryMask3DMeshSourceTest)
itk_add_test(NAME itkImageToParametricSpaceFilterTest
      COMMAND ITK-MeshTestDriver itkImageToParametricSpaceFilterTest)
itk_add_test(NAME itkInteriorExteriorMeshFilterTest
      COMMAND ITK-MeshTestDriver itkInteriorExteriorMeshFilterTest)
itk_add_test(NAME itkRegularSphereMeshSourceTest
      COMMAND ITK-MeshTestDriver itkRegularSphereMeshSourceTest)
itk_add_test(NAME itkSimplexMeshAdaptTopologyFilterTest
      COMMAND ITK-MeshTestDriver itkSimplexMeshAdaptTopologyFilterTest)
itk_add_test(NAME itkSimplexMeshToTriangleMeshFilterTest
      COMMAND ITK-MeshTestDriver itkSimplexMeshToTriangleMeshFilterTest)
itk_add_test(NAME itkSimplexMeshVolumeCalculatorTest
      COMMAND ITK-MeshTestDriver itkSimplexMeshVolumeCalculatorTest)
itk_add_test(NAME itkSphereMeshSourceTest
      COMMAND ITK-MeshTestDriver itkSphereMeshSourceTest)
itk_add_test(NAME itkTransformMeshFilterTest
      COMMAND ITK-MeshTestDriver itkTransformMeshFilterTest)
itk_add_test(NAME itkTriangleMeshToBinaryImageFilterTest
      COMMAND ITK-MeshTestDriver itkTriangleMeshToBinaryImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest.mha)
itk_add_test(NAME itkTriangleMeshToBinaryImageFilterTest2
      COMMAND ITK-MeshTestDriver itkTriangleMeshToBinaryImageFilterTest2
              ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest2.mha)
itk_add_test(NAME itkTriangleMeshToBinaryImageFilterTest3
      COMMAND ITK-MeshTestDriver itkTriangleMeshToBinaryImageFilterTest3
              ${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk ${ITK_TEST_OUTPUT_DIR}/itkTriangleMeshToBinaryImageFilterTest3.mha 140 160 180 -0.7 -0.8 -0.9 0.01 0.01 0.01)
itk_add_test(NAME itkTriangleMeshToSimplexMeshFilterTest
      COMMAND ITK-MeshTestDriver itkTriangleMeshToSimplexMeshFilterTest)
itk_add_test(NAME itkVTKPolyDataReaderTest
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/genusZeroSurface01.vtk)
itk_add_test(NAME itkVTKPolyDataReaderTest2
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/sphereBinary.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest0
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad0.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest1
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad1.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest2
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad2.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest3
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad3.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest4
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad4.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest5
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad5.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest6
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad6.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest7
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad7.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest8
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad8.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest9
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad9.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest10
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad10.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest11
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad11.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest12
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad12.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest13
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad13.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest14
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad14.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest15
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad15.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest16
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad16.vtk)
itk_add_test(NAME itkVTKPolyDataReaderBadTest17
      COMMAND ITK-MeshTestDriver itkVTKPolyDataReaderTest
              ${ITK_DATA_ROOT}/Input/vtkPolyDataBad17.vtk)
itk_add_test(NAME itkVTKPolyDataWriterTest01
      COMMAND ITK-MeshTestDriver itkVTKPolyDataWriterTest01
              ${ITK_TEST_OUTPUT_DIR}/VTKPolyDataWriterTest01.vtk)
itk_add_test(NAME itkVTKPolyDataWriterTest02
      COMMAND ITK-MeshTestDriver itkVTKPolyDataWriterTest02
              ${ITK_TEST_OUTPUT_DIR}/VTKPolyDataWriterTest02.vtk)
itk_add_test(NAME itkWarpMeshFilterTest
      COMMAND ITK-MeshTestDriver itkWarpMeshFilterTest)
itk_add_test(NAME itkDynamicMeshTest
      COMMAND ITK-MeshTestDriver itkDynamicMeshTest)
itk_add_test(NAME itkExtractMeshConnectedRegionsTest
      COMMAND ITK-MeshTestDriver itkExtractMeshConnectedRegionsTest)
itk_add_test(NAME itkMeshFstreamTest
      COMMAND ITK-MeshTestDriver itkMeshFstreamTest
              ${ITK_TEST_OUTPUT_DIR}/testMeshFstream.txt)
itk_add_test(NAME itkMeshSourceGraftOutputTest
      COMMAND ITK-MeshTestDriver itkMeshSourceGraftOutputTest)
itk_add_test(NAME itkMeshSpatialObjectIOTest
      COMMAND ITK-MeshTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest.txt)
itk_add_test(NAME itkMeshSpatialObjectIOTest1
      COMMAND ITK-MeshTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest1.txt binary)
itk_add_test(NAME itkMeshSpatialObjectIOTest2
      COMMAND ITK-MeshTestDriver itkMeshSpatialObjectIOTest
              ${ITK_TEST_OUTPUT_DIR}/metameshIOTest2.txt ${ITK_DATA_ROOT}/Input/metamesh.txt)
itk_add_test(NAME itkTriangleMeshToSimplexMeshFilter2Test
      COMMAND ITK-MeshTestDriver itkTriangleMeshToSimplexMeshFilter2Test)



SET_TESTS_PROPERTIES(itkVTKPolyDataReaderTest2
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
