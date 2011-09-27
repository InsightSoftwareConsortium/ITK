itk_add_test(NAME AutomaticMeshTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:AutomaticMesh>)

itk_add_test(NAME ImageToPointSetTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:ImageToPointSet> ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png)

itk_add_test(NAME Mesh1Test COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:Mesh1>)

itk_add_test(NAME Mesh2Test COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:Mesh2>)

itk_add_test(NAME Mesh3Test COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:Mesh3>)

itk_add_test(NAME MeshCellsIterationTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:MeshCellsIteration>)

itk_add_test(NAME MeshCellVisitorTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:MeshCellVisitor>)

itk_add_test(NAME MeshCellVisitor2Test COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:MeshCellVisitor2>)

itk_add_test(NAME MeshKComplexTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:MeshKComplex>)

itk_add_test(NAME MeshPolyLineTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:MeshPolyLine>)

itk_add_test(NAME MeshTraitsTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:MeshTraits>)

itk_add_test(NAME PointSet1Test COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:PointSet1>)

itk_add_test(NAME PointSet2Test COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:PointSet2>)

itk_add_test(NAME PointSet3Test COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:PointSet3>)

itk_add_test(NAME PointSetWithCovariantVectorsTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:PointSetWithCovariantVectors>)

itk_add_test(NAME PointSetWithVectorsTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:PointSetWithVectors>)

itk_add_test(NAME RGBPointSetTest COMMAND ${ITK_TEST_DRIVER} $<TARGET_FILE:RGBPointSet>)
