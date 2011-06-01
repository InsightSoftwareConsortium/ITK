project(MeshExamples)
include_regular_expression("^.*$")

add_executable(PointSet1 PointSet1.cxx )
target_link_libraries(PointSet1 ${ITK_LIBRARIES})

add_executable(PointSet2 PointSet2.cxx )
target_link_libraries(PointSet2 ${ITK_LIBRARIES})

add_executable(PointSet3 PointSet3.cxx )
target_link_libraries(PointSet3 ${ITK_LIBRARIES})

add_executable(RGBPointSet RGBPointSet.cxx )
target_link_libraries(RGBPointSet ${ITK_LIBRARIES})

add_executable(PointSetWithVectors PointSetWithVectors.cxx )
target_link_libraries(PointSetWithVectors ${ITK_LIBRARIES})

add_executable(PointSetWithCovariantVectors PointSetWithCovariantVectors.cxx )
target_link_libraries(PointSetWithCovariantVectors ${ITK_LIBRARIES})

add_executable(Mesh1 Mesh1.cxx )
target_link_libraries(Mesh1 ${ITK_LIBRARIES})

add_executable(Mesh2 Mesh2.cxx )
target_link_libraries(Mesh2 ${ITK_LIBRARIES})

add_executable(Mesh3 Mesh3.cxx )
target_link_libraries(Mesh3 ${ITK_LIBRARIES})

add_executable(MeshTraits MeshTraits.cxx )
target_link_libraries(MeshTraits ${ITK_LIBRARIES})

add_executable(MeshKComplex MeshKComplex.cxx )
target_link_libraries(MeshKComplex ${ITK_LIBRARIES})

add_executable(MeshPolyLine MeshPolyLine.cxx )
target_link_libraries(MeshPolyLine ${ITK_LIBRARIES})

add_executable(MeshCellsIteration MeshCellsIteration.cxx )
target_link_libraries(MeshCellsIteration ${ITK_LIBRARIES})

add_executable(MeshCellVisitor MeshCellVisitor.cxx )
target_link_libraries(MeshCellVisitor ${ITK_LIBRARIES})

add_executable(MeshCellVisitor2 MeshCellVisitor2.cxx )
target_link_libraries(MeshCellVisitor2 ${ITK_LIBRARIES})

add_executable(AutomaticMesh AutomaticMesh.cxx )
target_link_libraries(AutomaticMesh ${ITK_LIBRARIES})

add_executable(ImageToPointSet ImageToPointSet.cxx )
target_link_libraries(ImageToPointSet ${ITK_LIBRARIES})


if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

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

endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )
