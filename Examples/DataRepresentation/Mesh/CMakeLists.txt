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

add_test(NAME AutomaticMeshTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:AutomaticMesh>)

add_test(NAME ImageToPointSetTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:ImageToPointSet> ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png)

add_test(NAME Mesh1Test COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:Mesh1>)

add_test(NAME Mesh2Test COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:Mesh2>)

add_test(NAME Mesh3Test COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:Mesh3>)

add_test(NAME MeshCellsIterationTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:MeshCellsIteration>)

add_test(NAME MeshCellVisitorTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:MeshCellVisitor>)

add_test(NAME MeshCellVisitor2Test COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:MeshCellVisitor2>)

add_test(NAME MeshKComplexTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:MeshKComplex>)

add_test(NAME MeshPolyLineTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:MeshPolyLine>)

add_test(NAME MeshTraitsTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:MeshTraits>)

add_test(NAME PointSet1Test COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:PointSet1>)

add_test(NAME PointSet2Test COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:PointSet2>)

add_test(NAME PointSet3Test COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:PointSet3>)

add_test(NAME PointSetWithCovariantVectorsTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:PointSetWithCovariantVectors>)

add_test(NAME PointSetWithVectorsTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:PointSetWithVectors>)

add_test(NAME RGBPointSetTest COMMAND ${ITK_TEST_DRIVER} --process $<TARGET_FILE:RGBPointSet>)

endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )
