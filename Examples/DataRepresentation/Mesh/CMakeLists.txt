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

set(MESH_EXAMPLES ${CXX_TEST_PATH}/MeshExamples)

add_executable(MeshExamples MeshExamples.cxx )
target_link_libraries(MeshExamples ${ITK_LIBRARIES})

if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

add_test(AutomaticMeshTest ${CXX_TEST_PATH}/MeshExamples AutomaticMeshTest)

add_test(ImageToPointSetTest ${CXX_TEST_PATH}/MeshExamples
        ImageToPointSetTest ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png)

add_test(Mesh1Test ${CXX_TEST_PATH}/MeshExamples Mesh1Test)

add_test(Mesh2Test ${CXX_TEST_PATH}/MeshExamples Mesh2Test)

add_test(Mesh3Test ${CXX_TEST_PATH}/MeshExamples Mesh3Test)

add_test(MeshCellsIterationTest ${CXX_TEST_PATH}/MeshExamples MeshCellsIterationTest)

add_test(MeshCellVisitorTest ${CXX_TEST_PATH}/MeshExamples MeshCellVisitorTest)

add_test(MeshCellVisitor2Test ${CXX_TEST_PATH}/MeshExamples MeshCellVisitor2Test)

add_test(MeshKComplexTest ${CXX_TEST_PATH}/MeshExamples MeshKComplexTest)

add_test(MeshPolyLineTest ${CXX_TEST_PATH}/MeshExamples MeshPolyLineTest)

add_test(MeshTraitsTest ${CXX_TEST_PATH}/MeshExamples MeshTraitsTest)

add_test(PointSet1Test ${CXX_TEST_PATH}/MeshExamples PointSet1Test)

add_test(PointSet2Test ${CXX_TEST_PATH}/MeshExamples PointSet2Test)

add_test(PointSet3Test ${CXX_TEST_PATH}/MeshExamples PointSet3Test)

add_test(PointSetWithCovariantVectorsTest ${CXX_TEST_PATH}/MeshExamples PointSetWithCovariantVectorsTest)

add_test(PointSetWithVectorsTest ${CXX_TEST_PATH}/MeshExamples PointSetWithVectorsTest)

add_test(RGBPointSetTest ${CXX_TEST_PATH}/MeshExamples RGBPointSetTest)

endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )


