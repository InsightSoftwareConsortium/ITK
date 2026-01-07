project(MeshExamples)

add_executable(PointSet1 PointSet1.cxx)
target_link_libraries(PointSet1 PRIVATE ITKCommonModule)

add_executable(PointSet2 PointSet2.cxx)
target_link_libraries(PointSet2 PRIVATE ITKCommonModule)

add_executable(PointSet3 PointSet3.cxx)
target_link_libraries(PointSet3 PRIVATE ITKCommonModule)

add_executable(RGBPointSet RGBPointSet.cxx)
target_link_libraries(RGBPointSet PRIVATE ITKCommonModule)

add_executable(PointSetWithVectors PointSetWithVectors.cxx)
target_link_libraries(PointSetWithVectors PRIVATE ITKCommonModule)

add_executable(PointSetWithCovariantVectors PointSetWithCovariantVectors.cxx)
target_link_libraries(PointSetWithCovariantVectors PRIVATE ITKCommonModule)

add_executable(Mesh1 Mesh1.cxx)
target_link_libraries(Mesh1 PRIVATE ITKMeshModule)

add_executable(Mesh2 Mesh2.cxx)
target_link_libraries(
  Mesh2
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(Mesh3 Mesh3.cxx)
target_link_libraries(
  Mesh3
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(MeshTraits MeshTraits.cxx)
target_link_libraries(
  MeshTraits
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(MeshKComplex MeshKComplex.cxx)
target_link_libraries(
  MeshKComplex
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(MeshPolyLine MeshPolyLine.cxx)
target_link_libraries(
  MeshPolyLine
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(MeshCellsIteration MeshCellsIteration.cxx)
target_link_libraries(
  MeshCellsIteration
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(MeshCellVisitor MeshCellVisitor.cxx)
target_link_libraries(
  MeshCellVisitor
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(MeshCellVisitor2 MeshCellVisitor2.cxx)
target_link_libraries(
  MeshCellVisitor2
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(AutomaticMesh AutomaticMesh.cxx)
target_link_libraries(
  AutomaticMesh
  PRIVATE
    ITKCommonModule
    ITKMeshModule
)

add_executable(ImageToPointSet ImageToPointSet.cxx)
target_link_libraries(
  ImageToPointSet
  PRIVATE
    ITKImageIO
    ITKCommonModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
