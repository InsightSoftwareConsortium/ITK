cmake_minimum_required(VERSION 3.16)
project(ITKMeshToPolyDataExamples)

find_package(ITK REQUIRED
 COMPONENTS
   MeshToPolyData
   ITKMeshIO
   )
find_package(VTK REQUIRED)

include(${ITK_USE_FILE})

add_executable(convert-itk-poly-data-to-vtk-poly-data convert-itk-poly-data-to-vtk-poly-data.cxx)
target_link_libraries(convert-itk-poly-data-to-vtk-poly-data PUBLIC ${ITK_LIBRARIES} ${VTK_LIBRARIES})

enable_testing()
add_test(NAME convert-itk-poly-data-to-vtk-poly-data
 COMMAND convert-itk-poly-data-to-vtk-poly-data
  ${CMAKE_CURRENT_SOURCE_DIR}/cow.vtk
  ${CMAKE_CURRENT_BINARY_DIR}/cow-vtk.vtk)