cmake_minimum_required(VERSION 2.8)
if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)


project(SimpleImageReconstruction)

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

include(CTest)
include(CPack)

add_executable(SimpleImageReconstruction SimpleImageReconstruction.cxx )
target_link_libraries(SimpleImageReconstruction ITKIO)

add_executable(AffineImageReconstruction AffineImageReconstruction.cxx )
target_link_libraries(AffineImageReconstruction ITKIO ITKNumerics)

add_executable(PrintImageInformation PrintImageInformation.cxx )
target_link_libraries(PrintImageInformation ITKIO)

install(TARGETS SimpleImageReconstruction AffineImageReconstruction PrintImageInformation
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)
