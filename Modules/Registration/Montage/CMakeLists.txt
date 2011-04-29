cmake_minimum_required(VERSION 2.8)
if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)


project(SimpleImageReconstruction)

find_package(ITK REQUIRED)
include(${ITK_USE_FILE})

include(CMake/PlatformSpecificChecks.cmake)

include(CTest)
include(CPack)

add_executable(SimpleImageReconstruction SimpleImageReconstruction.cxx )
target_link_libraries(SimpleImageReconstruction ${ITK_LIBRARIES})

add_executable(AffineImageReconstruction AffineImageReconstruction.cxx )
target_link_libraries(AffineImageReconstruction ${ITK_LIBRARIES})

add_executable(PrintImageInformation PrintImageInformation.cxx )
target_link_libraries(PrintImageInformation ${ITK_LIBRARIES})

add_executable(ImageReadImageSeriesWrite ImageReadImageSeriesWrite.cxx )
target_link_libraries(ImageReadImageSeriesWrite ${ITK_LIBRARIES})

add_executable(ShapeContinuityValidation ShapeContinuityValidation.cxx )
target_link_libraries(ShapeContinuityValidation ${ITK_LIBRARIES})

add_executable(SpacingValidation SpacingValidation.cxx )
target_link_libraries(SpacingValidation ${ITK_LIBRARIES})

install(TARGETS SimpleImageReconstruction AffineImageReconstruction PrintImageInformation
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)
