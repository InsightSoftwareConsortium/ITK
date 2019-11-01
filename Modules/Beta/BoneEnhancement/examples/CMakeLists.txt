cmake_minimum_required(VERSION 3.10.2)
project(BoneEnhancementExamples)

find_package(ITK REQUIRED COMPONENTS BoneEnhancement ITKIOImageBase ITKIONIFTI ITKIONRRD)
include(${ITK_USE_FILE})

add_executable(computeKrcahBoneEnhancement computeKrcahBoneEnhancement.cxx)
target_link_libraries(computeKrcahBoneEnhancement ${ITK_LIBRARIES})

add_executable(computeDescoteauxBoneEnhancement computeDescoteauxBoneEnhancement.cxx)
target_link_libraries(computeDescoteauxBoneEnhancement ${ITK_LIBRARIES})

set(INSTALL_RUNTIME_DESTINATION bin CACHE STRING "Install destination")

install(
  TARGETS computeKrcahBoneEnhancement computeDescoteauxBoneEnhancement
  DESTINATION ${INSTALL_RUNTIME_DESTINATION}
  )
