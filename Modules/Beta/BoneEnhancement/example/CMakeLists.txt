cmake_minimum_required(VERSION 3.0)
set(module BoneEnhancementExamples)
project(${module})

find_package(ITK REQUIRED COMPONENTS BoneEnhancement ITKIOImageBase ITKIONIFTI ITKIONRRD)
include(${ITK_USE_FILE})
include_directories(../include)

set(${module}_SRC
    computeKrcahBoneEnhancement.cxx
)

add_executable(${module} ${${module}_SRC} )
target_link_libraries(${module} ${ITK_LIBRARIES})

set(INSTALL_RUNTIME_DESTINATION bin CACHE STRING "Install destination")

install(TARGETS ${module} DESTINATION ${INSTALL_RUNTIME_DESTINATION})
