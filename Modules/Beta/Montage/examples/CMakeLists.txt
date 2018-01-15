cmake_minimum_required(VERSION 3.9.5)
project(ITKMontageExamples)

find_package(ITK REQUIRED
  COMPONENTS ITKImageIO ITKTransformIO Montage
  )
include(${ITK_USE_FILE})

add_executable(PhaseCorrelationImageRegistration
  PhaseCorrelationImageRegistration.cxx)
target_link_libraries(PhaseCorrelationImageRegistration ${ITK_LIBRARIES})
