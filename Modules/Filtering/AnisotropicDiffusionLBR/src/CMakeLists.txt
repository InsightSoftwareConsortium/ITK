add_subdirectory("Headers")

include_directories(${PROJECT_DIR} ${CMAKE_CURRENT_SOURCE_DIR})

set(${itk-module}_SRC
CoherenceEnhancingDiffusion.cxx
)

add_library(${itk-module} ${${itk-module}_SRC} ${ADLBR_Headers})

target_link_libraries(${itk-module}  ${${itk-module}_LIBRARIES})
itk_module_target(${itk-module})