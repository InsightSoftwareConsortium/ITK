#
#   Create a library here
#

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/vtkRegistrationConfigure.h.in
               ${CMAKE_CURRENT_BINARY_DIR}/vtkRegistrationConfigure.h
               @ONLY IMMEDIATE)

include_directories(
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}
)

set(Libraries
  ${ITK_LIBRARIES}
  vtkIO
)

SET(VTK_Filter_SRCS
  vtkMicroscopyImageType.h
  vtkMicroscopyTileStitcher.h
  vtkMicroscopyTileStitcher.cxx
  vtkMicroscopyTileConfigParser.h
  vtkMicroscopyTileConfigParser.cxx
  vtkMicroscopyTileStitcherConfig.h
  vtkMicroscopyTileStitcherConfig.cxx
  vtkGlobalPositionOptimizer.h
  vtkGlobalPositionOptimizer.cxx
)

add_library(TileStitching ${VTK_Filter_SRCS})
if (WIN32)
  set_target_properties(TileStitching PROPERTIES COMPILE_FLAGS  " /bigobj")
endif (WIN32)
target_link_libraries(TileStitching ${Libraries})
