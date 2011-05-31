# Include the midas script
include(${Video-Core-Common_SOURCE_DIR}/MIDAS.cmake)

# Macro to add a midas test with the "Video" label
macro(add_video_test)
  midas_add_test(${ARGV})
  set_property(TEST ${ARGV1} PROPERTY LABELS Video)
endmacro(add_video_test)
