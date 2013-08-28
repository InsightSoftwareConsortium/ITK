set(DOCUMENTATION "This module constains classes related to the
visualization of the level-sets.")

itk_module( ITKLevelSetsv4Visualization
  DEPENDS
    ITKLevelSetsv4
    ITKVtkGlue
  TEST_DEPENDS
    ITKTestKernel
    EXCLUDE_FROM_DEFAULT
  DESCRIPTION
   "${DOCUMENTATION}"
  )
