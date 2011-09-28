set(DOCUMENTATION "This module contains classes related to level-set
segmentation (new level-set framework from the ITKv4 effort). In level-set
segmentation, the segmentation contour is modeled as the zero level set of a
higher dimensional level-set function. The level-set function evolves according
to a partial differential equation based on image-based features and characteristics
of the level-set function. The segmentation evolves from an initial value for
the segmenting contour.")

if( LevelSetsv4_VIZ )
   itk_module(ITKLevelSetsv4
    DEPENDS
      ITKCommon
      ITKLabelMap
      ITKDistanceMap
      ITKImageGradient
      ITKVtkGlue
    TEST_DEPENDS
      ITKTestKernel
      ITKFastMarching
    DESCRIPTION
      "${DOCUMENTATION}"
  )
else()
  itk_module(ITKLevelSetsv4
    DEPENDS
      ITKCommon
      ITKLabelMap
      ITKDistanceMap
      ITKImageGradient
    TEST_DEPENDS
      ITKTestKernel
      ITKFastMarching
    DESCRIPTION
      "${DOCUMENTATION}"
  )
endif()
