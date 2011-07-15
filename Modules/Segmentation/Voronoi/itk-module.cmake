itk_module(ITKVoronoi
  DEPENDS
    ITKImageFilterBase
    ITKMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKSmoothing
)

# Extra dependency on Smoothing is introduced by itkVoronoiPartitioningImageFilterTest.
