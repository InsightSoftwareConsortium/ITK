itk_module(ITK-Voronoi
  DEPENDS
    ITK-ImageFilterBase
    ITK-Mesh
  TEST_DEPENDS
    ITK-TestKernel
    ITK-Smoothing
)

# Extra dependency on Smoothing is introduced by itkVoronoiPartitioningImageFilterTest.
