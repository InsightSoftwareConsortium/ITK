set(DOCUMENTATION "This module provides classes to perform Voronoi segmentation
on scalar or RGB images.  Voronoi segmentation is a region based segmentation
technique.  A Voronoi diagram is created from a set of seed points in the image.
Each resulting Voronoi cell is classified as internal or external to the
structure of interest based on statistical classification criteria applied to
its contents.  The boundary of the internal-external regions are iteratively
refined by adding seed points to the boundary regions.")

itk_module(ITKVoronoi
  DEPENDS
    ITKImageFilterBase
    ITKMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKSmoothing
    ITKImageFusion
    ITKIOMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra dependency on Smoothing is introduced by itkVoronoiPartitioningImageFilterTest.
