set(DOCUMENTATION "The Mesh module contains the datastructures required for
representing N-Dimensional meshes in ITK. The Mesh class is heavily templated
and very generic. It can represent a K-Complex in an N-Dimensional space. Many
of the Mesh properties are defined in Traits helper classes, and then propagate
to the components of the Mesh. These classes are typically used for
representing the outcome of image segmentation.")

itk_module(ITKMesh
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKTransform
  TEST_DEPENDS
    ITKTestKernel
    ITKSpatialObjects
    ITKIOSpatialObjects
    ITKImageGrid
    ITKIOMesh
    ITKMetaIO
    ITKTransform
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency on IO-SpatialObjects is caused by itkMeshSpatialObjectIOTest.
