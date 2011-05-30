set(DOCUMENTATION "This module contains implementations of generalized versions
of the Fast Marching filter. These implementations cover the use of Fast
Marching in both Images and QuadEdgeMeshes.")

itk_module(ITK-FastMarching DEPENDS
ITK-Common
ITK-QuadEdgeMesh
ITK-ConnectedComponents
### test ###
TEST_DEPENDS
ITK-IO-NIFTI
ITK-ImageLabel
ITK-TestKernel
DESCRIPTION "${DOCUMENTATION}"
)
