set(DOCUMENTATION "Spatial Objects are intended to represent regions in space.
The basic functionality of a Spatial Object is to answer the question of
whether a physical point is inside or outside of the Spatial Object. They are
commonly used for representing masks in an analytical form, as well as
approximations of shape by combining them into hierarchical structures similar
to scene graphs.")

itk_module(ITK-SpatialObjects DEPENDS  ITK-ImageFunction ITK-Mesh ITK-IO-Base TEST_DEPENDS ITK-TestKernel DESCRIPTION "${DOCUMENTATION}")
