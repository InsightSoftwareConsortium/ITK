set(DOCUMENTATION "The QuadEdgeMesh module contain a specialized set of Mesh
classes intended to represent 2-manifolds embedded in a nD space. This family
of classes provides a consistent representation of oriented surfaces and
therefore they are used as a base for implementing common mesh filters and
operations. They are commonly used for representing the output of image
segmentation algorithms.

Some QuadEdgeMesh algorithms are based on iterating various connectivity
operators e.g. curvature driven surface deformation. Many of those connectivity
altering operators (e.g. the Euler operators) are lightweight in the sense that
they only modify very limited regions of a QuadEdgeMesh: they typically act
within the range of couple edges of distance from a considered vertex, edge or
face.

On the one side, those atomic operations cannot be implemented as \"classical\"
ITK filters since each filter invocation yields a new copy of the input mesh
as its output: this would drastically increase the memory consumption. In fact,
those atomic operations have a too much finer grain to be implemented as
filters: the filter is more at the scale of the application of a large number
of such atomic operations.

One the other hand, those atomic operations cannot be implemented as methods
of the itk::QuadEdgeMesh class (or a derived one) at the risk of rapid code
bloat.

Making a thematic regrouping within derived classes was discarded because this
would force an end user to multiple inheritance which can prove to be a drag in
a templated context.

Eventually, QuadEdgeMesh algorithms were implemented as function object: the
loosely coupling of those operation methods with the targeted QuadEdgeMesh
object and heavier invocation syntax are a small price to pay in exchange for
optimal memory usage and end user modularity.

But itk::FunctionBase could not be inherited since its
itk::FunctionBase::Evaluate method promises to leave its argument (the mesh we
want to modify in our case) untouched.

Hence the itk::MeshFunctionBase class was created whose main difference with
itk::FunctionBase is that its itk::MeshFunctionBase::Evaluate method allows
to modify the considered mesh.

When considering a new QuadEdgeMesh method there are four possible \"slots\"
to implement it:
  - The QuadEdgeMesh method.
  - A derived class from itk::FunctionBase when the method leaves the mesh
  constant.
  - A derived class from itk::MeshFunctionBase when the method modifies the
  mesh (typically in the case of Euler operators).
  - As a classic ITKMesh filter.

The choice of the slot is a mere matter of trade-off and in order to keep
QuadEdgeMesh tiny and humanly readable key decision factors can be the
occurrence of the calls and the human level complexity of the code.

With those criteria in mind the following choices were made:
  - Really atomic, lightweight and general purpose methods like
  itk::QuadEdgeMesh::ComputeNumberOfPoints are left within the
  itk::QuadEdgeMesh mesh class.
  - Heavier methods and less often called like
  itk::QuadEdgeMeshBoundaryEdgesMeshFunction were implemented as derived
  classes of itk::FunctionBase.
  - Methods with the same weight (measured e.g. in number of lines of code) but
  that modify the considered mesh, like itk::BoundaryEdgesMeshFunction, were
  implemented as derived classes of itk::MeshFunctionBase. Still the mesh
  modifications are really limited and concern a couple edges.
  - More specialized methods, with a wider scope and that require a
  copy of the mesh should follow the classical ITK Filter pattern and inherit
  from itk::MeshToMeshFilter.")

itk_module(ITKQuadEdgeMesh
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKMesh
  COMPILE_DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)
