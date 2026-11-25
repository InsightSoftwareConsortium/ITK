# PrincipalComponentsAnalysis

In-tree ITK module providing a principal component analysis filter for
scalar, vector, and mesh-vertex data. The flagship class is
`itk::VectorFieldPCA`, which computes the principal components of an
ensemble of vector fields defined on a common mesh, useful for
shape-modeling and multivariate statistics workflows.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKPrincipalComponentsAnalysis**](https://github.com/InsightSoftwareConsortium/ITKPrincipalComponentsAnalysis)
on 2026-05-08, following the v4 ingestion guidelines defined in
InsightSoftwareConsortium/ITK#6204. The upstream repository will be
archived read-only after this PR merges; it remains reachable at the
URL above for historical reference (notably the `doc/` and
`examples/` directories, which were intentionally left in the
upstream archive).

## References

- Bowers M., Younes L. *Principal Components Analysis of Scalar,
  Vector, and Mesh Vertex Data.* The Insight Journal. August, 2013.
  <https://doi.org/10.54294/loekqj>
