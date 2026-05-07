# SubdivisionQuadEdgeMeshFilter

In-tree ITK module providing triangle- and edge-based subdivision
filters for `itk::QuadEdgeMesh`. The filters refine a coarse triangular
quad-edge mesh into a finer one using classical subdivision schemes
(Loop, Modified Butterfly, Square Three, Linear), with both cell-based
and edge-based variants and a conditional/criterion-driven dispatcher
for adaptive refinement.

The flagship classes are
`itk::SubdivisionQuadEdgeMeshFilter`,
`itk::TriangleCellSubdivisionQuadEdgeMeshFilter`,
`itk::TriangleEdgeCellSubdivisionQuadEdgeMeshFilter`,
`itk::ConditionalSubdivisionQuadEdgeMeshFilter`, and the concrete
`Loop`, `LinearTriangle`, `ModifiedButterfly`, and `SquareThree`
specializations.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/itkSubdivisionQuadEdgeMeshFilter**](https://github.com/InsightSoftwareConsortium/itkSubdivisionQuadEdgeMeshFilter)
on 2026-05-07 via the v4 ingestion pipeline. The upstream repository
will be archived read-only after this PR merges; it remains reachable
at the URL above for historical reference.

## References

- Zhu W. *Triangle Mesh Subdivision.* The Insight Journal. 2010.
  <https://doi.org/10.54294/vw0ke0>
