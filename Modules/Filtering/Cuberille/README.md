# Cuberille

In-tree ITK module providing the cuberille implicit-surface
extraction algorithm: given a binary or scalar volume and an
iso-value, produce a quad mesh approximating the iso-surface, with
optional vertex normal interpolation and project-to-iso-surface
refinement.

The flagship class is `itk::CuberilleImageToMeshFilter`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKCuberille**](https://github.com/InsightSoftwareConsortium/ITKCuberille)
on 2026-05-04 via the v4 ingestion pipeline. The upstream repository
will be archived read-only after this PR merges; it remains
reachable at the URL above for historical reference.

## References

- Herman G.T., Liu H.K. *Three-dimensional display of human organs from computed tomograms.* Computer Graphics and Image Processing. 1979.
- Mueller D. *Cuberille implicit surface polygonization for ITK.* The Insight Journal. 2010. <https://doi.org/10.54294/p6m37k>
