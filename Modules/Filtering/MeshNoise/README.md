# MeshNoise

In-tree ITK module providing additive-noise filters for `itk::Mesh`
and `itk::QuadEdgeMesh` point coordinates. The flagship class is
`itk::AdditiveGaussianNoiseMeshFilter`, which perturbs every mesh
point by an independent Gaussian-distributed offset with
configurable mean and standard deviation. Useful for testing
mesh-based registration, surface reconstruction, and shape-analysis
pipelines under controlled perturbation.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKMeshNoise**](https://github.com/InsightSoftwareConsortium/ITKMeshNoise)
on 2026-04-28. The upstream repository will be archived read-only
after this PR merges; it remains reachable at the URL above.

This ingest also resolves the long-open compile error reported in
[InsightSoftwareConsortium/ITK#5174](https://github.com/InsightSoftwareConsortium/ITK/issues/5174).
