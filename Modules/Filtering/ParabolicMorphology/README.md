# ParabolicMorphology

In-tree ITK module providing fast mathematical morphology using
parabolic structuring functions. Implements distance transforms,
binary erosions, dilations, openings, closings by spheres, grayscale
operations, and sharpenings in linear time per pixel via
separable parabolic filtering.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKParabolicMorphology**](https://github.com/InsightSoftwareConsortium/ITKParabolicMorphology)
on 2026-05-12, following the v4 ingestion guidelines defined in
InsightSoftwareConsortium/ITK#6204. The upstream repository will be
archived read-only after this PR merges; it remains reachable at the
URL above for historical reference (notably the `doc/`, `examples/`,
and `oldtests/` directories, which were intentionally left in the
upstream archive).

## References

- Beare R. *A Locally Constrained Radial Sum Decomposition for Fast
  Spherical Mathematical Morphology.* International Symposium on
  Mathematical Morphology, 2007.
- Felzenszwalb P., Huttenlocher D. *Distance Transforms of Sampled
  Functions.* Theory of Computing 8(19), 415-428, 2012.
  <https://doi.org/10.4086/toc.2012.v008a019>
- Beare R., Jackway P. *Parabolic Morphology in ITK.* The Insight
  Journal. 2012. <https://doi.org/10.54294/aq68pt>
