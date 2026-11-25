# TwoProjectionRegistration

In-tree ITK module providing intensity-based 2D/3D rigid registration
for patient setup assessment in external-beam radiotherapy. The
framework simultaneously registers two projection (DRR) images to a 3D
image volume, simulating the x-ray imaging system attached to a medical
linear accelerator. Normalized correlation is the similarity measure,
Powell's method is the optimizer, and the Siddon-Jacobs fast ray-tracing
algorithm computes projection images from the volume.

The flagship class is `itk::TwoProjectionImageRegistrationMethod`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKTwoProjectionRegistration**](https://github.com/InsightSoftwareConsortium/ITKTwoProjectionRegistration)
on 2026-05-28 via the v4 ingestion pipeline. The upstream repository
will be archived read-only after this PR merges; it remains reachable
at the URL above for historical reference.

## References

- Wu J. *ITK-Based Implementation of Two-Projection 2D/3D Registration Method with an Application in Patient Setup for External Beam Radiotherapy.* The Insight Journal. 2010. <https://www.insight-journal.org/browse/publication/784>
