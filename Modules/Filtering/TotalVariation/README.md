# TotalVariation

In-tree ITK module providing fast total variation (TV) proximity
operators for image denoising, deconvolution, and related applications.

The flagship class is `itk::ProxTVImageFilter`, which wraps the proxTV
library for 2D and 3D images. proxTV itself is supplied by the
`ITKproxTV` third-party module (`Modules/ThirdParty/proxTV`), which
builds it at build time via `ExternalProject`. Both modules are
`EXCLUDE_FROM_DEFAULT`; enable `Module_TotalVariation` to build them.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKTotalVariation**](https://github.com/InsightSoftwareConsortium/ITKTotalVariation)
on 2026-06-02 via the v4 ingestion pipeline. The upstream repository
will be archived read-only after this PR merges; it remains reachable
at the URL above for historical reference.

## References

- Barbero A., Sra S. *Modular proximal optimization for multidimensional total-variation regularization.* Journal of Machine Learning Research. 2018.
- proxTV library: <https://github.com/albarji/proxTV>
