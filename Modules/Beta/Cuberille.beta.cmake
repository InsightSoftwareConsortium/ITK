# Provenance manifest for the inline beta module Modules/Beta/Cuberille/
#
# Parallels the (now-removed) Modules/Remote/Cuberille.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects. Consumed by tooling and human readers; kept current on each
# upstream resync.

# Contact: Matt McCormick <matt.mccormick@kitware.com>

itk_beta_module_manifest(
  Cuberille
  DESCRIPTION
    "Cuberille implicit surface polygonization for ITK. Divides the surface
     into small cubes (cuberilles) centered on iso-surface pixels; generates
     quadrilaterals for each face; projects vertices onto the implicit
     surface to smooth the block-like mesh.

     Insight Journal:
       Mueller, D. \"Cuberille Implicit Surface Polygonization for ITK\"
       https://doi.org/10.54294/df9mgw  (July-December, 2010)

     Related:
       Mueller, D. \"Fast Marching Minimal Path Extraction in ITK\"
       https://doi.org/10.54294/z5zwhh  (January-June, 2008)"

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKCuberille"
  UPSTREAM_SHA      "4ba447f256a93428e56507178f92124bb91e3f3c"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
