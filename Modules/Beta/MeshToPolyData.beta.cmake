# Provenance manifest for the inline beta module Modules/Beta/MeshToPolyData/
#
# Parallels the (now-removed) Modules/Remote/MeshToPolyData.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Matt McCormick <matt.mccormick@kitware.com>

itk_beta_module_manifest(
  MeshToPolyData
  DESCRIPTION
    "Convert between ITK Mesh and simple PolyData representation."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKMeshToPolyData"
  UPSTREAM_SHA      "216c951146f9f375ad189ab64d7dbe14dfa4cc38"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
