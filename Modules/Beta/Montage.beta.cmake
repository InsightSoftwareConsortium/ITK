# Provenance manifest for the inline beta module Modules/Beta/Montage/
#
# Parallels the (now-removed) Modules/Remote/Montage.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Dženan Zukić <dzenan.zukic@kitware.com>

itk_beta_module_manifest(
  Montage
  DESCRIPTION
    "Reconstruction of 3D volumetric dataset from a collection of 2D slices."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKMontage"
  UPSTREAM_SHA      "7a5c40ab980be32005e5897d700025edba650ce1"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
