# Provenance manifest for the inline beta module Modules/Beta/GrowCut/
#
# Parallels the (now-removed) Modules/Remote/GrowCut.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Dženan Zukić <dzenan.zukic@kitware.com>

itk_beta_module_manifest(
  GrowCut
  DESCRIPTION
    "GrowCut segmentation algorithm for ITK."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKGrowCut"
  UPSTREAM_SHA      "ca5288e6fbae77a9542ea615ec075f2eb0304a6b"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
