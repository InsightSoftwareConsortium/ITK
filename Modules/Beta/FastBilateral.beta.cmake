# Provenance manifest for the inline beta module Modules/Beta/FastBilateral/
#
# Parallels the (now-removed) Modules/Remote/FastBilateral.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Matt McCormick <matt.mccormick@kitware.com>

itk_beta_module_manifest(
  FastBilateral
  DESCRIPTION
    "Fast approximation to the bilateral filter for ITK."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKFastBilateral"
  UPSTREAM_SHA      "b8ac470ccd79dffdd7cef89ca2ae990b5f1ddaba"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
