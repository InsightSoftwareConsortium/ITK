# Provenance manifest for the inline beta module Modules/Beta/SimpleITKFilters/
#
# Parallels the (now-removed) Modules/Remote/SimpleITKFilters.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Bradley Lowekamp <blowekamp@mail.nih.gov>

itk_beta_module_manifest(
  SimpleITKFilters
  DESCRIPTION
    "Filter implementations used by the SimpleITK project."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKSimpleITKFilters"
  UPSTREAM_SHA      "ae7a7d84be75a20494d8bc383061aa064a488a6d"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
