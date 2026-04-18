# Provenance manifest for the inline beta module Modules/Beta/AnalyzeObjectLabelMap/
#
# Parallels the (now-removed) Modules/Remote/AnalyzeObjectLabelMap.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Hans Johnson <hans-johnson@uiowa.edu>

itk_beta_module_manifest(
  AnalyzeObjectLabelMap
  DESCRIPTION
    "Reader/writer for the Analyze Object Label Map format."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKAnalyzeObjectLabelMap"
  UPSTREAM_SHA      "7b3eb672e3dbd39b47926498a73a9ca0ef5fb3ca"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
