# Provenance manifest for the inline beta module Modules/Beta/LabelErodeDilate/
#
# Parallels the (now-removed) Modules/Remote/LabelErodeDilate.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Richard Beare <richard.beare@monash.edu>

itk_beta_module_manifest(
  LabelErodeDilate
  DESCRIPTION
    "Morphological erosion/dilation operating directly on label images."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKLabelErodeDilate"
  UPSTREAM_SHA      "a9aeab73ae30d9143e4538812789e406de711bf5"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
