# Provenance manifest for the inline beta module Modules/Beta/BoneMorphometry/
#
# Parallels the (now-removed) Modules/Remote/BoneMorphometry.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Michael Kuczynski <mkuczyns@ucalgary.ca>

itk_beta_module_manifest(
  BoneMorphometry
  DESCRIPTION
    "Texture-based bone morphometry filters for ITK."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKBoneMorphometry"
  UPSTREAM_SHA      "a81057b078cf4b1b7ee8dcbefe7130894b45c8a7"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
