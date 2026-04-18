# Provenance manifest for the inline beta module Modules/Beta/BoneEnhancement/
#
# Parallels the (now-removed) Modules/Remote/BoneEnhancement.remote.cmake in
# intent, but is a passive metadata record -- no configure-time side
# effects.

# Contact: Bryce Besler <bryce.besler@gmail.com>

itk_beta_module_manifest(
  BoneEnhancement
  DESCRIPTION
    "Multi-scale bone-enhancement filters for ITK."

  UPSTREAM_URL      "https://github.com/InsightSoftwareConsortium/ITKBoneEnhancement"
  UPSTREAM_SHA      "9d762aaccad5cbbf97c4858ef809ebad52d76625"
  UPSTREAM_BRANCH   "main"
  INGEST_DATE       "2026-04-18"

  MODULE_COMPLIANCE_LEVEL 3
  ORIGINAL_LICENSE        "Apache-2.0"
)
