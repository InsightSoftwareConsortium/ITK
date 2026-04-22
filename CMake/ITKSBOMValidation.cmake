#[=============================================================================[
  ITKSBOMValidation.cmake - Validate the generated SPDX SBOM document

  Adds a CTest test that validates the SBOM JSON file produced by
  ITKSBOMGeneration.cmake. Checks:
    1. Valid JSON syntax
    2. Required SPDX 2.3 top-level fields present
    3. At least one package (ITK itself)
    4. DESCRIBES relationship present
#]=============================================================================]

if(NOT ITK_GENERATE_SBOM OR NOT BUILD_TESTING)
  return()
endif()

set(_sbom_file "${CMAKE_BINARY_DIR}/sbom.spdx.json")
if(NOT EXISTS "${_sbom_file}")
  return()
endif()

if(NOT Python3_EXECUTABLE)
  message(WARNING "Python3 not found; skipping SBOM validation tests.")
  return()
endif()

add_test(
  NAME ITKSBOMValidation
  COMMAND
    ${Python3_EXECUTABLE} "${ITK_SOURCE_DIR}/Utilities/SPDX/validate_light.py"
    "${_sbom_file}"
)
set_tests_properties(
  ITKSBOMValidation
  PROPERTIES
    LABELS
      "SBOM"
)

# Verify SPDX_VERSION entries match UpdateFromUpstream.sh tags.
# This catches the case where a vendored dependency is updated but
# the SPDX_VERSION in itk-module.cmake is not bumped.
add_test(
  NAME ITKSBOMVersionConsistency
  COMMAND
    ${Python3_EXECUTABLE} "${ITK_SOURCE_DIR}/Utilities/SPDX/verify_versions.py"
    "${ITK_SOURCE_DIR}"
)
set_tests_properties(
  ITKSBOMVersionConsistency
  PROPERTIES
    LABELS
      "SBOM"
)

# Full SPDX 2.3 schema validation using the official spdx-tools package.
# Catches schema-level issues (wrong field names, invalid license
# expressions, broken relationship references) that the lightweight
# in-tree validator cannot detect. Reported as skipped (CTest return
# code 77) when spdx-tools is not installed, so the test is optional
# rather than a hard CI dependency.
add_test(
  NAME ITKSBOMSchemaValidation
  COMMAND
    ${Python3_EXECUTABLE}
    "${ITK_SOURCE_DIR}/Utilities/SPDX/validate_with_spdx_tools.py"
    "${_sbom_file}"
)
set_tests_properties(
  ITKSBOMSchemaValidation
  PROPERTIES
    LABELS
      "SBOM"
    SKIP_RETURN_CODE
      77
)

# SBOM drift detection. Compares the current SBOM fingerprint (a SHA-256
# over the sorted package name / version / license / PURL tuples) against
# a baseline committed into the tree. The test is enabled only when
# ITK_SBOM_FINGERPRINT_BASELINE points to an existing file, because the
# fingerprint depends on which optional modules are enabled at configure
# time. Typical usage: maintainers generate a baseline for the canonical
# CI configuration, commit it, and CI flags drift when a PR silently
# changes dependency versions or licenses.
set(
  ITK_SBOM_FINGERPRINT_BASELINE
  ""
  CACHE FILEPATH
  "Path to a committed SBOM fingerprint baseline; enables drift-detection test"
)
if(ITK_SBOM_FINGERPRINT_BASELINE AND EXISTS "${ITK_SBOM_FINGERPRINT_BASELINE}")
  add_test(
    NAME ITKSBOMFingerprint
    COMMAND
      ${Python3_EXECUTABLE}
      "${ITK_SOURCE_DIR}/Utilities/SPDX/compute_fingerprint.py" "${_sbom_file}"
      "--compare" "${ITK_SBOM_FINGERPRINT_BASELINE}"
  )
  set_tests_properties(
    ITKSBOMFingerprint
    PROPERTIES
      LABELS
        "SBOM"
  )
endif()
