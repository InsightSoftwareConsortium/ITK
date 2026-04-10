#[=============================================================================[
  ITKSBOMGeneration.cmake - Generate SPDX Software Bill of Materials (SBOM)

  This module generates an SPDX 2.3 JSON SBOM document describing ITK and
  its enabled third-party dependencies at build configuration time. The SBOM
  includes component names, versions, licenses, and dependency relationships.

  Per-module SPDX metadata is declared in each module's itk-module.cmake via
  the itk_module() macro arguments:
    SPDX_LICENSE             - SPDX license identifier (e.g. "Apache-2.0")
    SPDX_DOWNLOAD_LOCATION   - URL for the upstream source
    SPDX_COPYRIGHT           - Copyright text
    SPDX_CUSTOM_LICENSE_TEXT  - Extracted text for custom LicenseRef-* IDs
    SPDX_CUSTOM_LICENSE_NAME  - Human-readable name for custom license refs

  Usage:
    option(ITK_GENERATE_SBOM "Generate SPDX SBOM at configure time" ON)
    include(ITKSBOMGeneration)

  The generated file is written to:
    ${CMAKE_BINARY_DIR}/sbom.spdx.json
#]=============================================================================]

if(NOT ITK_GENERATE_SBOM)
  return()
endif()

#-----------------------------------------------------------------------------
# Allow remote modules to register SBOM package metadata.
#
# Usage in a remote module's CMakeLists.txt:
#   itk_sbom_register_package(
#     NAME "MyRemoteModule"
#     VERSION "1.0.0"
#     SPDX_LICENSE "Apache-2.0"
#     DOWNLOAD_LOCATION "https://github.com/example/MyRemoteModule"
#     SUPPLIER "Organization: Example"
#     COPYRIGHT "Copyright Example Inc."
#   )
#
define_property(GLOBAL PROPERTY ITK_SBOM_EXTRA_PACKAGES
  BRIEF_DOCS "Additional SBOM package entries registered by remote modules."
  FULL_DOCS "A list of JSON-formatted package entries for the SBOM."
)

function(itk_sbom_register_package)
  set(_options "")
  set(_one_value NAME VERSION SPDX_LICENSE DOWNLOAD_LOCATION SUPPLIER COPYRIGHT)
  set(_multi_value "")
  cmake_parse_arguments(_pkg "${_options}" "${_one_value}" "${_multi_value}" ${ARGN})

  if(NOT _pkg_NAME)
    message(FATAL_ERROR "itk_sbom_register_package: NAME is required.")
  endif()
  if(NOT _pkg_SPDX_LICENSE)
    set(_pkg_SPDX_LICENSE "NOASSERTION")
  endif()
  if(NOT _pkg_VERSION)
    set(_pkg_VERSION "NOASSERTION")
  endif()
  if(NOT _pkg_DOWNLOAD_LOCATION)
    set(_pkg_DOWNLOAD_LOCATION "NOASSERTION")
  endif()
  if(NOT _pkg_SUPPLIER)
    set(_pkg_SUPPLIER "NOASSERTION")
  endif()
  if(NOT _pkg_COPYRIGHT)
    set(_pkg_COPYRIGHT "NOASSERTION")
  endif()

  # Sanitize the name for use as SPDX ID (only alphanumeric and -)
  string(REGEX REPLACE "[^A-Za-z0-9-]" "-" _spdx_id "${_pkg_NAME}")

  set(_entry "")
  string(APPEND _entry "    {\n")
  string(APPEND _entry "      \"SPDXID\": \"SPDXRef-${_spdx_id}\",\n")
  string(APPEND _entry "      \"name\": \"${_pkg_NAME}\",\n")
  string(APPEND _entry "      \"versionInfo\": \"${_pkg_VERSION}\",\n")
  string(APPEND _entry "      \"downloadLocation\": \"${_pkg_DOWNLOAD_LOCATION}\",\n")
  string(APPEND _entry "      \"supplier\": \"${_pkg_SUPPLIER}\",\n")
  string(APPEND _entry "      \"licenseConcluded\": \"${_pkg_SPDX_LICENSE}\",\n")
  string(APPEND _entry "      \"licenseDeclared\": \"${_pkg_SPDX_LICENSE}\",\n")
  string(APPEND _entry "      \"copyrightText\": \"${_pkg_COPYRIGHT}\",\n")
  string(APPEND _entry "      \"filesAnalyzed\": false\n")
  string(APPEND _entry "    }")

  set_property(GLOBAL APPEND PROPERTY ITK_SBOM_EXTRA_PACKAGES "${_entry}")

  # Also store the SPDX ID for relationship generation
  set_property(GLOBAL APPEND PROPERTY ITK_SBOM_EXTRA_SPDX_IDS "SPDXRef-${_spdx_id}")
endfunction()

#-----------------------------------------------------------------------------
# Internal: Escape a string for use in a JSON value.
# Handles backslashes, double quotes, and newlines.
#
function(_itk_sbom_json_escape input_string output_var)
  set(_str "${input_string}")
  # Escape backslashes first
  string(REPLACE "\\" "\\\\" _str "${_str}")
  # Escape double quotes
  string(REPLACE "\"" "\\\"" _str "${_str}")
  # Escape newlines (CMake uses \n for semicolons and actual newlines)
  string(REPLACE "\n" "\\n" _str "${_str}")
  # Escape tabs
  string(REPLACE "\t" "\\t" _str "${_str}")
  set(${output_var} "${_str}" PARENT_SCOPE)
endfunction()

#-----------------------------------------------------------------------------
# Main function: Generate the SPDX 2.3 SBOM JSON document.
#
function(itk_generate_sbom)
  string(TIMESTAMP _sbom_timestamp "%Y-%m-%dT%H:%M:%SZ" UTC)
  string(TIMESTAMP _sbom_uid "%Y%m%d%H%M%S" UTC)

  set(_sbom_namespace
    "https://spdx.org/spdxdocs/ITK-${ITK_VERSION}-${_sbom_uid}")

  # --- Begin JSON document ---
  set(_json "")
  string(APPEND _json "{\n")
  string(APPEND _json "  \"spdxVersion\": \"SPDX-2.3\",\n")
  string(APPEND _json "  \"dataLicense\": \"CC0-1.0\",\n")
  string(APPEND _json "  \"SPDXID\": \"SPDXRef-DOCUMENT\",\n")
  string(APPEND _json "  \"name\": \"ITK-${ITK_VERSION}-SBOM\",\n")
  string(APPEND _json "  \"documentNamespace\": \"${_sbom_namespace}\",\n")

  # --- creationInfo ---
  string(APPEND _json "  \"creationInfo\": {\n")
  string(APPEND _json "    \"created\": \"${_sbom_timestamp}\",\n")
  string(APPEND _json "    \"creators\": [\n")
  string(APPEND _json "      \"Tool: CMake-${CMAKE_VERSION}\",\n")
  string(APPEND _json "      \"Organization: NumFOCUS\"\n")
  string(APPEND _json "    ],\n")
  string(APPEND _json "    \"licenseListVersion\": \"3.22\"\n")
  string(APPEND _json "  },\n")

  # --- packages array ---
  string(APPEND _json "  \"packages\": [\n")

  # ITK main package
  string(APPEND _json "    {\n")
  string(APPEND _json "      \"SPDXID\": \"SPDXRef-ITK\",\n")
  string(APPEND _json "      \"name\": \"ITK\",\n")
  string(APPEND _json "      \"versionInfo\": \"${ITK_VERSION}\",\n")
  string(APPEND _json "      \"downloadLocation\": \"https://github.com/InsightSoftwareConsortium/ITK\",\n")
  string(APPEND _json "      \"supplier\": \"Organization: NumFOCUS\",\n")
  string(APPEND _json "      \"licenseConcluded\": \"Apache-2.0\",\n")
  string(APPEND _json "      \"licenseDeclared\": \"Apache-2.0\",\n")
  string(APPEND _json "      \"copyrightText\": \"Copyright 1999-2019 Insight Software Consortium, Copyright 2020-present NumFOCUS\",\n")
  string(APPEND _json "      \"filesAnalyzed\": false\n")
  string(APPEND _json "    }")

  # Collect ThirdParty modules from enabled modules list
  set(_thirdparty_spdx_ids "")
  # Track custom license refs for hasExtractedLicensingInfo
  set(_custom_license_ids "")
  set(_custom_license_names "")
  set(_custom_license_texts "")
  foreach(_mod ${ITK_MODULES_ENABLED})
    if(${_mod}_IS_TEST)
      continue()
    endif()

    # Only include modules that have SPDX metadata declared
    set(_pkg_license "${ITK_MODULE_${_mod}_SPDX_LICENSE}")
    if(NOT _pkg_license)
      continue()
    endif()

    set(_pkg_download "${ITK_MODULE_${_mod}_SPDX_DOWNLOAD_LOCATION}")
    set(_pkg_copyright "${ITK_MODULE_${_mod}_SPDX_COPYRIGHT}")
    if(NOT _pkg_download)
      set(_pkg_download "NOASSERTION")
    endif()
    if(NOT _pkg_copyright)
      set(_pkg_copyright "NOASSERTION")
    endif()

    # Get description from module declaration and escape for JSON
    set(_pkg_description "${ITK_MODULE_${_mod}_DESCRIPTION}")
    if(_pkg_description)
      _itk_sbom_json_escape("${_pkg_description}" _pkg_description)
    endif()

    # Collect custom license references
    set(_custom_text "${ITK_MODULE_${_mod}_SPDX_CUSTOM_LICENSE_TEXT}")
    set(_custom_name "${ITK_MODULE_${_mod}_SPDX_CUSTOM_LICENSE_NAME}")
    if(_custom_text AND _custom_name)
      list(APPEND _custom_license_ids "${_pkg_license}")
      list(APPEND _custom_license_names "${_custom_name}")
      list(APPEND _custom_license_texts "${_custom_text}")
    endif()

    # Sanitize module name for SPDX ID
    string(REGEX REPLACE "[^A-Za-z0-9-]" "-" _spdx_id "${_mod}")
    list(APPEND _thirdparty_spdx_ids "SPDXRef-${_spdx_id}")

    string(APPEND _json ",\n")
    string(APPEND _json "    {\n")
    string(APPEND _json "      \"SPDXID\": \"SPDXRef-${_spdx_id}\",\n")
    string(APPEND _json "      \"name\": \"${_mod}\",\n")
    string(APPEND _json "      \"versionInfo\": \"NOASSERTION\",\n")
    string(APPEND _json "      \"downloadLocation\": \"${_pkg_download}\",\n")
    string(APPEND _json "      \"supplier\": \"NOASSERTION\",\n")
    string(APPEND _json "      \"licenseConcluded\": \"${_pkg_license}\",\n")
    string(APPEND _json "      \"licenseDeclared\": \"${_pkg_license}\",\n")
    string(APPEND _json "      \"copyrightText\": \"${_pkg_copyright}\",\n")
    if(_pkg_description)
      string(APPEND _json "      \"description\": \"${_pkg_description}\",\n")
    endif()
    string(APPEND _json "      \"filesAnalyzed\": false\n")
    string(APPEND _json "    }")
  endforeach()

  # FFTW (not an ITK module, but an optional external dependency)
  if(ITK_USE_FFTWD OR ITK_USE_FFTWF)
    set(_fftw_license "GPL-2.0-or-later")
    set(_fftw_version "NOASSERTION")
    if(DEFINED _fftw_target_version)
      set(_fftw_version "${_fftw_target_version}")
    elseif(DEFINED FFTW_VERSION)
      set(_fftw_version "${FFTW_VERSION}")
    endif()

    string(APPEND _json ",\n")
    string(APPEND _json "    {\n")
    string(APPEND _json "      \"SPDXID\": \"SPDXRef-FFTW\",\n")
    string(APPEND _json "      \"name\": \"FFTW\",\n")
    string(APPEND _json "      \"versionInfo\": \"${_fftw_version}\",\n")
    string(APPEND _json "      \"downloadLocation\": \"https://www.fftw.org\",\n")
    string(APPEND _json "      \"supplier\": \"Organization: MIT\",\n")
    string(APPEND _json "      \"licenseConcluded\": \"${_fftw_license}\",\n")
    string(APPEND _json "      \"licenseDeclared\": \"${_fftw_license}\",\n")
    string(APPEND _json "      \"copyrightText\": \"Copyright Matteo Frigo and Massachusetts Institute of Technology\",\n")
    string(APPEND _json "      \"description\": \"Fastest Fourier Transform in the West\",\n")
    string(APPEND _json "      \"filesAnalyzed\": false\n")
    string(APPEND _json "    }")
    list(APPEND _thirdparty_spdx_ids "SPDXRef-FFTW")
  endif()

  # Append extra packages registered by remote modules
  get_property(_extra_packages GLOBAL PROPERTY ITK_SBOM_EXTRA_PACKAGES)
  foreach(_extra_pkg ${_extra_packages})
    string(APPEND _json ",\n${_extra_pkg}")
  endforeach()

  string(APPEND _json "\n  ],\n")

  # --- relationships array ---
  string(APPEND _json "  \"relationships\": [\n")

  # DOCUMENT describes ITK
  string(APPEND _json "    {\n")
  string(APPEND _json "      \"spdxElementId\": \"SPDXRef-DOCUMENT\",\n")
  string(APPEND _json "      \"relationshipType\": \"DESCRIBES\",\n")
  string(APPEND _json "      \"relatedSpdxElement\": \"SPDXRef-ITK\"\n")
  string(APPEND _json "    }")

  # ITK DEPENDS_ON each ThirdParty module
  foreach(_spdx_id ${_thirdparty_spdx_ids})
    string(APPEND _json ",\n")
    string(APPEND _json "    {\n")
    string(APPEND _json "      \"spdxElementId\": \"SPDXRef-ITK\",\n")
    string(APPEND _json "      \"relationshipType\": \"DEPENDS_ON\",\n")
    string(APPEND _json "      \"relatedSpdxElement\": \"${_spdx_id}\"\n")
    string(APPEND _json "    }")
  endforeach()

  # Extra packages registered by remote modules
  get_property(_extra_spdx_ids GLOBAL PROPERTY ITK_SBOM_EXTRA_SPDX_IDS)
  foreach(_spdx_id ${_extra_spdx_ids})
    string(APPEND _json ",\n")
    string(APPEND _json "    {\n")
    string(APPEND _json "      \"spdxElementId\": \"SPDXRef-ITK\",\n")
    string(APPEND _json "      \"relationshipType\": \"DEPENDS_ON\",\n")
    string(APPEND _json "      \"relatedSpdxElement\": \"${_spdx_id}\"\n")
    string(APPEND _json "    }")
  endforeach()

  string(APPEND _json "\n  ]")

  # --- hasExtractedLicensingInfo for custom LicenseRef identifiers ---
  list(LENGTH _custom_license_ids _num_custom)
  if(_num_custom GREATER 0)
    string(APPEND _json ",\n")
    string(APPEND _json "  \"hasExtractedLicensingInfo\": [\n")
    set(_first_custom TRUE)
    math(EXPR _last_idx "${_num_custom} - 1")
    foreach(_idx RANGE ${_last_idx})
      list(GET _custom_license_ids ${_idx} _lic_id)
      list(GET _custom_license_names ${_idx} _lic_name)
      list(GET _custom_license_texts ${_idx} _lic_text)
      if(NOT _first_custom)
        string(APPEND _json ",\n")
      endif()
      set(_first_custom FALSE)
      string(APPEND _json "    {\n")
      string(APPEND _json "      \"licenseId\": \"${_lic_id}\",\n")
      string(APPEND _json "      \"name\": \"${_lic_name}\",\n")
      string(APPEND _json "      \"extractedText\": \"${_lic_text}\"\n")
      string(APPEND _json "    }")
    endforeach()
    string(APPEND _json "\n  ]")
  endif()

  # --- Close JSON document ---
  string(APPEND _json "\n}\n")

  # Write SBOM to build directory
  set(_sbom_file "${CMAKE_BINARY_DIR}/sbom.spdx.json")
  file(WRITE "${_sbom_file}" "${_json}")
  message(STATUS "SBOM generated: ${_sbom_file}")
endfunction()
