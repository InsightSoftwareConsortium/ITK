#[=============================================================================[
  ITKSBOMGeneration.cmake - Generate SPDX Software Bill of Materials (SBOM)

  Collects per-module SPDX metadata at configure time, writes a simple
  line-based manifest (${CMAKE_BINARY_DIR}/sbom-inputs.manifest), and
  invokes Utilities/SPDX/generate_sbom.py to emit the final SPDX 2.3
  JSON document at ${CMAKE_BINARY_DIR}/sbom.spdx.json.

  Rationale: JSON construction, SPDX-field wiring, LicenseRef
  validation, and relationship assembly are handled in Python. CMake
  is responsible only for enumerating enabled modules and flattening
  their SPDX_* properties into key=value records.

  Per-module SPDX metadata is declared in each module's itk-module.cmake
  via the itk_module() macro arguments:
    SPDX_LICENSE             - SPDX license identifier (e.g. "Apache-2.0")
    SPDX_VERSION             - Version of the vendored dependency
    SPDX_DOWNLOAD_LOCATION   - URL for the upstream source
    SPDX_COPYRIGHT           - Copyright text
    SPDX_CUSTOM_LICENSE_TEXT  - Extracted text for custom LicenseRef-* IDs
    SPDX_CUSTOM_LICENSE_NAME  - Human-readable name for custom license refs
    SPDX_PURL                - Package URL (e.g. "pkg:generic/libpng@1.6.54")
                                used to map the component to CVE feeds via
                                Trivy / Grype / OSV-Scanner

  Usage:
    option(ITK_GENERATE_SBOM "Generate SPDX SBOM at configure time" ON)
    include(ITKSBOMGeneration)
#]=============================================================================]

if(NOT ITK_GENERATE_SBOM)
  return()
endif()

# Capture the repo-root-relative path to the Python generator at include
# time; CMAKE_CURRENT_LIST_DIR inside the function body would resolve to
# the caller's directory.
set(
  _ITK_SBOM_GENERATOR
  "${CMAKE_CURRENT_LIST_DIR}/../Utilities/SPDX/generate_sbom.py"
)

# Python 3 is a hard requirement when SBOM generation is enabled. The
# Python generator replaces the previous hand-written CMake JSON
# emitter; there is no fallback path.
find_package(Python3 COMPONENTS Interpreter QUIET)
if(NOT Python3_EXECUTABLE)
  message(
    FATAL_ERROR
    "ITK_GENERATE_SBOM=ON requires a Python 3 interpreter. "
    "Install Python 3.10+ or set ITK_GENERATE_SBOM=OFF."
  )
endif()

set(
  ITK_SBOM_SPDX_LICENSE_LIST_VERSION
  "3.28"
  CACHE STRING
  "SPDX license list version recorded in the generated SBOM"
)

#-----------------------------------------------------------------------------
# Internal: escape a manifest value so newline / CR / backslash round-trip
# through the Python reader. Matches generate_sbom.py:_unescape().
function(_itk_sbom_manifest_escape input_string output_var)
  set(_s "${input_string}")
  string(REPLACE "\\" "\\\\" _s "${_s}")
  string(REPLACE "\n" "\\n" _s "${_s}")
  string(REPLACE "\r" "\\r" _s "${_s}")
  set(${output_var} "${_s}" PARENT_SCOPE)
endfunction()

# Internal: append "key=<escaped value>\n" to ${var_name} when value is
# non-empty. Avoids emitting useless empty-string fields into the manifest.
macro(_itk_sbom_manifest_kv var_name key value)
  if(NOT "${value}" STREQUAL "")
    _itk_sbom_manifest_escape("${value}" _escaped_value)
    string(APPEND ${var_name} "${key}=${_escaped_value}\n")
  endif()
endmacro()

#-----------------------------------------------------------------------------
# Public: register extra SBOM packages from remote modules.
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
define_property(
  GLOBAL
  PROPERTY ITK_SBOM_EXTRA_PACKAGES
  BRIEF_DOCS
    "Manifest blocks for SBOM packages registered by remote modules."
  FULL_DOCS
    "A list of package.begin/package.end blocks appended to the SBOM "
    "manifest before generate_sbom.py runs."
)

function(itk_sbom_register_package)
  set(_options "")
  set(
    _one_value
    NAME
    VERSION
    SPDX_LICENSE
    DOWNLOAD_LOCATION
    SUPPLIER
    ORIGINATOR
    COPYRIGHT
    PURL
    PRIMARY_PACKAGE_PURPOSE
  )
  cmake_parse_arguments(_pkg "${_options}" "${_one_value}" "" ${ARGN})

  if(NOT _pkg_NAME)
    message(FATAL_ERROR "itk_sbom_register_package: NAME is required.")
  endif()

  string(REGEX REPLACE "[^A-Za-z0-9-]" "-" _spdx_id "${_pkg_NAME}")

  set(_block "")
  string(APPEND _block "package.begin\n")
  _itk_sbom_manifest_kv(_block "spdx_id" "SPDXRef-${_spdx_id}")
  _itk_sbom_manifest_kv(_block "name" "${_pkg_NAME}")
  _itk_sbom_manifest_kv(_block "version" "${_pkg_VERSION}")
  _itk_sbom_manifest_kv(_block "download_location" "${_pkg_DOWNLOAD_LOCATION}")
  _itk_sbom_manifest_kv(_block "supplier" "${_pkg_SUPPLIER}")
  _itk_sbom_manifest_kv(_block "originator" "${_pkg_ORIGINATOR}")
  _itk_sbom_manifest_kv(_block "license_concluded" "${_pkg_SPDX_LICENSE}")
  _itk_sbom_manifest_kv(_block "license_declared" "${_pkg_SPDX_LICENSE}")
  _itk_sbom_manifest_kv(_block "copyright" "${_pkg_COPYRIGHT}")
  _itk_sbom_manifest_kv(_block "purl" "${_pkg_PURL}")
  _itk_sbom_manifest_kv(
    _block
    "primary_package_purpose"
    "${_pkg_PRIMARY_PACKAGE_PURPOSE}"
  )
  string(APPEND _block "package.end\n")

  set_property(
    GLOBAL
    APPEND_STRING
    PROPERTY
      ITK_SBOM_EXTRA_PACKAGES
        "${_block}"
  )
endfunction()

#-----------------------------------------------------------------------------
# Main entry point: write the manifest and invoke generate_sbom.py.
function(itk_generate_sbom)
  string(TIMESTAMP _sbom_timestamp "%Y-%m-%dT%H:%M:%SZ" UTC)

  # UUIDv5 seeded by ITK version + timestamp so parallel multi-config
  # reconfigures within the same second still produce distinct document
  # namespaces, as required by SPDX 2.3 §6.5.
  string(TIMESTAMP _sbom_uid "%Y%m%d%H%M%S" UTC)
  string(
    UUID
    _sbom_uuid
    NAMESPACE "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
    NAME "ITK-${ITK_VERSION}-${_sbom_uid}"
    TYPE SHA1
  )
  set(_sbom_namespace "https://itk.org/spdx/ITK-${ITK_VERSION}-${_sbom_uuid}")

  # --- Document fields ---
  set(_manifest "")
  string(APPEND _manifest "document.name=ITK-${ITK_VERSION}-SBOM\n")
  string(APPEND _manifest "document.namespace=${_sbom_namespace}\n")
  string(APPEND _manifest "document.timestamp=${_sbom_timestamp}\n")
  string(APPEND _manifest "document.cmake_version=${CMAKE_VERSION}\n")
  string(
    APPEND
    _manifest
    "document.spdx_license_list_version=${ITK_SBOM_SPDX_LICENSE_LIST_VERSION}\n"
  )

  # --- ITK main package (always first) ---
  string(APPEND _manifest "package.begin\n")
  _itk_sbom_manifest_kv(_manifest "spdx_id" "SPDXRef-ITK")
  _itk_sbom_manifest_kv(_manifest "name" "ITK")
  _itk_sbom_manifest_kv(_manifest "version" "${ITK_VERSION}")
  _itk_sbom_manifest_kv(
    _manifest
    "download_location"
    "https://github.com/InsightSoftwareConsortium/ITK"
  )
  _itk_sbom_manifest_kv(_manifest "supplier" "Organization: NumFOCUS")
  _itk_sbom_manifest_kv(_manifest "originator" "Organization: NumFOCUS")
  _itk_sbom_manifest_kv(_manifest "license_concluded" "Apache-2.0")
  _itk_sbom_manifest_kv(_manifest "license_declared" "Apache-2.0")
  _itk_sbom_manifest_kv(
    _manifest
    "copyright"
    "Copyright 1999-2019 Insight Software Consortium, Copyright 2020-present NumFOCUS"
  )
  _itk_sbom_manifest_kv(_manifest "primary_package_purpose" "LIBRARY")
  _itk_sbom_manifest_kv(
    _manifest
    "purl"
    "pkg:github/InsightSoftwareConsortium/ITK@v${ITK_VERSION}"
  )
  string(APPEND _manifest "package.end\n")

  # --- Enabled modules (ThirdParty and otherwise) ---
  foreach(_mod ${ITK_MODULES_ENABLED})
    if(${_mod}_IS_TEST)
      continue()
    endif()
    if(ITK_MODULE_${_mod}_SPDX_OPT_OUT)
      continue()
    endif()

    # ITK_MODULE_*_IS_THIRD_PARTY set inside itk_module() runs under the
    # top-level CMAKE_CURRENT_SOURCE_DIR (see itk_module_load_dag) so its
    # path-based detection is not reliable here. Use the per-module _BASE
    # relative path recorded by the scanner instead.
    set(_is_third_party 0)
    if("${${_mod}_BASE}" MATCHES "Modules/ThirdParty/")
      set(_is_third_party 1)
    endif()

    set(_pkg_license "${ITK_MODULE_${_mod}_SPDX_LICENSE}")
    if(NOT _pkg_license)
      if(_is_third_party)
        message(
          AUTHOR_WARNING
          "ThirdParty module ${_mod} has no SPDX_LICENSE in itk-module.cmake. "
          "Please add SPDX metadata for SBOM compliance, or declare "
          "SPDX_OPT_OUT in itk_module() if the module is not shipped."
        )
      endif()
      continue()
    endif()

    string(REGEX REPLACE "[^A-Za-z0-9-]" "-" _spdx_id "${_mod}")

    # Third-party modules: supplier is NumFOCUS (ships the vendored copy);
    # originator is the upstream author (unknown in CMake, so NOASSERTION).
    # ITK-owned modules: NumFOCUS on both sides.
    if(_is_third_party)
      set(_pkg_supplier "Organization: NumFOCUS")
      set(_pkg_originator "NOASSERTION")
    else()
      set(_pkg_supplier "Organization: NumFOCUS")
      set(_pkg_originator "Organization: NumFOCUS")
    endif()

    string(APPEND _manifest "package.begin\n")
    _itk_sbom_manifest_kv(_manifest "spdx_id" "SPDXRef-${_spdx_id}")
    _itk_sbom_manifest_kv(_manifest "name" "${_mod}")
    _itk_sbom_manifest_kv(
      _manifest
      "version"
      "${ITK_MODULE_${_mod}_SPDX_VERSION}"
    )
    _itk_sbom_manifest_kv(
      _manifest
      "download_location"
      "${ITK_MODULE_${_mod}_SPDX_DOWNLOAD_LOCATION}"
    )
    _itk_sbom_manifest_kv(_manifest "supplier" "${_pkg_supplier}")
    _itk_sbom_manifest_kv(_manifest "originator" "${_pkg_originator}")
    _itk_sbom_manifest_kv(_manifest "license_concluded" "${_pkg_license}")
    _itk_sbom_manifest_kv(_manifest "license_declared" "${_pkg_license}")
    _itk_sbom_manifest_kv(
      _manifest
      "copyright"
      "${ITK_MODULE_${_mod}_SPDX_COPYRIGHT}"
    )
    _itk_sbom_manifest_kv(
      _manifest
      "description"
      "${ITK_MODULE_${_mod}_DESCRIPTION}"
    )
    _itk_sbom_manifest_kv(_manifest "primary_package_purpose" "LIBRARY")
    _itk_sbom_manifest_kv(_manifest "purl" "${ITK_MODULE_${_mod}_SPDX_PURL}")
    string(APPEND _manifest "package.end\n")

    # Extracted license text for any LicenseRef-* identifier.
    set(_ctext "${ITK_MODULE_${_mod}_SPDX_CUSTOM_LICENSE_TEXT}")
    set(_cname "${ITK_MODULE_${_mod}_SPDX_CUSTOM_LICENSE_NAME}")
    if(_ctext AND _cname)
      string(APPEND _manifest "extracted_license.begin\n")
      _itk_sbom_manifest_kv(_manifest "license_id" "${_pkg_license}")
      _itk_sbom_manifest_kv(_manifest "name" "${_cname}")
      _itk_sbom_manifest_kv(_manifest "text" "${_ctext}")
      string(APPEND _manifest "extracted_license.end\n")
    endif()
  endforeach()

  # --- FFTW (optional external, not an ITK module) ---
  if(ITK_USE_FFTWD OR ITK_USE_FFTWF)
    set(_fftw_version "NOASSERTION")
    if(DEFINED _fftw_target_version)
      set(_fftw_version "${_fftw_target_version}")
    elseif(DEFINED FFTW_VERSION)
      set(_fftw_version "${FFTW_VERSION}")
    endif()
    string(APPEND _manifest "package.begin\n")
    _itk_sbom_manifest_kv(_manifest "spdx_id" "SPDXRef-FFTW")
    _itk_sbom_manifest_kv(_manifest "name" "FFTW")
    _itk_sbom_manifest_kv(_manifest "version" "${_fftw_version}")
    _itk_sbom_manifest_kv(_manifest "download_location" "https://www.fftw.org")
    _itk_sbom_manifest_kv(_manifest "supplier" "Organization: NumFOCUS")
    _itk_sbom_manifest_kv(_manifest "originator" "Organization: MIT")
    _itk_sbom_manifest_kv(_manifest "license_concluded" "GPL-2.0-or-later")
    _itk_sbom_manifest_kv(_manifest "license_declared" "GPL-2.0-or-later")
    _itk_sbom_manifest_kv(
      _manifest
      "copyright"
      "Copyright Matteo Frigo and Massachusetts Institute of Technology"
    )
    _itk_sbom_manifest_kv(
      _manifest
      "description"
      "Fastest Fourier Transform in the West"
    )
    _itk_sbom_manifest_kv(_manifest "primary_package_purpose" "LIBRARY")
    _itk_sbom_manifest_kv(_manifest "purl" "pkg:generic/fftw@${_fftw_version}")
    string(APPEND _manifest "package.end\n")
  endif()

  # --- Remote-module extras registered via itk_sbom_register_package() ---
  get_property(_extras GLOBAL PROPERTY ITK_SBOM_EXTRA_PACKAGES)
  if(_extras)
    string(APPEND _manifest "${_extras}")
  endif()

  # Write manifest and invoke the Python generator.
  set(_manifest_file "${CMAKE_BINARY_DIR}/sbom-inputs.manifest")
  set(_sbom_file "${CMAKE_BINARY_DIR}/sbom.spdx.json")
  file(WRITE "${_manifest_file}" "${_manifest}")

  execute_process(
    COMMAND
      "${Python3_EXECUTABLE}" "${_ITK_SBOM_GENERATOR}" "${_manifest_file}"
      "${_sbom_file}"
    RESULT_VARIABLE _gen_rc
    OUTPUT_VARIABLE _gen_out
    ERROR_VARIABLE _gen_err
  )
  if(NOT _gen_rc EQUAL 0)
    message(
      FATAL_ERROR
      "generate_sbom.py failed (${_gen_rc}):\n${_gen_out}${_gen_err}"
    )
  endif()
  message(STATUS "SBOM generated: ${_sbom_file}")
endfunction()
