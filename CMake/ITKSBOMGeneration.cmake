#[=============================================================================[
  ITKSBOMGeneration.cmake - Generate SPDX Software Bill of Materials (SBOM)

  This module generates an SPDX 2.3 JSON SBOM document describing ITK and
  its enabled third-party dependencies at build configuration time. The SBOM
  includes component names, versions, licenses, and dependency relationships.

  Usage:
    option(ITK_GENERATE_SBOM "Generate SPDX SBOM at build time" ON)
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
# Internal: Define SBOM metadata for a known ThirdParty module.
# Sets variables in the caller's scope:
#   _spdx_license, _download_location, _supplier, _copyright, _version
#
function(_itk_sbom_get_thirdparty_metadata module_name
  out_license out_download out_supplier out_copyright out_version)

  # Defaults
  set(_license "NOASSERTION")
  set(_download "NOASSERTION")
  set(_supplier "NOASSERTION")
  set(_copyright "NOASSERTION")
  set(_version "NOASSERTION")

  if("${module_name}" STREQUAL "ITKDCMTK")
    set(_license "BSD-3-Clause")
    set(_download "https://dicom.offis.de/dcmtk")
    set(_supplier "Organization: OFFIS e.V.")
    set(_copyright "Copyright OFFIS e.V.")
    set(_version "3.6.9")

  elseif("${module_name}" STREQUAL "ITKDICOMParser")
    set(_license "BSD-3-Clause")
    set(_download "https://github.com/InsightSoftwareConsortium/ITK")
    set(_supplier "Organization: Kitware Inc.")
    set(_copyright "Copyright Kitware Inc.")

  elseif("${module_name}" STREQUAL "ITKDoubleConversion")
    set(_license "BSD-3-Clause")
    set(_download "https://github.com/google/double-conversion")
    set(_supplier "Organization: Google Inc.")
    set(_copyright "Copyright Google Inc.")
    set(_version "3.1.6")

  elseif("${module_name}" STREQUAL "ITKEigen3")
    set(_license "MPL-2.0")
    set(_download "https://eigen.tuxfamily.org")
    set(_supplier "Organization: Eigen")
    set(_copyright "Copyright Eigen contributors")
    # Try to detect Eigen version from CMake variable
    if(DEFINED EIGEN3_VERSION_STRING)
      set(_version "${EIGEN3_VERSION_STRING}")
    elseif(DEFINED Eigen3_VERSION)
      set(_version "${Eigen3_VERSION}")
    endif()

  elseif("${module_name}" STREQUAL "ITKExpat")
    set(_license "MIT")
    set(_download "https://libexpat.github.io")
    set(_supplier "Organization: Expat development team")
    set(_copyright "Copyright Expat development team")

  elseif("${module_name}" STREQUAL "ITKGDCM")
    set(_license "BSD-3-Clause")
    set(_download "https://gdcm.sourceforge.net")
    set(_supplier "Organization: GDCM contributors")
    set(_copyright "Copyright GDCM contributors")
    set(_version "3.2.2")

  elseif("${module_name}" STREQUAL "ITKGIFTI")
    set(_license "LicenseRef-NITRC-Public-Domain")
    set(_download "https://www.nitrc.org/projects/gifti")
    set(_supplier "Organization: NITRC")
    set(_copyright "NOASSERTION")

  elseif("${module_name}" STREQUAL "ITKGoogleTest")
    set(_license "BSD-3-Clause")
    set(_download "https://github.com/google/googletest")
    set(_supplier "Organization: Google Inc.")
    set(_copyright "Copyright Google Inc.")
    set(_version "1.17.0")

  elseif("${module_name}" STREQUAL "ITKHDF5")
    set(_license "BSD-3-Clause")
    set(_download "https://www.hdfgroup.org/solutions/hdf5")
    set(_supplier "Organization: The HDF Group")
    set(_copyright "Copyright The HDF Group")
    # Try to detect HDF5 version from CMake variable
    if(DEFINED HDF5_VERSION)
      set(_version "${HDF5_VERSION}")
    endif()

  elseif("${module_name}" STREQUAL "ITKJPEG")
    set(_license "IJG AND BSD-3-Clause AND Zlib")
    set(_download "https://libjpeg-turbo.org")
    set(_supplier "Organization: libjpeg-turbo")
    set(_copyright "Copyright libjpeg-turbo contributors")

  elseif("${module_name}" STREQUAL "ITKKWSys")
    set(_license "BSD-3-Clause")
    set(_download "https://gitlab.kitware.com/utils/kwsys")
    set(_supplier "Organization: Kitware Inc.")
    set(_copyright "Copyright Kitware Inc.")

  elseif("${module_name}" STREQUAL "ITKMINC")
    set(_license "LGPL-2.1-only")
    set(_download "https://github.com/BIC-MNI/libminc")
    set(_supplier "Organization: McConnell Brain Imaging Centre")
    set(_copyright "Copyright McConnell Brain Imaging Centre")

  elseif("${module_name}" STREQUAL "ITKMetaIO")
    set(_license "Apache-2.0")
    set(_download "https://github.com/Kitware/MetaIO")
    set(_supplier "Organization: Kitware Inc.")
    set(_copyright "Copyright Kitware Inc.")

  elseif("${module_name}" STREQUAL "ITKNIFTI")
    set(_license "LicenseRef-NIFTI-Public-Domain")
    set(_download "https://nifti.nimh.nih.gov")
    set(_supplier "Organization: NITRC")
    set(_copyright "NOASSERTION")

  elseif("${module_name}" STREQUAL "ITKNetlib")
    set(_license "LicenseRef-Netlib-SLATEC")
    set(_download "https://www.netlib.org/slatec")
    set(_supplier "Organization: Netlib")
    set(_copyright "NOASSERTION")

  elseif("${module_name}" STREQUAL "ITKNrrdIO")
    set(_license "LGPL-2.1-only")
    set(_download "https://teem.sourceforge.net/nrrd")
    set(_supplier "Organization: Teem")
    set(_copyright "Copyright Teem contributors")

  elseif("${module_name}" STREQUAL "ITKOpenJPEG")
    set(_license "BSD-2-Clause")
    set(_download "https://www.openjpeg.org")
    set(_supplier "Organization: OpenJPEG contributors")
    set(_copyright "Copyright OpenJPEG contributors")
    set(_version "2.5.4")

  elseif("${module_name}" STREQUAL "ITKPNG")
    set(_license "Libpng-2.0")
    set(_download "http://www.libpng.org/pub/png/libpng.html")
    set(_supplier "Organization: libpng contributors")
    set(_copyright "Copyright libpng contributors")
    # Try to detect PNG version from CMake variable
    if(DEFINED PNG_VERSION_STRING)
      set(_version "${PNG_VERSION_STRING}")
    endif()

  elseif("${module_name}" STREQUAL "ITKTBB")
    set(_license "Apache-2.0")
    set(_download "https://github.com/oneapi-src/oneTBB")
    set(_supplier "Organization: Intel Corporation")
    set(_copyright "Copyright Intel Corporation")
    # Try to detect TBB version from CMake variable
    if(DEFINED TBB_VERSION)
      set(_version "${TBB_VERSION}")
    endif()

  elseif("${module_name}" STREQUAL "ITKTIFF")
    set(_license "libtiff")
    set(_download "http://www.libtiff.org")
    set(_supplier "Organization: libtiff contributors")
    set(_copyright "Copyright libtiff contributors")
    # Try to detect TIFF version from CMake variable
    if(DEFINED TIFF_VERSION_STRING)
      set(_version "${TIFF_VERSION_STRING}")
    endif()

  elseif("${module_name}" STREQUAL "ITKVNL")
    set(_license "BSD-3-Clause")
    set(_download "https://vxl.github.io")
    set(_supplier "Organization: VXL contributors")
    set(_copyright "Copyright VXL contributors")

  elseif("${module_name}" STREQUAL "ITKZLIB")
    set(_license "Zlib")
    set(_download "https://github.com/zlib-ng/zlib-ng")
    set(_supplier "Organization: zlib-ng contributors")
    set(_copyright "Copyright zlib-ng contributors")
    # Try to detect zlib version from CMake variable
    if(DEFINED ZLIB_VERSION_STRING)
      set(_version "${ZLIB_VERSION_STRING}")
    endif()

  elseif("${module_name}" STREQUAL "ITKLIBLBFGS")
    set(_license "MIT")
    set(_download "https://github.com/chokkan/liblbfgs")
    set(_supplier "Organization: Naoaki Okazaki")
    set(_copyright "Copyright Naoaki Okazaki")

  endif()

  set(${out_license} "${_license}" PARENT_SCOPE)
  set(${out_download} "${_download}" PARENT_SCOPE)
  set(${out_supplier} "${_supplier}" PARENT_SCOPE)
  set(${out_copyright} "${_copyright}" PARENT_SCOPE)
  set(${out_version} "${_version}" PARENT_SCOPE)
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
  foreach(_mod ${ITK_MODULES_ENABLED})
    if(${_mod}_IS_TEST)
      continue()
    endif()

    # Check if this is a ThirdParty module by examining its source directory
    if(NOT DEFINED ${_mod}_SOURCE_DIR)
      continue()
    endif()
    string(FIND "${${_mod}_SOURCE_DIR}" "Modules/ThirdParty" _tp_pos)
    if(_tp_pos EQUAL -1)
      continue()
    endif()

    # Get metadata for this ThirdParty module
    _itk_sbom_get_thirdparty_metadata("${_mod}"
      _pkg_license _pkg_download _pkg_supplier _pkg_copyright _pkg_version)

    # Get description from module declaration and escape for JSON
    set(_pkg_description "${ITK_MODULE_${_mod}_DESCRIPTION}")
    if(_pkg_description)
      _itk_sbom_json_escape("${_pkg_description}" _pkg_description)
    endif()

    # Sanitize module name for SPDX ID
    string(REGEX REPLACE "[^A-Za-z0-9-]" "-" _spdx_id "${_mod}")
    list(APPEND _thirdparty_spdx_ids "SPDXRef-${_spdx_id}")

    string(APPEND _json ",\n")
    string(APPEND _json "    {\n")
    string(APPEND _json "      \"SPDXID\": \"SPDXRef-${_spdx_id}\",\n")
    string(APPEND _json "      \"name\": \"${_mod}\",\n")
    string(APPEND _json "      \"versionInfo\": \"${_pkg_version}\",\n")
    string(APPEND _json "      \"downloadLocation\": \"${_pkg_download}\",\n")
    string(APPEND _json "      \"supplier\": \"${_pkg_supplier}\",\n")
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

  string(APPEND _json "\n  ]\n")

  # --- Close JSON document ---
  string(APPEND _json "}\n")

  # Write SBOM to build directory
  set(_sbom_file "${CMAKE_BINARY_DIR}/sbom.spdx.json")
  file(WRITE "${_sbom_file}" "${_json}")
  message(STATUS "SBOM generated: ${_sbom_file}")
endfunction()
