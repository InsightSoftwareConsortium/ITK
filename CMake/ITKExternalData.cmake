get_filename_component(_ITKExternalData_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
include(${_ITKExternalData_DIR}/ExternalData.cmake)

if(NOT ExternalData_OBJECT_STORES)
  # Use ExternalData_OBJECT_STORES from environment as default.
  set(ExternalData_OBJECT_STORES_DEFAULT "")
  if(DEFINED "ENV{ExternalData_OBJECT_STORES}")
    file(TO_CMAKE_PATH "$ENV{ExternalData_OBJECT_STORES}" ExternalData_OBJECT_STORES_DEFAULT)
  endif()
endif()

set(ExternalData_OBJECT_STORES "${ExternalData_OBJECT_STORES_DEFAULT}" CACHE STRING
  "Semicolon-separated list of local directories holding data objects in the layout %(algo)/%(hash).")
mark_as_advanced(ExternalData_OBJECT_STORES)
if(NOT ExternalData_OBJECT_STORES)
  set(ExternalData_OBJECT_STORES "${CMAKE_BINARY_DIR}/ExternalData/Objects")
  file(MAKE_DIRECTORY "${ExternalData_OBJECT_STORES}")
endif()
list(APPEND ExternalData_OBJECT_STORES
  # Local data store populated by the ITK pre-commit hook
  "${CMAKE_SOURCE_DIR}/.ExternalData"
  )

set(ExternalData_BINARY_ROOT ${CMAKE_BINARY_DIR}/ExternalData)

# Expands %(algo:lower)
set(ExternalData_URL_ALGO_MD5_lower md5)
set(ExternalData_URL_TEMPLATES "" CACHE STRING
  "Additional URL templates for the ExternalData CMake script to look for testing data. E.g.
file:///var/bigharddrive/%(algo)/%(hash)")
mark_as_advanced(ExternalData_URL_TEMPLATES)
if(NOT ITK_FORBID_DOWNLOADS)
  list(APPEND ExternalData_URL_TEMPLATES
    # Data published on GitHub Pages
    "https://insightsoftwareconsortium.github.io/ITKTestingData/%(algo)/%(hash)"

    # Data published on Girder
    "https://data.kitware.com:443/api/v1/file/hashsum/%(algo)/%(hash)/download"

    # Data published on MIDAS
    "https://midas3.kitware.com/midas/api/rest?method=midas.bitstream.download&checksum=%(hash)&algorithm=%(algo)"

    # Data published by developers using git-gerrit-push.
    "https://itk.org/files/ExternalData/%(algo)/%(hash)"

    # Mirror supported by the Slicer community.
    "https://slicer.kitware.com/midas3/api/rest?method=midas.bitstream.download&checksum=%(hash)&algorithm=%(algo)"
    )
endif()

# Tell ExternalData commands to transform raw files to content links.
# TODO: Condition this feature on presence of our pre-commit hook.
set(ExternalData_LINK_CONTENT MD5)

# Emscripten currently has difficulty reading symlinks.
if(EMSCRIPTEN)
  set(ExternalData_NO_SYMLINKS 1)
endif()

# Match series of the form <base>.<ext>, <base>.<n>.<ext> such that <base> may
# end in a (test) number that is not part of any series numbering.
set(ExternalData_SERIES_PARSE "()(\\.[^./]*)$")
set(ExternalData_SERIES_MATCH "(\\.[0-9]+)?")

# Sometimes we want to download very large files.
set(ExternalData_TIMEOUT_ABSOLUTE 900)
