get_filename_component(_ITKExternalData_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
include(${_ITKExternalData_DIR}/ExternalData.cmake)
list(APPEND ExternalData_URL_TEMPLATES
  # Local data store populated by the ITK pre-commit hook
  "file:///${CMAKE_SOURCE_DIR}/.ExternalData/%(algo)/%(hash)"

  # Data published by MIDAS
  "http://midas.kitware.com/api/rest/midas.bitstream.by.hash?hash=%(hash)&algorithm=%(algo)"

  # Data published by developers using git-gerrit-push.
  "http://www.itk.org/files/ExternalData/%(algo)/%(hash)"
  )

# Tell ExternalData commands to transform raw files to content links.
# TODO: Condition this feature on presence of our pre-commit hook.
set(ExternalData_LINK_CONTENT MD5)
