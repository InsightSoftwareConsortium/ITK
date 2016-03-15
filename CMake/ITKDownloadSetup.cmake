# This script provides download related helper macro and sets up download
# environment.

#-----------------------------------------------------------------------------
# Forbid downloading resources from the network during a build. This helps
# when building on systems without network connectivity to determine which
# resources must be obtained manually and made available to the build.
#
option(ITK_FORBID_DOWNLOADS "Do not download source code or data from the network" OFF)
mark_as_advanced(ITK_FORBID_DOWNLOADS)

macro(itk_download_attempt_check _name)
  if(ITK_FORBID_DOWNLOADS)
    message(SEND_ERROR "Attempted to download ${_name} when ITK_FORBID_DOWNLOADS is ON")
  endif()
endmacro()

#-----------------------------------------------------------------------------
# Git protocol setup
#
if(NOT ITK_FORBID_DOWNLOADS)
  find_package(Git)
  set(ITK_USE_GIT_PROTOCOL_default "OFF")
  if (GIT_VERSION_STRING VERSION_LESS "1.7.10")
    # minimum version for https support
    set(ITK_USE_GIT_PROTOCOL_default "ON")
  endif()
  option(ITK_USE_GIT_PROTOCOL "If behind a firewall turn this off to use https instead." ${ITK_USE_GIT_PROTOCOL_default})
  mark_as_advanced(ITK_USE_GIT_PROTOCOL)
  set(git_protocol "https")
  if(ITK_USE_GIT_PROTOCOL)
    set(git_protocol "git")
  endif()
endif()
