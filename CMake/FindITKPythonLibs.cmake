# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#.rst:
# FindITKPythonLibs
# -----------------
#
# FindITKPythonLibs is a work around to avoid a bug in FindPythonLibs.cmake .
# This module should always be used in place of FindPythonLibs. The code
# contained in this file will be directly integrated in FindPythonLibs.cmake in
# CMake (version more recent than 3.7.1).
#
# PYTHON_LIBRARY may contain a list because of SelectLibraryConfigurations.
# If it is the case, the list can be:
#
# ::
#
#   optimized;<FILEPATH_TO_RELEASE_LIBRARY>;debug;<FILEPATH_TO_DEBUG_LIBRARY>
#
# In this case, we need to set PYTHON_LIBRARY to one value, otherwise
# get_filename_component will crash (too many arguments).
# If SelectLibraryConfigurations was run, then the individual
# PYTHON_LIBRARY_<CONFIG> values have been populated.

if(CMAKE_VERSION VERSION_GREATER 3.7.1)
  # Do nothing, the bug has been fixed
else()
  if(PYTHON_LIBRARY_RELEASE)
    set(PYTHON_LIBRARY ${PYTHON_LIBRARY_RELEASE})
  elseif(PYTHON_LIBRARY_DEBUG)
    set(PYTHON_LIBRARY ${PYTHON_LIBRARY_DEBUG})
  endif()
endif()
find_package(PythonLibs)
