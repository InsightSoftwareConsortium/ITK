project(ITKTesting)

# The Testing directory is setup so it can build either as part of the
# normal ITK build or as a stand-alone project.  This design is useful
# for testing the installation tree of ITK.

if(ITK_CSWIG_JAVA)
  find_package(Java)
endif(ITK_CSWIG_JAVA)

make_directory(${ITKTesting_BINARY_DIR}/Temporary)
if(ITK_BINARY_DIR)
  # We are building inside the tree.
  add_subdirectory(Code)

  if(ITK_CSWIG_TCL)
    set(ITKWISH_EXECUTABLE "${CXX_TEST_PATH}/itkwish")
  endif(ITK_CSWIG_TCL)
else(ITK_BINARY_DIR)
  # We are building as a stand-alone project.

  cmake_minimum_required(VERSION 2.4)
  if(COMMAND CMAKE_POLICY)
    cmake_policy(SET CMP0003 NEW)
  endif(COMMAND CMAKE_POLICY)

  set(LIBRARY_OUTPUT_PATH ${ITKTesting_BINARY_DIR}/bin CACHE PATH "Single output directory for building all libraries.")
  set(EXECUTABLE_OUTPUT_PATH ${ITKTesting_BINARY_DIR}/bin CACHE PATH "Single output directory for building all executables.")
  mark_as_advanced(LIBRARY_OUTPUT_PATH EXECUTABLE_OUTPUT_PATH)
  set(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})
  get_filename_component(ITK_SOURCE_DIR "${ITKTesting_SOURCE_DIR}" PATH)

  set(ITK_COMPUTER_MEMORY_SIZE 1 CACHE STRING "Provide here the size of your RAM Memory in GigaBytes")
  mark_as_advanced(ITK_COMPUTER_MEMORY_SIZE)

  # Configure Dart testing support.
  include(${CMAKE_ROOT}/Modules/Dart.cmake)
  mark_as_advanced(TCL_TCLSH)
  if(BUILD_TESTING)
    enable_testing()
  endif(BUILD_TESTING)

  # Find the data directory.
  find_path(ITK_DATA_ROOT ITKData.readme ${ITKTesting_SOURCE_DIR}/Data $ENV{ITK_DATA_ROOT})
  mark_as_advanced(ITK_DATA_ROOT)

  # Find ITK and load its settings.
  find_package(ITK)
  if(ITK_FOUND)
    include(${ITK_USE_FILE})
    #-----------------------------------------------------------------------------
    # Test for some required system information.
    include(${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityC.cmake)
    include(${CMAKE_ROOT}/Modules/CMakeBackwardCompatibilityCXX.cmake)
    #-----------------------------------------------------------------------------
    add_subdirectory(Code)
    if(ITK_CSWIG_TCL)
      find_program(ITKWISH_EXECUTABLE itkwish ${ITK_DIR}/bin ${ITK_DIR})
    endif(ITK_CSWIG_TCL)
    if(ITK_CSWIG_PYTHON)
      find_program(PYTHON_EXECUTABLE
        NAMES python python2.3 python2.2 python2.1 python2.0 python1.6 python1.5
        PATHS
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.3\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.2\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.1\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\2.0\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.6\\InstallPath]
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\1.5\\InstallPath]
        )
      mark_as_advanced(PYTHON_EXECUTABLE)
    endif(ITK_CSWIG_PYTHON)
  else(ITK_FOUND)
    message("ITK not found.  Please set ITK_DIR")
  endif(ITK_FOUND)
endif(ITK_BINARY_DIR)
