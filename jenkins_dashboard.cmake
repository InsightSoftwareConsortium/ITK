# ITK Jenkins Dashboard Script
#
# This script will include all CTest configuration files found up one
# directory from this script, then include itk_common.cmake.
#
# Configuration files should match the globbing pattern
# [0-9][0-9]*CTestConfig.cmake.
#
# These configuration files can be provided with the Jenkins Config File
# Provider Plugin.
#
#
# Corresponding Jenkins build shell commands are:
#
#  cd $WORKSPACE
#  rm -rf ITK-dashboard
#  git clone -b dashboard --single-branch https://github.com/InsightSoftwareConsortium/ITK.git ITK-dashboard
#  ctest -S ITK-dashboard/jenkins_dashboard.cmake -VV
#
#
# Corresponding Jenkins Windows build batch commands are:
#
#  cd %WORKSPACE%
#  call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\bin\amd64\vcvars64.bat"
#  rd /s /q ITK-dashboard
#  git clone -b dashboard --single-branch https://github.com/InsightSoftwareConsortium/ITK.git ITK-dashboard
#  ctest -S ITK-dashboard/jenkins_dashboard.cmake -VV

set(CTEST_DASHBOARD_ROOT "$ENV{WORKSPACE}")
# In the Jenkins Item
#   Source Code Management
#   --> Git Repositories
#       --> Additional Behaviors
#           --> Checkout to a sub-directory
# Set "Local subdirectory for repo" to ITK-src
set(dashboard_source_name "ITK-src")
set(dashboard_binary_name "ITK-bin")
set(CTEST_SITE "$ENV{NODE_NAME}")

file(GLOB config_files "${CTEST_SCRIPT_DIRECTORY}/../CTest-config/[0-9][0-9]-*CTestConfig.cmake")
list(SORT config_files)
foreach(config IN LISTS config_files)
  message(STATUS "Including ${config}...")
  file(READ "${config}" config_content)
  message("${config_content}")
  include(${config})
endforeach()

# The "platform" and "compiler" variables should be set in the
# *CTestConfig.cmake variables to set the CTEST_BUILD_NAME.
# An optional "build_description" variable may be set.
if(NOT CTEST_BUILD_NAME)
  if(platform AND compiler)
    set(CTEST_BUILD_NAME "${platform}-${compiler}${build_description}")
  endif()
endif()
string(TIMESTAMP build_date "%Y-%m-%d")
message("CDash Build Identifier: ${build_date} ${CTEST_BUILD_NAME}")
message("CTEST_SITE = ${CTEST_SITE}")
# If using a Jenkins parameterized build with CTestTestModel Choice Parameter
if(NOT DEFINED dashboard_model)
  set(model_parameter "$ENV{CTestTestModel}")
  if(model_parameter)
    set(dashboard_model "${model_parameter}")
  endif()
endif()
message(STATUS "Including itk_common.cmake...")
include(${CTEST_SCRIPT_DIRECTORY}/itk_common.cmake)
