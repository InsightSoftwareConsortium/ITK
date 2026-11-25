# Mimics the CMake minimum of a typical remote module (CMP0126 OLD).
cmake_minimum_required(VERSION 3.16.3)
project(ExternalModuleSettingsCase LANGUAGES NONE)

if(DEFINED CASE_NORMAL_SHARED)
  set(BUILD_SHARED_LIBS ${CASE_NORMAL_SHARED})
endif()
include("${CMAKE_CURRENT_LIST_DIR}/../../../ITKModuleExternalSettings.cmake")
message(STATUS "BUILD_SHARED_LIBS=$CACHE{BUILD_SHARED_LIBS}")
