# Minimal consumer: proves ITKConfig.cmake exports ITK_MIGRATION_SENTINELS.
cmake_minimum_required(VERSION 3.16)
# CXX rather than NONE: ITK's config chain reaches find_package(OpenMP)
# via optional modules, and FindOpenMP requires a language to be enabled.
project(MigrationSentinelConsumer CXX)

find_package(ITK REQUIRED)

# DEFINED rather than truthiness: the list is legitimately empty when no
# sentinels are pending, but it must still be exported.
if(NOT DEFINED ITK_MIGRATION_SENTINELS)
  message(
    FATAL_ERROR
    "ITKConfig.cmake did not export ITK_MIGRATION_SENTINELS. Downstream "
    "projects cannot gate on migrations without it."
  )
endif()

message(
  STATUS
  "Consumer saw ITK_MIGRATION_SENTINELS=[${ITK_MIGRATION_SENTINELS}]"
)
