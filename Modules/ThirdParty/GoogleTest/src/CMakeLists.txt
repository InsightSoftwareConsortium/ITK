# Prevent overriding the parent project's compiler/linker
# settings on Windows
set(gtest_force_shared_crt ON CACHE INTERNAL "")

# - google test does not properly use pthreads on mingw
# - google test uses CMake modern "Threads::Threads" alias which is
#   not properly exported easily while ITK uses the old library
#   variable style
set(gtest_disable_pthreads ON CACHE INTERNAL "")

set(BUILD_GTEST ON CACHE INTERNAL "")
set(BUILD_GMOCK OFF CACHE INTERNAL "")

# Must build GTest as static since EXCLUDE_FROM_ALL, would exclude
# needed GTest shared libraries from being installed.
set(BUILD_SHARED_LIBS OFF)

set(GTEST_SRC "${CMAKE_CURRENT_SOURCE_DIR}/itkgoogletest/")

# check if we are using "system" source code
if(ITK_USE_SYSTEM_GOOGLETEST)
  set(GTEST_SRC "${GTEST_ROOT}")
endif()

# Add googletest directly to our build but exclude from using it's
# target's and installation unless referenced by other dependencies.
add_subdirectory(
  "${GTEST_SRC}"
  "${CMAKE_CURRENT_BINARY_DIR}/GTest-build"
  EXCLUDE_FROM_ALL
)

set_property(
  TARGET
    gtest
  PROPERTY
    EXPORT_NAME
      GTest::gtest
)
set_property(
  TARGET
    gtest_main
  PROPERTY
    EXPORT_NAME
      GTest::gtest_main
)

if(NOT ITK_LEGACY_REMOVE)
  # Note: CMake depricated these alias in 3.20, and are removed in 4.1.0
  # Please use GTest::gtest and GTest::gtest_main for compatibilty
  add_library(GTest::GTest INTERFACE IMPORTED)
  add_library(GTest::Main INTERFACE IMPORTED)
  set_target_properties(
    GTest::GTest
    PROPERTIES
      INTERFACE_LINK_LIBRARIES
        GTest::gtest
      DEPRECATION
        "Use GTest::GTest is deprecated, use GTest::gtest instead."
  )
  set_target_properties(
    GTest::Main
    PROPERTIES
      INTERFACE_LINK_LIBRARIES
        GTest::gtest_main
      DEPRECATION
        "Use GTest::Main is deprecated, use GTest::gtest_main instead."
  )
endif()

itk_module_target(gtest NO_INSTALL)
itk_module_target(gtest_main NO_INSTALL)

mark_as_advanced(gtest_build_samples)
mark_as_advanced(gtest_build_tests)
mark_as_advanced(gtest_disable_pthreads)
mark_as_advanced(gtest_hide_internal_symbols)
mark_as_advanced(INSTALL_GTEST)
mark_as_advanced(GTEST_HAS_ABSL)
