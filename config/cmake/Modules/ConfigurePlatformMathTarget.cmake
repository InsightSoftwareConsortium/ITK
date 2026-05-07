# Create a canonical vxl_platform_math target for downstream use
add_library(vxl_platform_math INTERFACE)
# Ensure the interface target participates in the export set so that
# exported libraries depending on it do not trigger CMake export errors.
# INTERFACE libraries have no artifacts, but can and must be exported
# if referenced by other exported targets.
if(NOT VXL_NO_EXPORT)
    # Participate in build-tree export
    set_property(GLOBAL APPEND PROPERTY VXLTargets_MODULES vxl_platform_math)
    # Participate in install-tree export
    install(TARGETS vxl_platform_math EXPORT ${VXL_INSTALL_EXPORT_NAME})
endif()
if(CMAKE_VERSION VERSION_LESS 3.14.0)
  # Revert to previous behavior of requiring libm
  target_link_libraries(vxl_platform_math INTERFACE m)
else()
  include(CheckCSourceCompiles)
  set(_MATH_TEST_SOURCE "
#include <math.h>
int main() {
  volatile double x = 0.5;
  volatile double y = cos(x);
  (void)y;
  return 0;
}
")
  # Probe: does the toolchain link math without extra libs?
  set(CMAKE_REQUIRED_LIBRARIES "")
  check_c_source_compiles("${_MATH_TEST_SOURCE}" HAVE_IMPLICIT_MATH)

  if(HAVE_IMPLICIT_MATH)
    # Darwin and Windows typically have implicit math
  else()
    # Probe: if not implicit, does -lm fix it?
    set(CMAKE_REQUIRED_LIBRARIES "m")
    check_c_source_compiles("${_MATH_TEST_SOURCE}" HAVE_EXPLICIT_LIBM)
    if(HAVE_EXPLICIT_LIBM)
      # Most Linux toolchains that require libm to be explicitly linked
      target_link_libraries(vxl_platform_math INTERFACE m)
    else()
      message(FATAL_ERROR
        "Math linking probe failed: toolchain cannot link cos() implicitly or via -lm. "
        "This indicates a broken/misconfigured compiler/linker setup.")
    endif()
  endif()
  unset(_MATH_TEST_SOURCE)
endif()
