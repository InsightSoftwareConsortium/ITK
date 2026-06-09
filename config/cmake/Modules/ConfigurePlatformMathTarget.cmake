# Probe the math libraries needed to link cos()/etc. and expose them as
# VXL_PLATFORM_MATH_LIBRARIES ("" when math is implicit, "m" otherwise).
# Consumers link the resolved system libs directly, so no named vxl target
# leaks into ITK's embedded export set as a dangling -l flag.
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
set(CMAKE_REQUIRED_LIBRARIES "")
check_c_source_compiles("${_MATH_TEST_SOURCE}" HAVE_IMPLICIT_MATH)
if(HAVE_IMPLICIT_MATH)
  set(VXL_PLATFORM_MATH_LIBRARIES "" CACHE INTERNAL "Math libs required to link vxl")
else()
  set(CMAKE_REQUIRED_LIBRARIES "m")
  check_c_source_compiles("${_MATH_TEST_SOURCE}" HAVE_EXPLICIT_LIBM)
  if(HAVE_EXPLICIT_LIBM)
    set(VXL_PLATFORM_MATH_LIBRARIES "m" CACHE INTERNAL "Math libs required to link vxl")
  else()
    message(FATAL_ERROR
      "Math linking probe failed: toolchain cannot link cos() implicitly or via -lm. "
      "This indicates a broken/misconfigured compiler/linker setup.")
  endif()
endif()
unset(_MATH_TEST_SOURCE)
