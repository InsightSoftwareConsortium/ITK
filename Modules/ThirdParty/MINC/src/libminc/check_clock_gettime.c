#  if defined(__APPLE__)
#    include <AvailabilityMacros.h>
#    if MAC_OS_X_VERSION_MIN_REQUIRED < 101200
#      error "clock_gettime not available on macOS < 10.12"
#    endif
#  endif

#include <time.h>
#include <stdio.h>

int main(int argc, char **argv)
{
  struct timespec ts;
  return clock_gettime(CLOCK_REALTIME, &ts);
}
