#include <fftw3.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef FFTW_TEST_FLOAT
#  define FFTW(name) fftwf_##name
typedef float Real;
#else
#  define FFTW(name) fftw_##name
typedef double Real;
#endif

int
main(void)
{
  enum
  {
    size = 8
  };
  Real input[size] = { 1 };
  FFTW(complex) output[size / 2 + 1];
  FFTW(plan) plan;
  int result = EXIT_SUCCESS;
  int i;

  if (!FFTW(init_threads)())
  {
    fprintf(stderr, "FFTW thread initialization failed\n");
    return EXIT_FAILURE;
  }
  FFTW(plan_with_nthreads)(2);
  plan = FFTW(plan_dft_r2c_1d)(size, input, output, FFTW_ESTIMATE);
  if (!plan)
  {
    fprintf(stderr, "FFTW plan creation failed\n");
    FFTW(cleanup_threads)();
    return EXIT_FAILURE;
  }
  FFTW(execute)(plan);
  for (i = 0; i <= size / 2; ++i)
  {
    if (fabs(output[i][0] - 1) > 1e-6 || fabs(output[i][1]) > 1e-6)
    {
      fprintf(stderr, "Incorrect FFTW impulse transform at index %d\n", i);
      result = EXIT_FAILURE;
      break;
    }
  }
  FFTW(destroy_plan)(plan);
  FFTW(cleanup_threads)();
  return result;
}
