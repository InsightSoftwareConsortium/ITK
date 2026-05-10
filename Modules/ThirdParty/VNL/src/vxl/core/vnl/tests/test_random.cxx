// This is core/vnl/tests/test_random.cxx
#include <iostream>
#include <cmath>
#include "vnl/vnl_random.h"
#include "testlib/testlib_test.h"

// This test deliberately exercises the deprecated lrand32 overloads to
// verify that the new fixed-width API produces an identical sequence.
// Suppress -Wdeprecated-declarations within this TU only.
#if defined(__clang__)
#  pragma clang diagnostic push
#  pragma clang diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__GNUC__)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(_MSC_VER)
#  pragma warning(push)
#  pragma warning(disable : 4996)
#endif

void
test_random()
{
  std::cout << "********************\n"
            << " Testing vnl_random\n"
            << "********************\n";

  vnl_random mz_random;
  mz_random.reseed(123456);

  TEST("lrand32", mz_random.lrand32(), 3501493769ul);
  TEST("lrand32(0,10)", mz_random.lrand32(0, 10), 9);

  // The new fixed-width-typed API must produce the same sequence as
  // lrand32() when seeded identically — they delegate to the same
  // engine. Reseed and compare draw-by-draw.
  vnl_random ref_random;
  ref_random.reseed(123456);
  vnl_random new_random;
  new_random.reseed(123456);
  TEST("next_uint32 matches lrand32", new_random.next_uint32(), static_cast<std::uint32_t>(ref_random.lrand32()));
  TEST("next_int32(0,10) matches lrand32(0,10)",
       new_random.next_int32(0, 10),
       static_cast<std::int32_t>(ref_random.lrand32(0, 10)));
  TEST(
    "next_int32(b) matches lrand32(b)", new_random.next_int32(42), static_cast<std::int32_t>(ref_random.lrand32(42)));

  const double d1 = mz_random.drand32(0, 1);
  TEST_NEAR("drand32(0,1)", d1, 0.6158541, 1e-7);
  const double d2 = mz_random.drand64(0, 1);
  TEST_NEAR("drand64(0,1)", d2, 0.2257411, 1e-7);

  double sum = 0.0;
  double sum_sq = 0.0;
  constexpr int n = 10000;
  for (int i = 0; i < n; ++i)
  {
    const double r = mz_random.normal();
    sum += r;
    sum_sq += r * r;
  }

  double mean = sum / n;
  double var = std::sqrt(sum_sq / n - mean * mean);
  TEST_NEAR("normal() mean near zero", mean, 0.0, 0.01);
  TEST_NEAR("normal() var near one", var, 1.0, 0.02);

  sum = 0.0;
  sum_sq = 0.0;
  for (int i = 0; i < n; ++i)
  {
    const double r = mz_random.normal64();
    sum += r;
    sum_sq += r * r;
  }

  mean = sum / n;
  var = std::sqrt(sum_sq / n - mean * mean);
  TEST_NEAR("normal64() mean near zero", mean, 0.0, 0.01);
  TEST_NEAR("normal64() var near one", var, 1.0, 0.01);
}

#if defined(__clang__)
#  pragma clang diagnostic pop
#elif defined(__GNUC__)
#  pragma GCC diagnostic pop
#elif defined(_MSC_VER)
#  pragma warning(pop)
#endif

TESTMAIN(test_random);
