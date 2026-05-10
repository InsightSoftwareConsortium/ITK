// This is core/vnl/vnl_random.cxx
//:
//  \file

#include <algorithm>
#include <cmath>
#include <ctime>
#include "vnl_random.h"
#include <cassert>

// next_uint32/next_int32 deliberately delegate to lrand32* to preserve
// bitwise backward compatibility — the produced sequence is identical
// for a given seed; only the public return type is corrected to fixed
// width. A future refactor onto C++11 <random> would change the
// sequence and is intentionally out of scope (separate effort).
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

unsigned long
vnl_random::linear_congruential_lrand32()
{
  return linear_congruential_previous =
           (linear_congruential_previous * linear_congruential_multiplier + 1) & 0xffffffff;
}

//: Construct with seed
vnl_random::vnl_random(unsigned long seed)
  : linear_congruential_previous(seed)

{
  reseed(seed);
}

//: Construct with seed
vnl_random::vnl_random(const std::array<unsigned long, vnl_random_array_size> & seed) { reseed(seed); }

// Deprecated C-array seed constructor (VXL_DEPRECATED_MSG in header):
// copies into std::array and forwards to the std::array overload.
// NOLINTNEXTLINE(modernize-avoid-c-arrays)
vnl_random::vnl_random(const unsigned long seed[vnl_random_array_size])
{
  std::array<unsigned long, vnl_random_array_size> tmp{};
  std::copy_n(seed, vnl_random_array_size, tmp.begin());
  reseed(tmp);
}

vnl_random::vnl_random(const vnl_random & r)
  : linear_congruential_previous(r.linear_congruential_previous)
  , mz_seed_array(r.mz_seed_array)
  , mz_array(r.mz_array)
  , mz_array_position(r.mz_array_position)
  , mz_borrow(r.mz_borrow)
  , mz_previous_normal_flag(r.mz_previous_normal_flag)
{}

vnl_random &
vnl_random::operator=(const vnl_random & r)
{
  linear_congruential_previous = r.linear_congruential_previous;
  mz_seed_array = r.mz_seed_array;
  mz_array = r.mz_array;
  mz_array_position = r.mz_array_position;
  mz_borrow = r.mz_borrow;
  mz_previous_normal_flag = r.mz_previous_normal_flag;
  return *this;
}

vnl_random::vnl_random() { reseed(); }

vnl_random::~vnl_random()
{
  mz_seed_array.fill(0);
  mz_array.fill(0);
}

void
vnl_random::reseed()
{
  reseed((unsigned long)std::time(nullptr));
}

void
vnl_random::reseed(unsigned long seed)
{
  mz_array_position = 0UL;
  mz_borrow = 0;

  linear_congruential_previous = seed;
  // Use the lc generator to fill the array
  for (unsigned int i = 0; i < vnl_random_array_size; ++i)
  {
    mz_seed_array[i] = linear_congruential_lrand32();
    mz_array[i] = mz_seed_array[i];
  }

  // Warm up with 1000 randoms
  for (int j = 0; j < 1000; j++)
    lrand32();
}

void
vnl_random::reseed(const std::array<unsigned long, vnl_random_array_size> & seed)
{
  mz_array_position = 0UL;
  mz_borrow = 0L;
  mz_seed_array = seed;
  mz_array = seed;
}

// Deprecated C-array reseed (VXL_DEPRECATED_MSG in header): copies into
// std::array and forwards to the std::array overload.
void
// NOLINTNEXTLINE(modernize-avoid-c-arrays)
vnl_random::reseed(const unsigned long seed[vnl_random_array_size])
{
  std::array<unsigned long, vnl_random_array_size> tmp{};
  std::copy_n(seed, vnl_random_array_size, tmp.begin());
  reseed(tmp);
}

void
vnl_random::restart()
{
  mz_array_position = 0UL;

  std::copy_n(mz_seed_array.begin(), vnl_random_array_size, mz_array.begin());
}

double
vnl_random::normal()
{
  if (mz_previous_normal_flag)
  {
    mz_previous_normal_flag = 0;
    return mz_previous_normal;
  }
  else
  {
    double x = NAN;
    double y = NAN;
    double r2 = NAN;
    do
    {
      x = drand32(-1.0, 1.0);
      y = drand32(-1.0, 1.0);
      r2 = x * x + y * y;
    } while (r2 >= 1.0 || r2 == 0.0);
    const double fac = std::sqrt(-2.0 * std::log(r2) / r2);
    mz_previous_normal = x * fac;
    mz_previous_normal_flag = 1;
    return y * fac;
  }
}


//: Random value from a unit normal distribution about zero
// Uses a drand64() as its underlying generator.
// Because the function uses a probability transform, the randomness (and
// quantisation) is non-linearly dependent on the value. The further the sample
// is from zero, the lower the number of bits on which it is random.
double
vnl_random::normal64()
{
  if (mz_previous_normal_flag)
  {
    mz_previous_normal_flag = 0;
    return mz_previous_normal;
  }
  else
  {
    double x = NAN;
    double y = NAN;
    double r2 = NAN;
    do
    {
      x = drand64(-1.0, 1.0);
      y = drand64(-1.0, 1.0);
      r2 = x * x + y * y;
    } while (r2 >= 1.0 || r2 == 0.0);
    const double fac = std::sqrt(-2.0 * std::log(r2) / r2);
    mz_previous_normal = x * fac;
    mz_previous_normal_flag = 1;
    return y * fac;
  }
}

unsigned long
vnl_random::lrand32()
{
  const unsigned long p1 = mz_array[(vnl_random_array_size + mz_array_position - mz_previous1) % vnl_random_array_size];
  const unsigned long p2 = (p1 - mz_array[mz_array_position] - mz_borrow) & 0xffffffff;
  if (p2 < p1)
    mz_borrow = 0;
  if (p2 > p1)
    mz_borrow = 1;
  mz_array[mz_array_position] = p2;
  mz_array_position = (mz_array_position + 1) % vnl_random_array_size;
  return p2;
}

int
vnl_random::lrand32(int lower, int upper)
{
  assert(lower <= upper);

  // Note: we have to reject some numbers otherwise we get a very slight bias
  // towards the lower part of the range lower - upper. See below

  const unsigned long range = upper - lower + 1;
  const unsigned long denom = 0xffffffff / range;
  unsigned long ran = 0;
  while ((ran = lrand32()) >= denom * range)
    ;
  return lower + int(ran / denom);
}


int
vnl_random::lrand32(int lower, int upper, int & count)
{
  assert(lower <= upper);

  // Note: we have to reject some numbers otherwise we get a very slight bias
  // towards the lower part of the range lower - upper. Hence this is a "count"
  // version of the above function that returns the number of lrand32()
  // calls made.

  const unsigned long range = upper - lower + 1;
  const unsigned long denom = 0xffffffff / range;
  unsigned long ran = 0;
  count = 1;
  while ((ran = lrand32()) >= denom * range)
    ++count;
  return lower + int(ran / denom);
}

std::uint32_t
vnl_random::next_uint32()
{
  // lrand32() already masks its output with & 0xffffffff, so the
  // narrowing cast to uint32_t is exact and well-defined on every
  // platform regardless of sizeof(unsigned long).
  return static_cast<std::uint32_t>(lrand32());
}

std::int32_t
vnl_random::next_int32(std::int32_t a, std::int32_t b)
{
  return static_cast<std::int32_t>(lrand32(a, b));
}

double
vnl_random::drand32(double lower, double upper)
{
  assert(lower <= upper);
  return (double(lrand32()) / 0xffffffff) * (upper - lower) + lower;
}

double
vnl_random::drand64(double lower, double upper)
{
  assert(lower <= upper);
  return (double(lrand32()) / 0xffffffff + double(lrand32()) / (double(0xffffffff) * double(0xffffffff))) *
           (upper - lower) +
         lower;
}

#if defined(__clang__)
#  pragma clang diagnostic pop
#elif defined(__GNUC__)
#  pragma GCC diagnostic pop
#elif defined(_MSC_VER)
#  pragma warning(pop)
#endif
