// This is core/vnl/vnl_random.h
#ifndef vnl_random_h
#define vnl_random_h
#ifdef _MSC_VER
#  include <vcl_msvc_warnings.h>
#endif
#include <array>
#include <cstdint>
#include "vcl_compiler.h"
#include "vnl/vnl_export.h"

//:
// \file
// \author Aaron Kotcheff (Manchester)
// \brief A superior random number generator

constexpr unsigned int vnl_random_array_size = 37;

//: A superior random number generator.
// Implements a new random number generator that
// recently appeared in the literature. It generates 32 bit
// numbers with a higher degree of randomness than previous
// generators and has a cycle of 10^354 i.e. so huge that in
// practice it never cycles.
// For the mathematics behind it see:
// "A New Class of Random Number Generators" G. Marsaglia and A. Zaman,
// Annals of Applied Probability 1991, Vol. 1, No. 3, 462.
class VNL_EXPORT vnl_random
{
  enum
  {
    linear_congruential_multiplier = 1664525,
    mz_previous1 = 24
  };
  unsigned long linear_congruential_previous;
  std::array<unsigned long, vnl_random_array_size> mz_seed_array{};
  std::array<unsigned long, vnl_random_array_size> mz_array{};
  unsigned int mz_array_position{ 0UL };
  int mz_borrow{ 0 };
  unsigned long
  linear_congruential_lrand32();

  double mz_previous_normal;
  int mz_previous_normal_flag{ 0 };

public:
  //: Default constructor.
  // Initializes the random number generator non-deterministically.
  // i.e. it will generate a different series of random numbers each
  // time the program is run.
  vnl_random();

  //: Destructor
  ~vnl_random();

  //: Construct with seed.
  //  Initializes the random number generator deterministically
  //  using a single ulong as the 'seed'. A linear congruential
  //  generator is used to generate the 37 ulongs needed
  //  as the real seed. The same seed will produce the
  //  same series of random numbers.
  //
  //  9667566  is a good seed.
  vnl_random(unsigned long seed);

  //: Construct with seed.
  //  Initializes the random number generator deterministically
  //  using 37 ulongs as the 'seed'. The same seed will
  //  produce the same series of random numbers.
  vnl_random(const std::array<unsigned long, vnl_random_array_size> & seed);

  //: Construct with C-array seed (deprecated; use std::array overload).
  [[deprecated("Pass std::array<unsigned long, vnl_random_array_size> instead")]]
  // NOLINTNEXTLINE(modernize-avoid-c-arrays)
  vnl_random(const unsigned long seed[vnl_random_array_size]);

  //: Copy constructor.
  //  Initializes/sets the random number generator to exactly
  //  the same state as the argument, i.e. both will generate exactly
  //  the same series of random numbers from then on.
  vnl_random(const vnl_random &);

  //: Copy operator.
  //  Initializes/sets the random number generator to exactly
  //  the same state as the argument, i.e. both will generate exactly
  //  the same series of random numbers from then on.
  vnl_random &
  operator=(const vnl_random &);

  //: Starts a new non-deterministic sequence from an already declared generator.
  void
  reseed();

  //: Starts a new deterministic sequence from an already declared generator using the provided seed.
  void
  reseed(unsigned long);

  //: Starts a new deterministic sequence from an already declared generator using the provided seed.
  void
  reseed(const std::array<unsigned long, vnl_random_array_size> & seed);

  //: Reseed from C-array (deprecated; use std::array overload).
  [[deprecated("Pass std::array<unsigned long, vnl_random_array_size> instead")]]
  // NOLINTNEXTLINE(modernize-avoid-c-arrays)
  void
  reseed(const unsigned long seed[vnl_random_array_size]);

  //: This restarts the sequence of random numbers.
  //  Restarts so that it repeats
  //  from the point at which you declared the generator, last
  //  initialized it, or last called a 'reseed'.
  void
  restart();

  //: Generates a random unsigned 32-bit value in [0, 2^32 - 1].
  // \deprecated Use next_uint32() — fixed-width return type (issue #976).
  [[deprecated("Use next_uint32() - see vxl issue #976")]]
  unsigned long
  lrand32();

  //: Generates a random signed integer in the inclusive range [a, b].
  // \deprecated Use next_int32(a, b) — fixed-width return type (issue #976).
  // \pre \c a \c <= \c b
  [[deprecated("Use next_int32(a, b) - see vxl issue #976")]]
  int
  lrand32(int a, int b);

  //: Generates a random signed integer in the inclusive range [0, b].
  // \deprecated Use next_int32(b) — fixed-width return type (issue #976).
  // \pre \c 0 \c <= \c b
  [[deprecated("Use next_int32(b) - see vxl issue #976")]]
  int
  lrand32(int b)
  {
    return next_int32(0, b);
  }

  //: Generates a random signed integer in [a, b]; \c count returns
  //  the number of underlying draws taken (rejection sampling).
  // \deprecated No fixed-width replacement; use std::uniform_int_distribution
  //             over next_uint32() if a draw count is needed.
  [[deprecated("No direct replacement; use std::uniform_int_distribution over next_uint32()")]]
  int
  lrand32(int a, int b, int &);

  //: Generates a random 32-bit unsigned integer in [0, 2^32 - 1].
  //  Strongly typed C++11 replacement for \c lrand32(). Delegates to
  //  the same underlying Marsaglia-Zaman engine so migrating callers
  //  observe an identical random sequence. Prefer this over
  //  \c lrand32() in new code; the return type is guaranteed to be
  //  exactly 32 bits on every platform.
  std::uint32_t
  next_uint32();

  //: Generates a random signed 32-bit integer in [a, b] (inclusive).
  //  Strongly typed C++11 replacement for \c lrand32(int,int).
  //  Delegates to the existing rejection-sampling implementation, so
  //  the produced sequence matches \c lrand32(a,b). Prefer this in
  //  new code.
  //  \pre \c a \c <= \c b
  std::int32_t
  next_int32(std::int32_t a, std::int32_t b);

  //: Generates a random signed 32-bit integer in [0, b] (inclusive).
  //  Strongly typed C++11 replacement for \c lrand32(int).
  //  \pre \c 0 \c <= \c b
  std::int32_t
  next_int32(std::int32_t b)
  {
    return next_int32(0, b);
  }

  //:  Generates a random double in the range a <= x <= b with 32 bit randomness.
  //   drand32(1,0) is random down to about the 10th decimal place.
  double
  drand32(double a, double b);

  //: Generates a random unsigned integer in [0,n)
  // This function allows the random number generator to be used as
  // a functor, e.g. with std::random_shuffle()
  unsigned long
  operator()(unsigned n)
  {
    return static_cast<unsigned long>(next_int32(static_cast<std::int32_t>(n) - 1));
  }

  //:  Generates a random double in the range 0 <= x <= b with 32 bit randomness.
  //   drand32(1.0) is random down to about the 10th decimal place.
  double
  drand32(double b)
  {
    return drand32(0.0, b);
  }

  //:  Generates a random double in the range 0 <= x <= 1 with 32 bit randomness.
  //   drand32() is random down to about the 10th decimal place.
  double
  drand32()
  {
    return drand32(0.0, 1.0);
  }

  //: Generates a random double in the range a <= x <= b with 64 bit randomness.
  //  Completely random down to the accuracy of an IEEE double.
  double
  drand64(double a, double b);

  //: Generates a random double in the range 0 <= x <= b with 64 bit randomness.
  //  Completely random down to the accuracy of an IEEE double.
  double
  drand64(double b)
  {
    return drand64(0.0, b);
  }

  //: Generates a random double in the range 0 <= x <= 1 with 64 bit randomness.
  //  Completely random down to the accuracy of an IEEE double.
  double
  drand64()
  {
    return drand64(0.0, 1.0);
  }

  //: Random value from a unit normal distribution about zero.
  // Uses a drand32() as its underlying generator.
  // Because the function uses a probability transform, the randomness (and
  // quantisation) is non-linearly dependent on the value. The further the
  // sample is from zero, the lower the number of bits on which it is random.
  double
  normal();

  //: Random value from a unit normal distribution about zero.
  // Uses a drand64() as its underlying generator.
  // Because the function uses a probability transform, the randomness (and
  // quantisation) is non-linearly dependent on the value. The further the
  // sample is from zero, the lower the number of bits on which it is random.
  double
  normal64();
};

#endif // vnl_random_h
