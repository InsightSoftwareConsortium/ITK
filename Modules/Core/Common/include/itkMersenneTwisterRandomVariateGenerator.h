/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMersenneTwisterRandomVariateGenerator_h
#define itkMersenneTwisterRandomVariateGenerator_h

#include "itkMacro.h"
#include "itkObjectFactory.h"
#include "itkRandomVariateGeneratorBase.h"
#include "itkIntTypes.h"
#include "itkMath.h"
#include "itkSingletonMacro.h"

#include <atomic>
#include <mutex>
#include <climits>
#include <ctime>
#include <limits>

namespace itk::Statistics
{
/** \class MersenneTwisterRandomVariateGenerator
 * \brief MersenneTwisterRandom random variate generator
 *
 * It is recommended to create a separate object in each thread. By
 * default, each instantiated class will have a different seed created
 * by the GetNextSeed method. The creation of the initial seeds are
 * initialized once from the time. For deterministic behavior, the
 * individual instances' seeds should be manual set to separate
 * values in each thread.
 *
 * It is no longer recommended to use this class using a
 * "Singleton-like" GetInstance method for the global instance of this
 * class. This usage may result in unsafe concurrent access to
 * the global instance.
 *
 * \warning This class's instance methods are NEITHER reentrant
 * nor concurrent thread-safe, except where marked as
 * thread-safe. That is to say you can still use separate objects
 * concurrently.
 *
 * This notice was included with the original implementation.
 * The only changes made were to obfuscate the author's email addresses.
 *
 * MersenneTwister.h
 * Mersenne Twister random number generator -- a C++ class MTRand
 * Based on code by Makoto Matsumoto, Takuji Nishimura, and Shawn Cokus
 * Richard J. Wagner  v1.0  15 May 2003  rjwagner at writeme dot com
 *
 * The Mersenne Twister is an algorithm for generating random numbers.  It
 * was designed with consideration of the flaws in various other generators.
 * The period, 2^19937-1, and the order of equidistribution, 623 dimensions,
 * are far greater.  The generator is also fast; it avoids multiplication and
 * division, and it benefits from caches and pipelines.  For more information
 * see the inventors' web page at http:*www.math.keio.ac.jp/~matumoto/emt.html
 *
 * Algorithmic details can be found in \cite matsumoto1998.
 *
 * Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
 * Copyright (C) 2000 - 2003, Richard J. Wagner
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. The names of its contributors may not be used to endorse or promote
 *      products derived from this software without specific prior written
 *      permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * The original code included the following notice:
 *
 *     When you use this, send an email to: matumoto at math dot keio dot ac dot jp
 *     with an appropriate reference to your work.
 *
 * It would be nice to CC:
 * rjwagner at writeme dot com and Cokus at math dot washington dot edu
 * when you write.
 *
 * \ingroup Common
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/MersenneTwisterRandomNumberGenerator,Mersenne Twister Random Number Generator}
 * \endsphinx
 */

struct MersenneTwisterGlobals;

class ITKCommon_EXPORT MersenneTwisterRandomVariateGenerator : public RandomVariateGeneratorBase
{
public:
  /** Standard class type aliases. */
  using Self = MersenneTwisterRandomVariateGenerator;
  using Superclass = RandomVariateGeneratorBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using IntegerType = uint32_t;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MersenneTwisterRandomVariateGenerator);

  /** \brief Method for creation through the object factory.
   *
   * This method allocates a new instance of a Mersenne Twister,
   * and initializes it with the next seed from the global instance's
   * seed.
   *
   * \note This method is thread-safe.
   */
  static Pointer
  New();

  /** Return the global Mersenne Twister instance.
   *
   * This method returns a Singleton of the Mersenne Twister.
   * The seed is initialized from the wall clock at first use, but can
   * be globally set using the resulting instance's SetSeed().
   *
   * \note This method is thread-safe.
   */
  static Pointer
  GetInstance();

  /** Resets the internal data that is used to calculate the next seed. (Does not reset the initial seed.) Allows
   * generating a reproducible sequence of pseudo-random numbers. */
  static void
  ResetNextSeed();

  /** The initial seed of a default-constructed generator.
   *
   * \note The global generator retrieved by `GetInstance()`, and local generators created by `New()` may all have a
   * different (non-default) seeds.
   * \note The Mersenne Twister random number engine of the Standard C++ Library `std::mersenne_twister_engine`
   * (`std::mt19937`) has a different default seed, `default_seed == 5489`.
   */
  static constexpr IntegerType DefaultSeed = 121212;

  /** Length of state vector */
  static constexpr IntegerType StateVectorLength = 624;

#if !defined(ITK_FUTURE_LEGACY_REMOVE) && !defined(ITK_WRAPPING_PARSER)
  /** Sets the seed and initializes the internal state of this generator.
   * \deprecated ITK 6 discourages using this member function. Please use `SetSeed` instead!
   */
  ITK_FUTURE_DEPRECATED("ITK 6 discourages using this member function. Please use `SetSeed` instead!")
  void
  Initialize(const IntegerType seed = Self::CreateRandomSeed())
  {
    this->SetSeed(seed);
  }
#endif

  /** Get a random variate in the range [0, 1] */
  double
  GetVariateWithClosedRange()
  {
    return static_cast<double>(GetIntegerVariate()) * (1.0 / double{ std::numeric_limits<uint32_t>::max() });
  }

  /** Get a random variate in the range [0, n] */
  double
  GetVariateWithClosedRange(const double n)
  {
    return GetVariateWithClosedRange() * n;
  }

  /** Get a range variate in the range [0, 1) */
  double
  GetVariateWithOpenUpperRange()
  {
    return static_cast<double>(GetIntegerVariate()) / double{ 1ULL << 32ULL };
  }

  /** Get a range variate in the range [0, n) */
  double
  GetVariateWithOpenUpperRange(const double n)
  {
    return GetVariateWithOpenUpperRange() * n;
  }

  /** Get a range variate in the range (0, 1) */
  double
  GetVariateWithOpenRange()
  {
    return (static_cast<double>(GetIntegerVariate()) + 0.5) / double{ 1ULL << 32ULL };
  }

  /** Get a range variate in the range (0, n) */
  double
  GetVariateWithOpenRange(const double n)
  {
    return GetVariateWithOpenRange() * n;
  }

  /** Get an integer variate in [0, 2^32-1] */
  IntegerType
  GetIntegerVariate();

  /** Get an integer variate in [0, n] for n < 2^32 */
  IntegerType
  GetIntegerVariate(const IntegerType n);

  /** Access to 53-bit random numbers (capacity of IEEE double precision)
   * in the range [0,1) */
  double
  Get53BitVariate();

  /* Access to a normal random number distribution
   * TODO: Compare with vnl_sample_normal */
  double
  GetNormalVariate(const double mean = 0.0, const double variance = 1.0);

  /* Access to a uniform random number distribution in the range [a, b)
   * TODO: Compare with vnl_sample_uniform */
  double
  GetUniformVariate(const double a, const double b);

  /** Get a variate in the range [0, 1]
   * Do NOT use for CRYPTOGRAPHY without securely hashing several returned
   * values together, otherwise the generator state can be learned after
   * reading 624 consecutive values.
   */
  double
  GetVariate() override
  {
    return GetVariateWithClosedRange();
  }

  /** Same as GetVariate() */
  double
  operator()()
  {
    return GetVariate();
  }

  /** Re-seeding function with same behavior as Initialize.
   *
   * \note This method is thread-safe.
   */
  void
  SetSeed(const IntegerType seed = Self::CreateRandomSeed());

  /** Return the current seed
   *
   * \note This method is thread-safe.
   */
  IntegerType
  GetSeed() const
  {
    return m_Seed;
  }

  /** Return the next seed, derived as a sequence from the seed of the
   * singleton instance.
   *
   * \note This method is thread-safe.
   */
  static IntegerType
  GetNextSeed();

protected:
  MersenneTwisterRandomVariateGenerator();
  ~MersenneTwisterRandomVariateGenerator() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Reload array with N new values. */
  void
  reload();

  static IntegerType
  hash(time_t t, clock_t c);

private:
  /** Only used to synchronize the global variable across static libraries.*/
  itkGetGlobalDeclarationMacro(MersenneTwisterGlobals, PimplGlobals);

  /** Internal method to actually create a new object. */
  static Pointer
  CreateInstance();

  /** Uses time() and clock() to generate a unlikely-to-repeat seed. */
  static IntegerType
  CreateRandomSeed()
  {
    return hash(time(nullptr), clock());
  }

  /** Internal method to initialize a generator object without mutex locking. */
  void
  InitializeWithoutMutexLocking(const IntegerType seed);

  // Internal state
  IntegerType m_State[StateVectorLength];

  // Next value to get from state
  IntegerType * m_PNext{};

  // Number of values left before reload is needed
  int m_Left{};

  // Seed value
  std::atomic<IntegerType> m_Seed{};

  // Local lock to enable concurrent access to singleton
  std::mutex m_InstanceMutex{};

  // Static/Global Variable need to be thread-safely accessed

  static MersenneTwisterGlobals * m_PimplGlobals;

}; // end of class

// Declare inlined functions.... (must be declared in the header)

/** Access to 53-bit random numbers (capacity of IEEE double precision)
 * in the range [0,1) */
inline double
MersenneTwisterRandomVariateGenerator::Get53BitVariate()
{
  const IntegerType a = GetIntegerVariate() >> 5;
  const IntegerType b = GetIntegerVariate() >> 6;

  return (a * 67108864.0 + b) * (1.0 / 9007199254740992.0); // by Isaku
                                                            // Wada
}

/** Access to a normal random number distribution. */
// TODO: Compare with vnl_sample_normal
inline double
MersenneTwisterRandomVariateGenerator::GetNormalVariate(const double mean, const double variance)
{
  // Return a real number from a normal (Gaussian) distribution with given
  // mean and variance by Box-Muller method
  const double r = std::sqrt(-2.0 * std::log(1.0 - GetVariateWithOpenRange()) * variance);
  const double phi = 2.0 * itk::Math::pi * GetVariateWithOpenUpperRange();

  return mean + r * std::cos(phi);
}

/** Access to a uniform random number distribution */
// TODO: Compare with vnl_sample_uniform
inline double
MersenneTwisterRandomVariateGenerator::GetUniformVariate(const double a, const double b)
{
  const double u = GetVariateWithOpenUpperRange();

  return ((1.0 - u) * a + u * b);
}

/* Change log from MTRand.h */
// Change log:
//
// v0.1 - First release on 15 May 2000
//      - Based on code by Makoto Matsumoto, Takuji Nishimura, and Shawn Cokus
//      - Translated from C to C++
//      - Made completely ANSI compliant
//      - Designed convenient interface for initialization, seeding, and
//        obtaining numbers in default or user-defined ranges
//      - Added automatic seeding from /dev/urandom or time() and clock()
//      - Provided functions for saving and loading generator state
//
// v0.2 - Fixed bug which reloaded generator one step too late
//
// v0.3 - Switched to clearer, faster reload() code from Matthew Bellew
//
// v0.4 - Removed trailing newline in saved generator format to be consistent
//        with output format of built-in types
//
// v0.5 - Improved portability by replacing static const int's with enum's and
//        clarifying return values in seed(); suggested by Eric Heimburg
//      - Removed MAXINT constant; use 0xffffffffUL instead
//
// v0.6 - Eliminated seed overflow when uint32 is larger than 32 bits
//      - Changed integer [0,n] generator to give better uniformity
//
// v0.7 - Fixed operator precedence ambiguity in reload()
//      - Added access for real numbers in (0,1) and (0,n)
//
// v0.8 - Included time.h header to properly support time_t and clock_t
//
// v1.0 - Revised seeding to match 26 Jan 2002 update of Nishimura and Matsumoto
//      - Allowed for seeding with arrays of any length
//      - Added access for real numbers in [0,1) with 53-bit resolution
//      - Added access for real numbers from normal (Gaussian) distributions
//      - Increased overall speed by optimizing twist()
//      - Doubled speed of integer [0,n] generation
//      - Fixed out-of-range number generation on 64-bit machines
//      - Improved portability by substituting literal constants for long enum's
//      - Changed license from GNU LGPL to BSD
} // namespace itk::Statistics

#endif
