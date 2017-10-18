/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkMutexLockHolder.h"
#include <climits>
#include <ctime>

namespace itk
{
namespace Statistics
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
 * Reference
 * M. Matsumoto and T. Nishimura, "Mersenne Twister: A 623-Dimensionally
 * Equidistributed Uniform Pseudo-Random Number Generator", ACM Transactions on
 * Modeling and Computer Simulation, Vol. 8, No. 1, January 1998, pp 3-30.
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
 * \wiki
 * \wikiexample{Utilities/MersenneTwisterRandomVariateGenerator,Random number generator}
 * \endwiki
 */
class ITKCommon_EXPORT MersenneTwisterRandomVariateGenerator:
  public RandomVariateGeneratorBase
{
public:

  /** Standard class typedefs. */
  typedef MersenneTwisterRandomVariateGenerator Self;
  typedef RandomVariateGeneratorBase            Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  typedef uint32_t IntegerType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MersenneTwisterRandomVariateGenerator,
               RandomVariateGeneratorBase);

  /** \brief Method for creation through the object factory.
   *
   * This method allocates a new instance of a Mersenne Twister,
   * and initializes it with the next seed from the global instance's
   * seed.
   *
   * \note This method is thread-safe.
   */
  static Pointer New();

  /** Return the global Mersenne Twister instance.
   *
   * This method returns a Singleton of the Mersenne Twister.
   * The seed is initialized from the wall clock at first use, but can
   * be globally set using the resulting instance's SetSeed().
   *
   * \note This method is thread-safe.
   */
  static Pointer GetInstance();

  /** Length of state vector */
  itkStaticConstMacro(StateVectorLength, IntegerType, 624);

  /** Initialize with a simple IntegerType */
  void Initialize(const IntegerType oneSeed);

  /* Initialize with clock time */
  void Initialize();

  /** Get a random variate in the range [0, 1] */
  double GetVariateWithClosedRange();

  /** Get a random variate in the range [0, n] */
  double GetVariateWithClosedRange(const double & n);

  /** Get a range variate in the range [0, 1) */
  double GetVariateWithOpenUpperRange();

  /** Get a range variate in the range [0, n) */
  double GetVariateWithOpenUpperRange(const double & n);

  /** Get a range variate in the range (0, 1) */
  double GetVariateWithOpenRange();

  /** Get a range variate in the range (0, n) */
  double GetVariateWithOpenRange(const double & n);

  /** Get an integer variate in [0, 2^32-1] */
  IntegerType GetIntegerVariate();

  /** Get an integer variate in [0, n] for n < 2^32 */
  IntegerType GetIntegerVariate(const IntegerType & n);

  /** Access to 53-bit random numbers (capacity of IEEE double precision)
   * in the range [0,1) */
  double Get53BitVariate();

  /* Access to a normal random number distribution
   * TODO: Compare with vnl_sample_normal */
  double GetNormalVariate(const double & mean = 0.0,
                          const double & variance = 1.0);

  /* Access to a uniform random number distribution in the range [a, b)
   * TODO: Compare with vnl_sample_uniform */
  double GetUniformVariate(const double & a, const double & b);

  /** Get a variate in the range [0, 1]
   * Do NOT use for CRYPTOGRAPHY without securely hashing several returned
   * values together, otherwise the generator state can be learned after
   * reading 624 consecutive values.
   */
  virtual double GetVariate() ITK_OVERRIDE;

  /** Same as GetVariate() */
  double operator()();

  /** Re-seeding functions with same behavior as initializers
   *
   * \note This method is thread-safe.
   */
  inline void SetSeed(const IntegerType oneSeed);
  inline void SetSeed();

  /** Return the current seed
   *
   * \note This method is thread-safe.
   */
  IntegerType GetSeed();

  /** Return the next seed, derived as a sequence from the seed of the
   * singleton instance.
   *
   * \note This method is thread-safe.
   */
  static IntegerType GetNextSeed();

  /*
  // Saving and loading generator state
  void save( IntegerType* saveArray ) const;  // to array of size SAVE
  void load( IntegerType *const loadArray );  // from such array
  */

protected:
  MersenneTwisterRandomVariateGenerator();
  virtual ~MersenneTwisterRandomVariateGenerator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Period parameter */
  itkStaticConstMacro(M, unsigned int, 397);

  /** Reload array with N new values. */
  void reload();

  IntegerType hiBit(const IntegerType & u) const { return u & 0x80000000; }
  IntegerType loBit(const IntegerType & u) const { return u & 0x00000001; }
  IntegerType loBits(const IntegerType & u) const { return u & 0x7fffffff; }
  IntegerType mixBits(const IntegerType & u, const IntegerType & v) const
  {
    return hiBit(u) | loBits(v);
  }

  IntegerType twist(const IntegerType & m, const IntegerType & s0, const IntegerType & s1) const
  {
    return m ^ ( mixBits(s0, s1) >> 1 ) ^ ( -static_cast<int32_t>(loBit(s1)) & 0x9908b0df );
  }

  static IntegerType hash(time_t t, clock_t c);

  // Internal state
  IntegerType  state[StateVectorLength];

  // Next value to get from state
  IntegerType *m_PNext;

  // Number of values left before reload is needed
  int          m_Left;

  // Seed value
  IntegerType  m_Seed;

private:

  /** Internal method to actually create a new object. */
  static Pointer CreateInstance();

  // Local lock to enable concurrent access to singleton
  SimpleFastMutexLock m_InstanceLock;

  // Static/Global Variable need to be thread-safely accessed
  static Pointer             m_StaticInstance;
  static SimpleFastMutexLock m_StaticInstanceLock;
  static IntegerType         m_StaticDiffer;

};  // end of class

// Declare inlined functions.... (must be declared in the header)

inline void
MersenneTwisterRandomVariateGenerator::Initialize(const IntegerType seed)
{
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_InstanceLock);
  this->m_Seed = seed;
  // Initialize generator state with seed
  // See Knuth TAOCP Vol 2, 3rd Ed, p.106 for multiplier.
  // In previous versions, most significant bits (MSBs) of the seed affect
  // only MSBs of the state array.  Modified 9 Jan 2002 by Makoto Matsumoto.
  IntegerType *s = state;
  IntegerType *r = state;
  IntegerType  i = 1;

  *s++ = seed & 0xffffffffUL;
  for ( i = 1; i < MersenneTwisterRandomVariateGenerator::StateVectorLength; ++i )
    {
    *s++ = ( 1812433253UL * ( *r ^ ( *r >> 30 ) ) + i ) & 0xffffffffUL;
    r++;
    }
  reload();
}

inline void
MersenneTwisterRandomVariateGenerator::reload()
{
  // Generate N new values in state
  // Made clearer and faster by Matthew Bellew
  // matthew dot bellew at home dot com

  // get rid of VS warning
  int index = static_cast< int >(
    M - MersenneTwisterRandomVariateGenerator::StateVectorLength );

  IntegerType *p = state;
  int          i;

  for ( i = MersenneTwisterRandomVariateGenerator::StateVectorLength - M; i--; ++p )
    {
    *p = twist(p[M], p[0], p[1]);
    }
  for ( i = M; --i; ++p )
    {
    *p = twist(p[index], p[0], p[1]);
    }
  *p = twist(p[index], p[0], state[0]);

  m_Left = MersenneTwisterRandomVariateGenerator::StateVectorLength;
  m_PNext = state;
}

inline void
MersenneTwisterRandomVariateGenerator::Initialize()
{
  SetSeed();
}

inline void
MersenneTwisterRandomVariateGenerator::SetSeed(const IntegerType oneSeed)
{
  // Seed the generator with a simple IntegerType
  Initialize(oneSeed);
}

inline void
MersenneTwisterRandomVariateGenerator::SetSeed()
{
  // use time() and clock() to generate a unlikely-to-repeat seed.
  SetSeed( hash( time(ITK_NULLPTR), clock() ) );
}


inline MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::GetSeed()
{
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_InstanceLock);
  volatile IntegerType seed = this->m_Seed;
  return seed;
}

/** Get an integer variate in [0, 2^32-1] */
inline MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::GetIntegerVariate()
{
  if ( m_Left == 0 ) { reload(); }
  --m_Left;

  IntegerType s1 = *m_PNext++;
  s1 ^= ( s1 >> 11 );
  s1 ^= ( s1 <<  7 ) & 0x9d2c5680;
  s1 ^= ( s1 << 15 ) & 0xefc60000;
  return ( s1 ^ ( s1 >> 18 ) );
}

inline double
MersenneTwisterRandomVariateGenerator::GetVariateWithClosedRange()
{
  return double( GetIntegerVariate() ) * ( 1.0 / 4294967295.0 );
}

/** Get a random variate in the range [0, n] */
inline double
MersenneTwisterRandomVariateGenerator::GetVariateWithClosedRange(
  const double & n)
{
  return GetVariateWithClosedRange() * n;
}

/** Get a range variate in the range [0, 1) */
inline double
MersenneTwisterRandomVariateGenerator::GetVariateWithOpenUpperRange()
{
  return double( GetIntegerVariate() ) * ( 1.0 / 4294967296.0 );
}

/** Get a range variate in the range [0, n) */
inline double
MersenneTwisterRandomVariateGenerator::GetVariateWithOpenUpperRange(
  const double & n)
{
  return GetVariateWithOpenUpperRange() * n;
}

/** Get a range variate in the range (0, 1) */
inline double
MersenneTwisterRandomVariateGenerator::GetVariateWithOpenRange()
{
  return ( double( GetIntegerVariate() ) + 0.5 ) * ( 1.0 / 4294967296.0 );
}

/** Get a range variate in the range (0, n) */
inline double
MersenneTwisterRandomVariateGenerator::GetVariateWithOpenRange(
  const double & n)
{
  return GetVariateWithOpenRange() * n;
}

inline MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::GetIntegerVariate(
  const IntegerType & n)
{
  // Find which bits are used in n
  IntegerType used = n;

  used |= used >> 1;
  used |= used >> 2;
  used |= used >> 4;
  used |= used >> 8;
  used |= used >> 16;

  // Draw numbers until one is found in [0,n]
  IntegerType i;
  do
    {
    i = GetIntegerVariate() & used; // toss unused bits to shorten search
    }
  while ( i > n );

  return i;
}

/** Access to 53-bit random numbers (capacity of IEEE double precision)
 * in the range [0,1) */
inline double
MersenneTwisterRandomVariateGenerator::Get53BitVariate()
{
  IntegerType a = GetIntegerVariate() >> 5, b = GetIntegerVariate() >> 6;

  return ( a * 67108864.0 + b ) * ( 1.0 / 9007199254740992.0 );  // by Isaku
                                                                 // Wada
}

/** Access to a normal random number distribution. */
// TODO: Compare with vnl_sample_normal
inline double
MersenneTwisterRandomVariateGenerator::GetNormalVariate(
  const double & mean, const double & variance)
{
  // Return a real number from a normal (Gaussian) distribution with given
  // mean and variance by Box-Muller method
  double r = std::sqrt(-2.0 * std::log( 1.0 - GetVariateWithOpenRange() ) * variance);
  double phi = 2.0 * itk::Math::pi
               * GetVariateWithOpenUpperRange();

  return mean + r *std::cos(phi);
}

/** Access to a uniform random number distribution */
// TODO: Compare with vnl_sample_uniform
inline double
MersenneTwisterRandomVariateGenerator::GetUniformVariate(
  const double & a, const double & b)
{
  double u = GetVariateWithOpenUpperRange();

  return ( ( 1.0 - u ) * a + u * b );
}

inline double
MersenneTwisterRandomVariateGenerator::GetVariate()
{
  return GetVariateWithClosedRange();
}

inline double
MersenneTwisterRandomVariateGenerator::operator()()
{
  return GetVariate();
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
} // end namespace Statistics
} // end namespace itk

#endif
