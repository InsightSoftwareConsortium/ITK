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
#ifndef __itkMersenneTwisterRandomVariateGenerator_h
#define __itkMersenneTwisterRandomVariateGenerator_h

#include "itkMacro.h"
#include "itkObjectFactory.h"
#include "itkRandomVariateGeneratorBase.h"
#include "itkIntTypes.h"
#include "vcl_ctime.h"
#include "vnl/vnl_math.h"
#include <climits>

namespace itk
{
namespace Statistics
{
/** \class MersenneTwisterRandomVariateGenerator
 * \brief MersenneTwisterRandom random variate generator
 *
 * \warning This class is NOT reentrant.
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

  /** Method for creation through the object factory.
      This method allocates a new instance of a Mersenne Twister,
      and initializes it with the seed from the global instance. */
  static Pointer New();

  /** returns the global Merseene Twister instance
      This method returns a Singleton of the Mersenne Twister.
      The seed is initialized from the wall clock, but can be
      set using SetSeed().
  */
  static Pointer GetInstance();

  /** Length of state vector */
  itkStaticConstMacro(StateVectorLength, IntegerType, 624);

  /** initialize with a simple IntegerType */
  void Initialize(const IntegerType oneSeed);

  /* Initialize with vcl_clock time */
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

  /** get a variate in the range [0, 1]
   * Do NOT use for CRYPTOGRAPHY without securely hashing several returned
   * values together, otherwise the generator state can be learned after
   * reading 624 consecutive values.
   */
  virtual double GetVariate();

  /** Same as GetVariate() */
  double operator()();

  // Re-seeding functions with same behavior as initializers
  inline void SetSeed(const IntegerType oneSeed);
  // inline void SetSeed(IntegerType *bigSeed, const IntegerType seedLength = StateVectorLength);
  inline void SetSeed();
  // Return the current seed
  IntegerType GetSeed() { return this->m_Seed; };

  /*
  // Saving and loading generator state
  void save( IntegerType* saveArray ) const;  // to array of size SAVE
  void load( IntegerType *const loadArray );  // from such array
  */

protected:
  inline MersenneTwisterRandomVariateGenerator();
  virtual ~MersenneTwisterRandomVariateGenerator() {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);

    // Print state vector contents
    os << indent << "State vector: " << state << std::endl;
    os << indent;
    register const IntegerType *s = state;
    register int                i = StateVectorLength;
    for (; i--; os << *s++ << "\t" ) {}
    os << std::endl;

    //Print next value to be gotten from state
    os << indent << "Next value to be gotten from state: " << pNext << std::endl;

    //Number of values left before reload
    os << indent << "Values left before next reload: " << left << std::endl;
  }

  // period parameter
  itkStaticConstMacro (M, unsigned int, 397);

  IntegerType  state[StateVectorLength]; // internal state
  IntegerType *pNext;                    // next value to get from state
  int          left;                     // number of values left before reload
                                         // needed
  IntegerType  m_Seed;                   // Seed value

  /* Reload array with N new values */
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
    return m ^ ( mixBits(s0, s1) >> 1 ) ^ ( -loBit(s1) & 0x9908b0df );
  }

  static IntegerType hash(vcl_time_t t, vcl_clock_t c);

  static Pointer m_Instance;
};  // end of class

// Declare inlined functions.... (must be declared in the header)

inline MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::hash(vcl_time_t t, vcl_clock_t c)
{
  // Get a IntegerType from t and c
  // Better than IntegerType(x) in case x is floating point in [0,1]
  // Based on code by Lawrence Kirby: fred at genesis dot demon dot co dot uk

  static IntegerType differ = 0;  // guarantee time-based seeds will change

  IntegerType    h1 = 0;
  uint8_t *p = (uint8_t *)&t;

  const unsigned int sizeOfT = static_cast< unsigned int >( sizeof(t) );
  for ( unsigned int i = 0; i < sizeOfT; ++i )
    {
    h1 *= UCHAR_MAX + 2U;
    h1 += p[i];
    }
  IntegerType h2 = 0;
  p = (uint8_t *)&c;

  const unsigned int sizeOfC = static_cast< unsigned int >( sizeof(c) );
  for ( unsigned int j = 0; j < sizeOfC; ++j )
    {
    h2 *= UCHAR_MAX + 2U;
    h2 += p[j];
    }
  return ( h1 + differ++ ) ^ h2;
}

inline void
MersenneTwisterRandomVariateGenerator::Initialize(const IntegerType seed)
{
  this->m_Seed = seed;
  // Initialize generator state with seed
  // See Knuth TAOCP Vol 2, 3rd Ed, p.106 for multiplier.
  // In previous versions, most significant bits (MSBs) of the seed affect
  // only MSBs of the state array.  Modified 9 Jan 2002 by Makoto Matsumoto.
  register IntegerType *s = state;
  register IntegerType *r = state;
  register IntegerType  i = 1;

  *s++ = seed & 0xffffffffUL;
  for ( i = 1; i < MersenneTwisterRandomVariateGenerator::StateVectorLength; ++i )
    {
    *s++ = ( 1812433253UL * ( *r ^ ( *r >> 30 ) ) + i ) & 0xffffffffUL;
    r++;
    }
}

inline void
MersenneTwisterRandomVariateGenerator::reload()
{
  // Generate N new values in state
  // Made clearer and faster by Matthew Bellew
  // matthew dot bellew at home dot com

  // get rid of VS warning
  register int index = static_cast< int >(
    M - MersenneTwisterRandomVariateGenerator::StateVectorLength );

  register IntegerType *p = state;
  register int          i;

  for ( i = MersenneTwisterRandomVariateGenerator::StateVectorLength - M; i--; ++p )
    {
    *p = twist(p[M], p[0], p[1]);
    }
  for ( i = M; --i; ++p )
    {
    *p = twist(p[index], p[0], p[1]);
    }
  *p = twist(p[index], p[0], state[0]);

  left = MersenneTwisterRandomVariateGenerator::StateVectorLength, pNext = state;
}

  /*
#define SVL 624
inline void
MersenneTwisterRandomVariateGenerator::SetSeed(
  IntegerType *const bigSeed, const IntegerType seedLength)
{
  // Seed the generator with an array of IntegerType's
  // There are 2^19937-1 possible initial states.  This function allows
  // all of those to be accessed by providing at least 19937 bits (with a
  // default seed length of StateVectorLength = 624 IntegerType's).
  // Any bits above the lower 32
  // in each element are discarded.
  // Just call seed() if you want to get array from /dev/urandom
  Initialize(19650218UL);
  register IntegerType i = 1;
  register IntegerType j = 0;
  register int         k;
  if ( StateVectorLength > seedLength )
    {
    k = StateVectorLength;
    }
  else
    {
    k = seedLength;
    }
  for (; k; --k )
    {
    state[i] =
      state[i] ^ ( ( state[i - 1] ^ ( state[i - 1] >> 30 ) ) * 1664525UL );
    state[i] += ( bigSeed[j] & 0xffffffffUL ) + j;
    state[i] &= 0xffffffffUL;
    ++i;  ++j;
    if ( i >= StateVectorLength ) { state[0] = state[StateVectorLength - 1];  i = 1; }
    if ( j >= seedLength ) { j = 0; }
    }
  for ( k = StateVectorLength - 1; k; --k )
    {
    state[i] =
      state[i] ^ ( ( state[i - 1] ^ ( state[i - 1] >> 30 ) ) * 1566083941UL );
    state[i] -= i;
    state[i] &= 0xffffffffUL;
    ++i;
    if ( i >= SVL )
      {
      state[0] = state[StateVectorLength - 1];  i = 1;
      }
    }
  state[0] = 0x80000000UL;  // MSB is 1, assuring non-zero initial array
  reload();
}
  */

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
  reload();
}

inline void
MersenneTwisterRandomVariateGenerator::SetSeed()
{
  // use time() and clock() to generate a unlikely-to-repeat seed.
  SetSeed( hash( vcl_time(0), vcl_clock() ) );
}

/** Get an integer variate in [0, 2^32-1] */
inline MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::GetIntegerVariate()
{
  if ( left == 0 ) { reload(); }
  --left;

  register IntegerType s1;
  s1 = *pNext++;
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
  // Optimized by Magnus Jonsson magnus at smartelectronix dot com
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
    i = GetIntegerVariate() & used;  // toss unused bits to shorten search
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

/* Access to a normal random number distribution */
// TODO: Compare with vnl_sample_normal
inline double
MersenneTwisterRandomVariateGenerator::GetNormalVariate(
  const double & mean, const double & variance)
{
  // Return a real number from a normal (Gaussian) distribution with given
  // mean and variance by Box-Muller method
  double r = vcl_sqrt(-2.0 * vcl_log( 1.0 - GetVariateWithOpenRange() ) * variance);
  double phi = 2.0 * vnl_math::pi
               * GetVariateWithOpenUpperRange();

  return mean + r *vcl_cos(phi);
}

/* Access to a uniform random number distribution */
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

inline
MersenneTwisterRandomVariateGenerator::MersenneTwisterRandomVariateGenerator()
{
  SetSeed (121212);
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
} // end of namespace Statistics
} // end of namespace itk

#endif
