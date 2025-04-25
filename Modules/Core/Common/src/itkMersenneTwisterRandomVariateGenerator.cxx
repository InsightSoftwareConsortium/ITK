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
#include "itkMersenneTwisterRandomVariateGenerator.h"

#include "itkSingleton.h"

#include <atomic>

namespace itk
{
namespace Statistics
{
namespace
{
using IntegerType = MersenneTwisterRandomVariateGenerator::IntegerType;

IntegerType
hiBit(const IntegerType u)
{
  return u & 0x80000000;
}

IntegerType
loBit(const IntegerType u)
{
  return u & 0x00000001;
}

IntegerType
loBits(const IntegerType u)
{
  return u & 0x7fffffff;
}
IntegerType
mixBits(const IntegerType u, const IntegerType v)
{
  return hiBit(u) | loBits(v);
}

IntegerType
twist(const IntegerType m, const IntegerType s0, const IntegerType s1)
{
  return m ^ (mixBits(s0, s1) >> 1) ^ (-static_cast<int32_t>(loBit(s1)) & 0x9908b0df);
}
} // namespace


/** Private nested class to easily synchronize global variables across static libraries.*/
struct MersenneTwisterGlobals
{
  ITK_DISALLOW_COPY_AND_MOVE(MersenneTwisterGlobals);

  MersenneTwisterGlobals() = default;
  ~MersenneTwisterGlobals() = default;

  MersenneTwisterRandomVariateGenerator::Pointer                  m_StaticInstance{};
  std::mutex                                                      m_StaticInstanceMutex{};
  std::atomic<MersenneTwisterRandomVariateGenerator::IntegerType> m_StaticDiffer{};
};

itkGetGlobalSimpleMacro(MersenneTwisterRandomVariateGenerator, MersenneTwisterGlobals, PimplGlobals);

MersenneTwisterGlobals * MersenneTwisterRandomVariateGenerator::m_PimplGlobals;

MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator::CreateInstance()
{
  // Try the factory first
  MersenneTwisterRandomVariateGenerator::Pointer obj = ObjectFactory<Self>::Create();
  // If the factory did not provide one, then create it here
  if (!obj)
  {
    obj = new MersenneTwisterRandomVariateGenerator;
    // Remove extra reference from construction.
    obj->UnRegister();
  }
  return obj;
}


MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator::New()
{
  MersenneTwisterRandomVariateGenerator::Pointer obj = MersenneTwisterRandomVariateGenerator::CreateInstance();

  obj->InitializeWithoutMutexLocking(MersenneTwisterRandomVariateGenerator::GetNextSeed());
  return obj;
}

MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator::GetInstance()
{
  itkInitGlobalsMacro(PimplGlobals);
  const std::lock_guard<std::mutex> lockGuard(m_PimplGlobals->m_StaticInstanceMutex);

  if (!m_PimplGlobals->m_StaticInstance)
  {
    m_PimplGlobals->m_StaticInstance = MersenneTwisterRandomVariateGenerator::CreateInstance();
    m_PimplGlobals->m_StaticInstance->InitializeWithoutMutexLocking(Self::CreateRandomSeed());
  }

  return m_PimplGlobals->m_StaticInstance;
}


void
MersenneTwisterRandomVariateGenerator::ResetNextSeed()
{
  itkInitGlobalsMacro(PimplGlobals);
  m_PimplGlobals->m_StaticDiffer = 0;
}


MersenneTwisterRandomVariateGenerator::MersenneTwisterRandomVariateGenerator()
{
  this->InitializeWithoutMutexLocking(121212);
}

MersenneTwisterRandomVariateGenerator::~MersenneTwisterRandomVariateGenerator() = default;

MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::hash(const time_t t, const clock_t c)
{
  itkInitGlobalsMacro(PimplGlobals);
  // Get an IntegerType from t and c
  // Better than IntegerType(x) in case x is floating point in [0,1]
  // Based on code by Lawrence Kirby: fred at genesis dot demon dot co dot uk

  const auto convert = [](const auto arg) {
    IntegerType        h{ 0 };
    const auto * const p = reinterpret_cast<const unsigned char *>(&arg);

    for (size_t i = 0; i < sizeof(arg); ++i)
    {
      h *= UCHAR_MAX + 2U;
      h += p[i];
    }
    return h;
  };

  const IntegerType h1 = convert(t);
  const IntegerType h2 = convert(c);
  return (h1 + m_PimplGlobals->m_StaticDiffer++) ^ h2;
}

MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::GetNextSeed()
{
  itkInitGlobalsMacro(PimplGlobals);
  return GetInstance()->m_Seed + m_PimplGlobals->m_StaticDiffer++;
}


void
MersenneTwisterRandomVariateGenerator::Initialize(const IntegerType seed)
{
  // This is a public member function, so it must lock the mutex of the instance.
  const std::lock_guard<std::mutex> lockGuard(m_InstanceMutex);
  this->InitializeWithoutMutexLocking(seed);
}

void
MersenneTwisterRandomVariateGenerator::InitializeWithoutMutexLocking(const IntegerType seed)
{
  m_Seed = seed;
  // Initialize generator state with seed
  // See Knuth TAOCP Vol 2, 3rd Ed, p.106 for multiplier.
  // In previous versions, most significant bits (MSBs) of the seed affect
  // only MSBs of the state array.  Modified 9 Jan 2002 by Makoto Matsumoto.
  IntegerType *       s = m_State;
  const IntegerType * r = m_State;

  *s++ = seed;
  for (IntegerType i = 1; i < StateVectorLength; ++i)
  {
    *s++ = uint32_t{ (uint32_t{ 1812433253 } * (*r ^ (*r >> 30)) + i) };
    ++r;
  }
  reload();
}

void
MersenneTwisterRandomVariateGenerator::reload()
{
  // Generate N new values in state
  // Made clearer and faster by Matthew Bellew
  // matthew dot bellew at home dot com

  // Period parameter
  static constexpr unsigned int M = 397;

  // get rid of VS warning
  constexpr auto index = int{ M } - int{ StateVectorLength };

  IntegerType * p = m_State;

  for (int i = StateVectorLength - M; i--; ++p)
  {
    *p = twist(p[M], p[0], p[1]);
  }
  for (int i = M; --i; ++p)
  {
    *p = twist(p[index], p[0], p[1]);
  }
  *p = twist(p[index], p[0], m_State[0]);

  m_Left = StateVectorLength;
  m_PNext = m_State;
}


/** Get an integer variate in [0, 2^32-1] */
MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::GetIntegerVariate()
{
  if (m_Left == 0)
  {
    reload();
  }
  --m_Left;

  IntegerType s1 = *m_PNext++;
  s1 ^= (s1 >> 11);
  s1 ^= (s1 << 7) & 0x9d2c5680;
  s1 ^= (s1 << 15) & 0xefc60000;
  return (s1 ^ (s1 >> 18));
}


MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator::GetIntegerVariate(const IntegerType n)
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
  } while (i > n);

  return i;
}


void
MersenneTwisterRandomVariateGenerator::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  // Print state vector contents
  os << indent << "State vector: " << m_State << std::endl;
  os << indent;
  for (const IntegerType stateValue : m_State)
  {
    os << stateValue << '\t';
  }
  os << std::endl;

  // Print next value to be gotten from state
  os << indent << "Next value to be gotten from state: " << m_PNext << std::endl;

  // Number of values left before reload
  os << indent << "Values left before next reload: " << m_Left << std::endl;
}

} // end namespace Statistics
} // end namespace itk
