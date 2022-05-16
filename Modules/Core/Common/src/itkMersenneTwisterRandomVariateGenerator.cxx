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

/** Private nested class to easily synchronize global variables across static libraries.*/
struct MersenneTwisterGlobals
{
  MersenneTwisterGlobals()
    : m_StaticInstance(nullptr)
    , m_StaticDiffer(0){};
  MersenneTwisterRandomVariateGenerator::Pointer                  m_StaticInstance;
  std::recursive_mutex                                            m_StaticInstanceLock;
  std::atomic<MersenneTwisterRandomVariateGenerator::IntegerType> m_StaticDiffer;
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

  obj->SetSeed(MersenneTwisterRandomVariateGenerator::GetNextSeed());
  return obj;
}

MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator::GetInstance()
{
  itkInitGlobalsMacro(PimplGlobals);
  std::lock_guard<std::recursive_mutex> mutexHolder(m_PimplGlobals->m_StaticInstanceLock);

  if (!m_PimplGlobals->m_StaticInstance)
  {
    m_PimplGlobals->m_StaticInstance = MersenneTwisterRandomVariateGenerator::CreateInstance();
    m_PimplGlobals->m_StaticInstance->SetSeed();
  }

  return m_PimplGlobals->m_StaticInstance;
}

MersenneTwisterRandomVariateGenerator::MersenneTwisterRandomVariateGenerator()
{
  SetSeed(121212);
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
  IntegerType newSeed = GetInstance()->GetSeed();
  {
    newSeed += m_PimplGlobals->m_StaticDiffer++;
  }
  return newSeed;
}

void
MersenneTwisterRandomVariateGenerator::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  // Print state vector contents
  os << indent << "State vector: " << state << std::endl;
  os << indent;
  const IntegerType * s = state;
  int                 i = StateVectorLength;
  for (; i--; os << *s++ << "\t")
  {
  }
  os << std::endl;

  // Print next value to be gotten from state
  os << indent << "Next value to be gotten from state: " << m_PNext << std::endl;

  // Number of values left before reload
  os << indent << "Values left before next reload: " << m_Left << std::endl;
}

} // end namespace Statistics
} // end namespace itk
