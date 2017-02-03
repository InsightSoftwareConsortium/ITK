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
#include "itkMersenneTwisterRandomVariateGenerator.h"


namespace itk
{
namespace Statistics
{

// Static/Global variables
MersenneTwisterRandomVariateGenerator::Pointer MersenneTwisterRandomVariateGenerator::m_StaticInstance = ITK_NULLPTR;
SimpleFastMutexLock MersenneTwisterRandomVariateGenerator::m_StaticInstanceLock;
MersenneTwisterRandomVariateGenerator::IntegerType MersenneTwisterRandomVariateGenerator::m_StaticDiffer = 0;

MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator
::CreateInstance()
{
  // Try the factory first
  MersenneTwisterRandomVariateGenerator::Pointer obj =
    ObjectFactory< Self >::Create();
  // If the factory did not provide one, then create it here
  if ( !obj )
    {
      obj = new MersenneTwisterRandomVariateGenerator;
      // Remove extra reference from construction.
      obj->UnRegister();
    }
  return obj;
}


MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator
::New()
{
  MersenneTwisterRandomVariateGenerator::Pointer obj =
    MersenneTwisterRandomVariateGenerator::CreateInstance();

  obj->SetSeed ( MersenneTwisterRandomVariateGenerator::GetNextSeed() );
  return obj;
}

MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator
::GetInstance()
{
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_StaticInstanceLock);

  if ( !m_StaticInstance )
    {
    m_StaticInstance  = MersenneTwisterRandomVariateGenerator::CreateInstance();
    m_StaticInstance->SetSeed();
    }

  return m_StaticInstance;
}

MersenneTwisterRandomVariateGenerator
::MersenneTwisterRandomVariateGenerator()
{
  SetSeed (121212);
}

MersenneTwisterRandomVariateGenerator
::~MersenneTwisterRandomVariateGenerator() {}

MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator
::hash(time_t t, clock_t c)
{
  // Get an IntegerType from t and c
  // Better than IntegerType(x) in case x is floating point in [0,1]
  // Based on code by Lawrence Kirby: fred at genesis dot demon dot co dot uk

  IntegerType    h1 = 0;
  unsigned char *p = (unsigned char *)&t;

  const unsigned int sizeOfT = static_cast< unsigned int >( sizeof(t) );
  for ( unsigned int i = 0; i < sizeOfT; ++i )
    {
    h1 *= UCHAR_MAX + 2U;
    h1 += p[i];
    }
  IntegerType h2 = 0;
  p = (unsigned char *)&c;

  const unsigned int sizeOfC = static_cast< unsigned int >( sizeof(c) );
  for ( unsigned int j = 0; j < sizeOfC; ++j )
    {
    h2 *= UCHAR_MAX + 2U;
    h2 += p[j];
    }
  // lock for m_StaticDiffer
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_StaticInstanceLock);
  return ( h1 + m_StaticDiffer++ ) ^ h2;
}

MersenneTwisterRandomVariateGenerator::IntegerType
MersenneTwisterRandomVariateGenerator
::GetNextSeed()
{
  IntegerType newSeed = GetInstance()->GetSeed();
  {
    MutexLockHolder<SimpleFastMutexLock> mutexHolder(m_StaticInstanceLock);
    newSeed += m_StaticDiffer++;
  }
  return newSeed;
}

void
MersenneTwisterRandomVariateGenerator
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  // Print state vector contents
  os << indent << "State vector: " << state << std::endl;
  os << indent;
  const IntegerType *s = state;
  int i = StateVectorLength;
  for (; i--; os << *s++ << "\t" ) {}
  os << std::endl;

  //Print next value to be gotten from state
  os << indent << "Next value to be gotten from state: " << m_PNext << std::endl;

  //Number of values left before reload
  os << indent << "Values left before next reload: " << m_Left << std::endl;
}

}  // end namespace Statistics
}  // end namespace itk
