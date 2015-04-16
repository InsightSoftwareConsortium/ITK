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
#include "itkCacheableScalarFunction.h"

namespace itk
{
CacheableScalarFunction
::CacheableScalarFunction() :
  m_NumberOfSamples(0),
  m_CacheTable(0),
  m_CacheUpperBound(0.0),
  m_CacheLowerBound(0.0),
  m_TableInc(0.0),
  m_CacheAvailable(false)
{
}

CacheableScalarFunction::~CacheableScalarFunction() {}

void
CacheableScalarFunction
::CreateCache(double lowerBound, double upperBound, SizeValueType sampleSize)
{
  m_NumberOfSamples = sampleSize;
  m_CacheLowerBound = lowerBound;
  m_CacheUpperBound = upperBound;

  SizeValueType i;
  MeasureType   d;

  m_CacheTable = MeasureArrayType(m_NumberOfSamples);

  m_TableInc =
    static_cast< MeasureType >( ( m_CacheUpperBound - m_CacheLowerBound )
                                / double(m_NumberOfSamples - 1) );

  d = static_cast< MeasureType >( m_CacheLowerBound );
  for ( i = 0; i < m_NumberOfSamples; i++ )
    {
    m_CacheTable[i] = Evaluate(d);
    d += m_TableInc;
    }

  m_CacheAvailable = true;
}

CacheableScalarFunction::MeasureType CacheableScalarFunction::Evaluate(MeasureType x)
{ return x; }
} // end of namespace itk
