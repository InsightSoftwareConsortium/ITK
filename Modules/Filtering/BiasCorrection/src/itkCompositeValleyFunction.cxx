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
#include "itkCompositeValleyFunction.h"

namespace itk
{
CompositeValleyFunction
::CompositeValleyFunction(const MeasureArrayType & classMeans,
                          const MeasureArrayType & classSigmas)
{
  unsigned int length = classMeans.size();

  if ( length != classSigmas.size() )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("Arrays of Means and Sigmas have not the same length");
    throw ex;
    }

  if ( length == 0 )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("arrays of Means is empty");
    throw ex;
    }

  for ( unsigned int i = 0; i < length; i++ )
    {
    this->AddNewClass(classMeans[i], classSigmas[i]);
    }

  this->Initialize();
}

CompositeValleyFunction::~CompositeValleyFunction() {}

void CompositeValleyFunction
::Initialize()
{
  SizeValueType i, low, high;

  // build table
  // when using valley-func then the table values run from
  // lowest_my - 10 * sigma to highest_my + 10 * sigma

  low = 0; high = 0;

  SizeValueType noOfClasses = static_cast< SizeValueType >( m_Targets.size() );

  for ( i = 0; i < noOfClasses; i++ )
    {
    if ( m_Targets[i].GetMean() > m_Targets[high].GetMean() )
      {
      high = i;
      }
    if ( m_Targets[i].GetMean() < m_Targets[low].GetMean() )
      {
      low = i;
      }
    }

  m_LowerBound = m_Targets[low].GetMean() - 9.0 * m_Targets[low].GetSigma();
  m_UpperBound = m_Targets[high].GetMean() + 9.0 * m_Targets[high].GetSigma();

  CreateCache(m_LowerBound, m_UpperBound, 1000000);
}

CompositeValleyFunction::MeasureType CompositeValleyFunction::Evaluate(MeasureType x)
{
  MeasureType res = 1;

  for ( unsigned int k = 0; k < m_Targets.size(); k++ )
  {
    res *= valley( ( x - m_Targets[k].GetMean() )
                   / m_Targets[k].GetSigma() );
  }

  return res;
}
} // end of namespace itk
