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
#include "itkSparseFrequencyContainer2.h"

namespace itk
{
namespace Statistics
{
SparseFrequencyContainer2
::SparseFrequencyContainer2()
{
  m_TotalFrequency = NumericTraits< TotalAbsoluteFrequencyType >::ZeroValue();
}

void
SparseFrequencyContainer2
::Initialize(SizeValueType)
{
  this->SetToZero();
}

void
SparseFrequencyContainer2
::SetToZero()
{
  typedef FrequencyContainerType::iterator IteratorType;
  IteratorType iter = m_FrequencyContainer.begin();
  IteratorType end  = m_FrequencyContainer.end();
  while ( iter != end )
    {
    iter->second = NumericTraits< AbsoluteFrequencyType >::ZeroValue();
    ++iter;
    }
  m_TotalFrequency = NumericTraits< TotalAbsoluteFrequencyType >::ZeroValue();
}

bool
SparseFrequencyContainer2
::SetFrequency(const InstanceIdentifier id, const AbsoluteFrequencyType value)
{
  // No need to test for bounds because in a map container the
  // element is allocated if the key doesn't exist yet
  AbsoluteFrequencyType frequency = this->GetFrequency(id);

  m_FrequencyContainer[id] = value;
  m_TotalFrequency += ( value - frequency );
  return true;
}

SparseFrequencyContainer2::AbsoluteFrequencyType
SparseFrequencyContainer2
::GetFrequency(const InstanceIdentifier id) const
{
  FrequencyContainerType::const_iterator iter = m_FrequencyContainer.find(id);

  if ( iter != m_FrequencyContainer.end() )
    {
    return iter->second;
    }
  else
    {
    return 0;
    }
}

bool
SparseFrequencyContainer2
::IncreaseFrequency(const InstanceIdentifier id, const AbsoluteFrequencyType value)
{
  // No need to test for bounds because in a map container the
  // element is allocated if the key doesn't exist yet
  AbsoluteFrequencyType frequency = this->GetFrequency(id);

  m_FrequencyContainer[id] = frequency + value;
  m_TotalFrequency += value;
  return true;
}

void
SparseFrequencyContainer2
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end of namespace Statistics
} // end of namespace itk
