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
#include "itkDenseFrequencyContainer2.h"

namespace itk
{
namespace Statistics
{
DenseFrequencyContainer2
::DenseFrequencyContainer2()
{
  m_FrequencyContainer = FrequencyContainerType::New();
  m_TotalFrequency = NumericTraits< TotalAbsoluteFrequencyType >::ZeroValue();
}

void
DenseFrequencyContainer2
::Initialize(SizeValueType length)
{
  m_FrequencyContainer->Reserve(length);
  this->SetToZero();
}

void
DenseFrequencyContainer2
::SetToZero()
{
  m_FrequencyContainer->Fill(NumericTraits< AbsoluteFrequencyType >::ZeroValue());
  m_TotalFrequency = NumericTraits< TotalAbsoluteFrequencyType >::ZeroValue();
}

bool
DenseFrequencyContainer2
::SetFrequency(const InstanceIdentifier id, const AbsoluteFrequencyType value)
{
  if ( id >= m_FrequencyContainer->Size() )
    {
    return false;
    }
  AbsoluteFrequencyType frequency = this->GetFrequency(id);
  ( *m_FrequencyContainer )[id] = value;
  m_TotalFrequency += ( value - frequency );
  return true;
}

DenseFrequencyContainer2::AbsoluteFrequencyType
DenseFrequencyContainer2
::GetFrequency(const InstanceIdentifier id) const
{
  if ( id >= m_FrequencyContainer->Size() )
    {
    return NumericTraits< AbsoluteFrequencyType >::ZeroValue();
    }
  return ( *m_FrequencyContainer )[id];
}

bool
DenseFrequencyContainer2
::IncreaseFrequency(const InstanceIdentifier id, const AbsoluteFrequencyType value)
{
  if ( id >= m_FrequencyContainer->Size() )
    {
    return false;
    }
  AbsoluteFrequencyType frequency = this->GetFrequency(id);
  ( *m_FrequencyContainer )[id] = frequency + value;
  m_TotalFrequency += value;
  return true;
}

void
DenseFrequencyContainer2
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end of namespace Statistics
} // end of namespace itk
