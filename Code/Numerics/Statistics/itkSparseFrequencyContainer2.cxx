/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFrequencyContainer2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkSparseFrequencyContainer2.h"

namespace itk
{
namespace Statistics
{
SparseFrequencyContainer2
::SparseFrequencyContainer2()
{
  m_TotalFrequency = NumericTraits< TotalAbsoluteFrequencyType >::Zero;
}

void
SparseFrequencyContainer2
::Initialize(unsigned long)
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
    iter->second = NumericTraits< AbsoluteFrequencyType >::Zero;
    ++iter;
    }
  m_TotalFrequency = NumericTraits< TotalAbsoluteFrequencyType >::Zero;
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
