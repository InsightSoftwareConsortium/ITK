/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDenseFrequencyContainer2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDenseFrequencyContainer2.h"

namespace itk
{
namespace Statistics
{
DenseFrequencyContainer2
::DenseFrequencyContainer2()
{
  m_FrequencyContainer = FrequencyContainerType::New();
  m_TotalFrequency = NumericTraits< TotalAbsoluteFrequencyType >::Zero;
}

void
DenseFrequencyContainer2
::Initialize(unsigned long length)
{
  m_FrequencyContainer->Reserve(length);
  this->SetToZero();
}

void
DenseFrequencyContainer2
::SetToZero()
{
  m_FrequencyContainer->Fill(NumericTraits< AbsoluteFrequencyType >::Zero);
  m_TotalFrequency = NumericTraits< TotalAbsoluteFrequencyType >::Zero;
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
    return NumericTraits< AbsoluteFrequencyType >::Zero;
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
