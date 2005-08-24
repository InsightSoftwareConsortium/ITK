/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFrequencyContainer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSparseFrequencyContainer_txx
#define _itkSparseFrequencyContainer_txx

#include "itkSparseFrequencyContainer.h"
#include "itkNumericTraits.h"

namespace itk{ 
namespace Statistics{

SparseFrequencyContainer
::SparseFrequencyContainer()
{
  m_TotalFrequency = NumericTraits< FrequencyType >::Zero ;
}

void
SparseFrequencyContainer
::Initialize(unsigned long) 
{   
  this->SetToZero();
}

void
SparseFrequencyContainer
::SetToZero() 
{   
  typedef FrequencyContainerType::iterator IteratorType;
  IteratorType iter = m_FrequencyContainer.begin();
  IteratorType end  = m_FrequencyContainer.end();
  if ( iter != end )
    {
    iter->second = NumericTraits< FrequencyType >::Zero;
    ++iter;
    }
  m_TotalFrequency = NumericTraits< FrequencyType >::Zero ;
}

bool
SparseFrequencyContainer
::SetFrequency(const InstanceIdentifier id, const FrequencyType value)
{ 
  // No need to test for bounds because in a map container the
  // element is allocated if the key doesn't exist yet
  FrequencyType frequency = this->GetFrequency(id) ;
  m_FrequencyContainer[id] = value ; 
  m_TotalFrequency += (value - frequency) ;
  return true;
}

SparseFrequencyContainer::FrequencyType
SparseFrequencyContainer
::GetFrequency(const InstanceIdentifier id) const
{
  FrequencyContainerType::const_iterator iter = m_FrequencyContainer.find(id) ;
  if ( iter != m_FrequencyContainer.end() )
    {
    return iter->second ;
    }
  else
    {
    return 0;
    }
}

bool
SparseFrequencyContainer
::IncreaseFrequency(const InstanceIdentifier id, const FrequencyType value)
{
  // No need to test for bounds because in a map container the
  // element is allocated if the key doesn't exist yet
  FrequencyType frequency = this->GetFrequency(id) ;
  m_FrequencyContainer[id] = frequency + value ; 
  m_TotalFrequency += value ;
  return true;
}

void
SparseFrequencyContainer
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end of namespace Statistics
} // end of namespace itk 

#endif
