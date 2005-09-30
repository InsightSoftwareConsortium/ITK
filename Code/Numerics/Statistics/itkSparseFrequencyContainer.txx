/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFrequencyContainer.txx
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

namespace itk{ 
namespace Statistics{

template< class TFrequencyValue >
SparseFrequencyContainer< TFrequencyValue >
::SparseFrequencyContainer()
{
  m_TotalFrequency = NumericTraits< TotalFrequencyType >::Zero ;
}

template< class TFrequencyValue >
void
SparseFrequencyContainer< TFrequencyValue >
::Initialize(unsigned long) 
{   
  this->SetToZero();
}

template< class TFrequencyValue >
void
SparseFrequencyContainer< TFrequencyValue >
::SetToZero() 
{   
  typedef typename FrequencyContainerType::iterator IteratorType;
  IteratorType iter = m_FrequencyContainer.begin();
  IteratorType end  = m_FrequencyContainer.end();
  if ( iter != end )
    {
    iter->second = NumericTraits< FrequencyType >::Zero;
    ++iter;
    }
  m_TotalFrequency = NumericTraits< TotalFrequencyType >::Zero ;
}

template< class TFrequencyValue >
bool
SparseFrequencyContainer< TFrequencyValue >
::SetFrequency(const InstanceIdentifier id, const FrequencyType value)
{ 
  // No need to test for bounds because in a map container the
  // element is allocated if the key doesn't exist yet
  FrequencyType frequency = this->GetFrequency(id) ;
  m_FrequencyContainer[id] = value ; 
  m_TotalFrequency += (value - frequency) ;
  return true;
}

template< class TFrequencyValue >
typename SparseFrequencyContainer< TFrequencyValue >::FrequencyType
SparseFrequencyContainer< TFrequencyValue >
::GetFrequency(const InstanceIdentifier id) const
{
  typename FrequencyContainerType::const_iterator iter = 
    m_FrequencyContainer.find(id) ;
  if ( iter != m_FrequencyContainer.end() )
    {
    return iter->second ;
    }
  else
    {
    return 0;
    }
}

template< class TFrequencyValue >
bool
SparseFrequencyContainer< TFrequencyValue >
::IncreaseFrequency(const InstanceIdentifier id, const FrequencyType value)
{
  // No need to test for bounds because in a map container the
  // element is allocated if the key doesn't exist yet
  FrequencyType frequency = this->GetFrequency(id) ;
  m_FrequencyContainer[id] = frequency + value ; 
  m_TotalFrequency += value ;
  return true;
}

template< class TFrequencyValue >
void
SparseFrequencyContainer< TFrequencyValue >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end of namespace Statistics
} // end of namespace itk 

#endif
