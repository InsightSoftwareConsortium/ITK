/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFrequencyContainer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  m_TotalFrequency = NumericTraits< FrequencyType >::Zero ;
}

template< class TFrequencyValue >
void
SparseFrequencyContainer< TFrequencyValue >
::Initialize(unsigned long) 
{   
}

template< class TFrequencyValue >
void
SparseFrequencyContainer< TFrequencyValue >
::SetFrequency(const InstanceIdentifier id, const FrequencyType value)
{ 
  FrequencyType frequency = this->GetFrequency(id) ;
  m_FrequencyContainer[id] = value ; 
  m_TotalFrequency += (value - frequency) ;
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
    return 0;
}

template< class TFrequencyValue >
void
SparseFrequencyContainer< TFrequencyValue >
::IncreaseFrequency(const InstanceIdentifier id, const FrequencyType value)
{
  FrequencyType frequency = this->GetFrequency(id) ;
  m_FrequencyContainer[id] = frequency + value ; 
  m_TotalFrequency += value ;
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
