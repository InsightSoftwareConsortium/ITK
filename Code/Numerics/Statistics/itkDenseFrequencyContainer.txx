/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDenseFrequencyContainer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDenseFrequencyContainer_txx
#define _itkDenseFrequencyContainer_txx

#include "itkDenseFrequencyContainer.h"
#include "itkNumericTraits.h"

namespace itk{ 
namespace Statistics{

template< class TFrequencyValue >
DenseFrequencyContainer< TFrequencyValue >
::DenseFrequencyContainer()
{
  m_FrequencyContainer = FrequencyContainerType::New() ;
  m_TotalFrequency = NumericTraits< FrequencyType >::Zero ;
} 

template< class TFrequencyValue >
void
DenseFrequencyContainer< TFrequencyValue >
::Initialize(unsigned long length) 
{   
  m_FrequencyContainer->Reserve(length) ;
}

template< class TFrequencyValue >
void
DenseFrequencyContainer< TFrequencyValue >
::SetFrequency(const InstanceIdentifier id, const FrequencyType value)
{ 
  FrequencyType frequency = this->GetFrequency(id) ;
  (*m_FrequencyContainer)[id] = value ; 
  m_TotalFrequency += (value - frequency) ;
}

template< class TFrequencyValue >
typename DenseFrequencyContainer< TFrequencyValue >::FrequencyType
DenseFrequencyContainer< TFrequencyValue >
::GetFrequency(const InstanceIdentifier id) const
{
  return (*m_FrequencyContainer)[id];
}

template< class TFrequencyValue >
void
DenseFrequencyContainer< TFrequencyValue >
::IncreaseFrequency(const InstanceIdentifier id, const FrequencyType value)
{
  FrequencyType frequency = this->GetFrequency(id) ;
  (*m_FrequencyContainer)[id] = frequency + value ; 
  m_TotalFrequency += value ;
}

template< class TFrequencyValue >
void
DenseFrequencyContainer< TFrequencyValue >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end of namespace Statistics
} // end of namespace itk 

#endif
