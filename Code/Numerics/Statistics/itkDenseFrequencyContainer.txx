/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDenseFrequencyContainer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkDenseFrequencyContainer_txx
#define _itkDenseFrequencyContainer_txx

#include "itkDenseFrequencyContainer.h"

namespace itk{ 
namespace Statistics{

template< class TFrequencyValue >
DenseFrequencyContainer< TFrequencyValue >
::DenseFrequencyContainer()
{
  m_FrequencyContainer = FrequencyContainerType::New() ;
  m_TotalFrequency = NumericTraits< TotalFrequencyType >::Zero ;
} 

template< class TFrequencyValue >
void
DenseFrequencyContainer< TFrequencyValue >
::Initialize(unsigned long length) 
{   
  m_FrequencyContainer->Reserve(length) ;
  this->SetToZero();
}

template< class TFrequencyValue >
void
DenseFrequencyContainer< TFrequencyValue >
::SetToZero() 
{   
  m_FrequencyContainer->Fill( NumericTraits< FrequencyType >::Zero );
  m_TotalFrequency = NumericTraits< TotalFrequencyType >::Zero ;
}

template< class TFrequencyValue >
bool
DenseFrequencyContainer< TFrequencyValue >
::SetFrequency(const InstanceIdentifier id, const FrequencyType value)
{ 
  if( id >= m_FrequencyContainer->Size() )
    {
    return false;
    }
  FrequencyType frequency = this->GetFrequency(id) ;
  (*m_FrequencyContainer)[id] = value ; 
  m_TotalFrequency += (value - frequency) ;
  return true;
}

template< class TFrequencyValue >
typename DenseFrequencyContainer< TFrequencyValue >::FrequencyType
DenseFrequencyContainer< TFrequencyValue >
::GetFrequency(const InstanceIdentifier id) const
{
  if( id >= m_FrequencyContainer->Size() )
    {
    return NumericTraits< FrequencyType >::Zero;
    }
  return (*m_FrequencyContainer)[id];
}

template< class TFrequencyValue >
bool
DenseFrequencyContainer< TFrequencyValue >
::IncreaseFrequency(const InstanceIdentifier id, const FrequencyType value)
{
  if( id >= m_FrequencyContainer->Size() )
    {
    return false;
    }
  FrequencyType frequency = this->GetFrequency(id) ;
  (*m_FrequencyContainer)[id] = frequency + value ; 
  m_TotalFrequency += value ;
  return true;
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
