/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSubsample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSubsample_txx
#define __itkSubsample_txx

#include "itkObject.h"

namespace itk{ 
namespace Statistics{

template< class TSample >
Subsample< TSample >
::Subsample()
{
  m_TotalFrequency = NumericTraits< FrequencyType >::Zero ;
}

template< class TSample >
inline typename Subsample< TSample >::MeasurementVectorType
Subsample< TSample >
::GetMeasurementVectorByIndex(int index)
{
  return m_Sample->GetMeasurementVector(m_IdHolder[index]) ;
}

template< class TSample >
inline typename Subsample< TSample >::FrequencyType
Subsample< TSample >
::GetFrequencyByIndex(int index)
{
  return m_Sample->GetFrequency(m_IdHolder[index]) ;
}

template< class TSample >
inline typename Subsample< TSample >::InstanceIdentifier
Subsample< TSample >
::GetInstanceIdentifier(int index)
{
  return m_IdHolder[index] ;
}

template< class TSample >
inline void
Subsample< TSample >
::Swap(int index1, int index2)
{
  InstanceIdentifier temp = m_IdHolder[index1] ;
  m_IdHolder[index1] = m_IdHolder[index2] ;
  m_IdHolder[index2] = temp ;
}

template< class TSample >
void
Subsample< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Sample: " << m_Sample << std::endl ;
  os << indent << "InstanceIdentifierHolder : " << &m_IdHolder << std::endl ;
}
} // end of namespace Statistics 
} // end of namespace itk

#endif







