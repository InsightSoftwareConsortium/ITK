/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSample_txx
#define __itkListSample_txx

#include "itkListSample.h"

namespace itk{ 
namespace Statistics{

template< class TMeasurementVector >
ListSample< TMeasurementVector >
::ListSample()
{
}


template< class TMeasurementVector >
typename ListSample< TMeasurementVector >::MeasurementVectorType& 
ListSample< TMeasurementVector >
::GetMeasurementVector(const InstanceIdentifier &id)
{
  if ( id < m_InternalContainer.size() )
    {
    return m_InternalContainer[id] ;
    }
  throw ExceptionObject(__FILE__,__LINE__);  
}

template< class TMeasurementVector >
void 
ListSample< TMeasurementVector >
::SetMeasurement(const InstanceIdentifier &id, 
                 const unsigned int &dim,
                 const MeasurementType &value)
{
  if ( id < m_InternalContainer.size() )
    {
    m_InternalContainer[id][dim] = value ;
    }
}

template< class TMeasurementVector >
void
ListSample< TMeasurementVector >
::SetMeasurementVector(const InstanceIdentifier &id, 
                       const MeasurementVectorType &mv)
{
  if ( id < m_InternalContainer.size() )
    {
    m_InternalContainer[id] = mv ;
    }
}

template< class TMeasurementVector >
typename ListSample< TMeasurementVector >::FrequencyType 
ListSample< TMeasurementVector >
::GetFrequency(const InstanceIdentifier &id) const
{
  if ( id < m_InternalContainer.size() )
    {
    return 1.0 ;
    }
  else
    {
    return 0.0 ;
    }
}

template< class TMeasurementVector >
void 
ListSample< TMeasurementVector >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Internal Data Container: "
     << &m_InternalContainer << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk 

#endif
