/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSample_txx
#define __itkListSample_txx

#include "itkListSample.h"

namespace itk
{
namespace Statistics
{
template< class TMeasurementVector >
ListSample< TMeasurementVector >
::ListSample()
{}

template< class TMeasurementVector >
void
ListSample< TMeasurementVector >
::Resize(InstanceIdentifier newsize)
{
  this->m_InternalContainer.resize(newsize);
}

template< class TMeasurementVector >
void
ListSample< TMeasurementVector >
::Clear()
{
  this->m_InternalContainer.clear();
}

template< class TMeasurementVector >
void
ListSample< TMeasurementVector >
::PushBack(const MeasurementVectorType & mv)
{
  if ( this->GetMeasurementVectorSize() != MeasurementVectorTraits::GetLength(mv) )
    {
    itkExceptionMacro("MeasurementVector instance doesn't match MeasurementVectorSize");
    }
  this->m_InternalContainer.push_back(mv);
}

template< class TMeasurementVector >
typename ListSample< TMeasurementVector >::InstanceIdentifier
ListSample< TMeasurementVector >
::Size() const
{
  return static_cast< InstanceIdentifier >(
           this->m_InternalContainer.size() );
}

template< class TMeasurementVector >
typename ListSample< TMeasurementVector >::TotalAbsoluteFrequencyType
ListSample< TMeasurementVector >
::GetTotalFrequency() const
{
  // Since the entries are unique, the total
  // frequency is equal to the numbe of entries.
  return this->Size();
}

template< class TMeasurementVector >
const typename ListSample< TMeasurementVector >::MeasurementVectorType &
ListSample< TMeasurementVector >
::GetMeasurementVector(InstanceIdentifier instanceId) const
{
  if ( instanceId < m_InternalContainer.size() )
    {
    return m_InternalContainer[instanceId];
    }
  itkExceptionMacro("MeasurementVector " << instanceId << " does not exist");
}

template< class TMeasurementVector >
void
ListSample< TMeasurementVector >
::SetMeasurement(InstanceIdentifier instanceId,
                 unsigned int dim,
                 const MeasurementType & value)
{
  if ( instanceId < m_InternalContainer.size() )
    {
    m_InternalContainer[instanceId][dim] = value;
    }
}

template< class TMeasurementVector >
void
ListSample< TMeasurementVector >
::SetMeasurementVector(InstanceIdentifier instanceId,
                       const MeasurementVectorType & mv)
{
  if ( instanceId < m_InternalContainer.size() )
    {
    m_InternalContainer[instanceId] = mv;
    }
}

template< class TMeasurementVector >
typename ListSample< TMeasurementVector >::AbsoluteFrequencyType
ListSample< TMeasurementVector >
::GetFrequency(InstanceIdentifier instanceId) const
{
  if ( instanceId < m_InternalContainer.size() )
    {
    return 1;
    }
  else
    {
    return 0;
    }
}

template< class TMeasurementVector >
void
ListSample< TMeasurementVector >
::Graft(const DataObject *thatObject)
{
  this->Superclass::Graft(thatObject);

  const Self *thatConst = dynamic_cast< const Self * >( thatObject );
  if ( thatConst )
    {
    Self *that = const_cast< Self * >( thatConst );
    this->m_InternalContainer = that->m_InternalContainer;
    }
}

template< class TMeasurementVector >
void
ListSample< TMeasurementVector >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Internal Data Container: "
     << &m_InternalContainer << std::endl;
  os << indent << "Number of samples: "
     << this->m_InternalContainer.size() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
