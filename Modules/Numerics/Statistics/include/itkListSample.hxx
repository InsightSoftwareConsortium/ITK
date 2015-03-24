/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkListSample_hxx
#define itkListSample_hxx

#include "itkListSample.h"

namespace itk
{
namespace Statistics
{
template< typename TMeasurementVector >
ListSample< TMeasurementVector >
::ListSample()
{}

template< typename TMeasurementVector >
void
ListSample< TMeasurementVector >
::Resize(InstanceIdentifier newsize)
{
  this->m_InternalContainer.resize(newsize);
}

template< typename TMeasurementVector >
void
ListSample< TMeasurementVector >
::Clear()
{
  this->m_InternalContainer.clear();
}

template< typename TMeasurementVector >
void
ListSample< TMeasurementVector >
::PushBack(const MeasurementVectorType & mv)
{
  if ( this->GetMeasurementVectorSize() != NumericTraits<MeasurementVectorType>::GetLength(mv) )
    {
    itkExceptionMacro("MeasurementVectorSize: " << this->GetMeasurementVectorSize()
      << " doesn't match input measurement vector length: " << NumericTraits<MeasurementVectorType>::GetLength(mv));
    }
  this->m_InternalContainer.push_back(mv);
}

template< typename TMeasurementVector >
typename ListSample< TMeasurementVector >::InstanceIdentifier
ListSample< TMeasurementVector >
::Size() const
{
  return static_cast< InstanceIdentifier >(
           this->m_InternalContainer.size() );
}

template< typename TMeasurementVector >
typename ListSample< TMeasurementVector >::TotalAbsoluteFrequencyType
ListSample< TMeasurementVector >
::GetTotalFrequency() const
{
  // Since the entries are unique, the total
  // frequency is equal to the numbe of entries.
  return this->Size();
}

template< typename TMeasurementVector >
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

template< typename TMeasurementVector >
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

template< typename TMeasurementVector >
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

template< typename TMeasurementVector >
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

template< typename TMeasurementVector >
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

template< typename TMeasurementVector >
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
