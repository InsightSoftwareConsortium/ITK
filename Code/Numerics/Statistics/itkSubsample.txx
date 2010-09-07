/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSubsample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSubsample_txx
#define __itkSubsample_txx

#include "itkObject.h"

namespace itk
{
namespace Statistics
{
template< class TSample >
Subsample< TSample >
::Subsample()
{
  m_Sample = 0;
  m_TotalFrequency = NumericTraits< AbsoluteFrequencyType >::Zero;
  m_ActiveDimension = 0;
}

template< class TSample >
void
Subsample< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Sample: ";
  if ( m_Sample != 0 )
    {
    os << m_Sample << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }

  os << indent << "TotalFrequency: " << m_TotalFrequency << std::endl;
  os << indent << "ActiveDimension: " << m_ActiveDimension << std::endl;
  os << indent << "InstanceIdentifierHolder : " << &m_IdHolder << std::endl;
}

template< class TSample >
void
Subsample< TSample >
::SetSample(const TSample *sample)
{
  m_Sample = sample;
  this->SetMeasurementVectorSize( m_Sample->GetMeasurementVectorSize() );
  this->Modified();
}

template< class TSample >
const TSample *
Subsample< TSample >
::GetSample() const
{
  return m_Sample;
}

template< class TSample >
void
Subsample< TSample >
::InitializeWithAllInstances()
{
  m_IdHolder.resize( m_Sample->Size() );
  typename InstanceIdentifierHolder::iterator idIter = m_IdHolder.begin();
  typename TSample::ConstIterator iter = m_Sample->Begin();
  typename TSample::ConstIterator last = m_Sample->End();
  m_TotalFrequency = NumericTraits< AbsoluteFrequencyType >::Zero;
  while ( iter != last )
    {
    *idIter++ = iter.GetInstanceIdentifier();
    m_TotalFrequency += iter.GetFrequency();
    ++iter;
    }
  this->Modified();
}

template< class TSample >
void
Subsample< TSample >
::AddInstance(InstanceIdentifier id)
{
  if ( id > m_Sample->Size() )
    {
    itkExceptionMacro("MeasurementVector " << id << " does not exist in the Sample");
    }

  m_IdHolder.push_back(id);
  m_TotalFrequency += m_Sample->GetFrequency(id);
  this->Modified();
}

template< class TSample >
typename Subsample< TSample >::InstanceIdentifier
Subsample< TSample >
::Size() const
{
  return static_cast< unsigned int >( m_IdHolder.size() );
}

template< class TSample >
void
Subsample< TSample >
::Clear()
{
  m_IdHolder.clear();
  m_TotalFrequency = NumericTraits< AbsoluteFrequencyType >::Zero;
  this->Modified();
}

template< class TSample >
const typename Subsample< TSample >::MeasurementVectorType &
Subsample< TSample >
::GetMeasurementVector(InstanceIdentifier id) const
{
  if ( id >= m_IdHolder.size() )
    {
    itkExceptionMacro("MeasurementVector " << id << " does not exist");
    }

  // translate the id to its Sample container id
  InstanceIdentifier idInTheSample = m_IdHolder[id];
  return m_Sample->GetMeasurementVector(idInTheSample);
}

template< class TSample >
inline typename Subsample< TSample >::AbsoluteFrequencyType
Subsample< TSample >
::GetFrequency(InstanceIdentifier id) const
{
  if ( id >= m_IdHolder.size() )
    {
    itkExceptionMacro("MeasurementVector " << id << " does not exist");
    }

  // translate the id to its Sample container id
  InstanceIdentifier idInTheSample = m_IdHolder[id];
  return m_Sample->GetFrequency(idInTheSample);
}

template< class TSample >
inline typename Subsample< TSample >::TotalAbsoluteFrequencyType
Subsample< TSample >
::GetTotalFrequency() const
{
  return m_TotalFrequency;
}

template< class TSample >
inline void
Subsample< TSample >
::Swap(unsigned int index1, unsigned int index2)
{
  if ( index1 >= m_IdHolder.size()
       || index2 >= m_IdHolder.size() )
    {
    itkExceptionMacro("Index out of range");
    }

  InstanceIdentifier temp = m_IdHolder[index1];
  m_IdHolder[index1] = m_IdHolder[index2];
  m_IdHolder[index2] = temp;
  this->Modified();
}

template< class TSample >
inline const typename Subsample< TSample >::MeasurementVectorType &
Subsample< TSample >
::GetMeasurementVectorByIndex(unsigned int index) const
{
  if ( index >= m_IdHolder.size() )
    {
    itkExceptionMacro("Index out of range");
    }
  return m_Sample->GetMeasurementVector(m_IdHolder[index]);
}

template< class TSample >
inline typename Subsample< TSample >::AbsoluteFrequencyType
Subsample< TSample >
::GetFrequencyByIndex(unsigned int index) const
{
  if ( index >= m_IdHolder.size() )
    {
    itkExceptionMacro("Index out of range");
    }

  return m_Sample->GetFrequency(m_IdHolder[index]);
}

template< class TSample >
typename Subsample< TSample >::InstanceIdentifier
Subsample< TSample >
::GetInstanceIdentifier(unsigned int index)
{
  if ( index >= m_IdHolder.size() )
    {
    itkExceptionMacro("Index out of range");
    }
  return m_IdHolder[index];
}

template< class TSample >
void
Subsample< TSample >
::Graft(const DataObject *thatObject)
{
  this->Superclass::Graft(thatObject);

  // Most of what follows is really a deep copy, rather than grafting of
  // output. Wish it were managed by pointers to bulk data. Sigh !

  const Self *thatConst = dynamic_cast< const Self * >( thatObject );
  if ( thatConst )
    {
    Self *that = const_cast< Self * >( thatConst );
    this->SetSample( that->GetSample() );
    this->m_IdHolder          = that->m_IdHolder;
    this->m_ActiveDimension   = that->m_ActiveDimension;
    this->m_TotalFrequency    = that->m_TotalFrequency;
    }
}
} // end of namespace Statistics
} // end of namespace itk

#endif
