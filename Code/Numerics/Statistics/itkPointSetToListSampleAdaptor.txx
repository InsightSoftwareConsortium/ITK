/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkPointSetToListSampleAdaptor.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSetToListSampleAdaptor_txx
#define __itkPointSetToListSampleAdaptor_txx

#include "itkPointSetToListSampleAdaptor.h"

namespace itk
{
namespace Statistics
{
template< class TPointSet >
PointSetToListSampleAdaptor< TPointSet >
::PointSetToListSampleAdaptor()
{
  m_PointSet = 0;
}

template< class TPointSet >
void
PointSetToListSampleAdaptor< TPointSet >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "PointSet: ";
  if ( m_PointSet.IsNotNull() )
    {
    os << m_PointSet << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }
}

template< class TPointSet >
void
PointSetToListSampleAdaptor< TPointSet >
::SetPointSet(const TPointSet *pointSet)
{
  m_PointSet = pointSet;
  m_PointsContainer = pointSet->GetPoints();

  this->Modified();
}

template< class TPointSet >
const TPointSet *
PointSetToListSampleAdaptor< TPointSet >
::GetPointSet()
{
  if ( m_PointSet.IsNull() )
    {
    itkExceptionMacro("Point set has not been set yet");
    }

  return m_PointSet.GetPointer();
}

/** returns the number of measurement vectors in this container*/
template< class TPointSet >
typename PointSetToListSampleAdaptor< TPointSet >::InstanceIdentifier
PointSetToListSampleAdaptor< TPointSet >
::Size() const
{
  if ( m_PointSet.IsNull() )
    {
    itkExceptionMacro("Point set has not been set yet");
    }

  return m_PointsContainer->Size();
}

template< class TPointSet >
inline const typename PointSetToListSampleAdaptor< TPointSet >::MeasurementVectorType &
PointSetToListSampleAdaptor< TPointSet >
::GetMeasurementVector(InstanceIdentifier identifier) const
{
  if ( m_PointSet.IsNull() )
    {
    itkExceptionMacro("Point set has not been set yet");
    }

  m_PointSet->GetPoint(identifier, &m_TempPoint);
  return ( MeasurementVectorType & )m_TempPoint;
}

template< class TPointSet >
inline typename PointSetToListSampleAdaptor< TPointSet >::AbsoluteFrequencyType
PointSetToListSampleAdaptor< TPointSet >
::GetFrequency(InstanceIdentifier) const
{
  if ( m_PointSet.IsNull() )
    {
    itkExceptionMacro("Point set has not been set yet");
    }

  return 1;
}

template< class TPointSet >
typename PointSetToListSampleAdaptor< TPointSet >::TotalAbsoluteFrequencyType
PointSetToListSampleAdaptor< TPointSet >
::GetTotalFrequency() const
{
  if ( m_PointSet.IsNull() )
    {
    itkExceptionMacro("Point set has not been set yet");
    }

  return this->Size();
}
} // end of namespace Statistics
} // end of namespace itk

#endif
