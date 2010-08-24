/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceToCentroidMembershipFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDistanceToCentroidMembershipFunction_txx
#define __itkDistanceToCentroidMembershipFunction_txx

#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkEuclideanDistanceMetric.h"

namespace itk
{
namespace Statistics
{
template< class TVector >
DistanceToCentroidMembershipFunction< TVector >
::DistanceToCentroidMembershipFunction()
{
  // Initialize by default to an Euclidean distance. This default can be
  // changed by calling SetDistanceMetric().
  this->m_DistanceMetric = EuclideanDistanceMetric< TVector >::New();
}

template< class TVector >
void
DistanceToCentroidMembershipFunction< TVector >
::SetCentroid(const CentroidType & centroid)
{
  this->m_DistanceMetric->SetOrigin(centroid);
  this->Modified();
}

template< class TVector >
void
DistanceToCentroidMembershipFunction< TVector >
::SetMeasurementVectorSize(MeasurementVectorSizeType s)
{
  this->Superclass::SetMeasurementVectorSize(s);
  this->m_DistanceMetric->SetMeasurementVectorSize(s);
  this->Modified();
}

template< class TVector >
const typename DistanceToCentroidMembershipFunction< TVector >::CentroidType &
DistanceToCentroidMembershipFunction< TVector >
::GetCentroid() const
{
  return this->m_DistanceMetric->GetOrigin();
}

template< class TVector >
double
DistanceToCentroidMembershipFunction< TVector >
::Evaluate(const MeasurementVectorType & measurement) const
{
  return this->m_DistanceMetric->Evaluate(measurement);
}

template< class TVector >
typename DistanceToCentroidMembershipFunction< TVector >::Pointer
DistanceToCentroidMembershipFunction< TVector >
::Clone()
{
  Pointer membershipFunction =
    DistanceToCentroidMembershipFunction< TVector >::New();

  membershipFunction->SetMeasurementVectorSize( this->GetMeasurementVectorSize() );
  membershipFunction->SetCentroid( this->GetCentroid() );

  return membershipFunction;
}

template< class TVector >
void
DistanceToCentroidMembershipFunction< TVector >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "Distance Metric: " << this->m_DistanceMetric.GetPointer() << std::endl;
}
} // end namespace Statistics
} // end of namespace itk
#endif
