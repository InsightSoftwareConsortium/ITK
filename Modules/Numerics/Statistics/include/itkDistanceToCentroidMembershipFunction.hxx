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
#ifndef __itkDistanceToCentroidMembershipFunction_hxx
#define __itkDistanceToCentroidMembershipFunction_hxx

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
  m_DistanceMetric = EuclideanDistanceMetric< TVector >::New();
}

template< class TVector >
void
DistanceToCentroidMembershipFunction< TVector >
::SetCentroid(const CentroidType & centroid)
{
  if (centroid != m_DistanceMetric->GetOrigin())
    {
    m_DistanceMetric->SetOrigin(centroid);
    this->Modified();
    }
}

template< class TVector >
void
DistanceToCentroidMembershipFunction< TVector >
::SetMeasurementVectorSize(MeasurementVectorSizeType s)
{
  this->Superclass::SetMeasurementVectorSize(s);
  m_DistanceMetric->SetMeasurementVectorSize(s);
}

template< class TVector >
const typename DistanceToCentroidMembershipFunction< TVector >::CentroidType &
DistanceToCentroidMembershipFunction< TVector >
::GetCentroid() const
{
  return m_DistanceMetric->GetOrigin();
}

template< class TVector >
double
DistanceToCentroidMembershipFunction< TVector >
::Evaluate(const MeasurementVectorType & measurement) const
{
  return m_DistanceMetric->Evaluate(measurement);
}

template< class TVector >
typename DistanceToCentroidMembershipFunction< TVector >::MembershipFunctionPointer
DistanceToCentroidMembershipFunction< TVector >
::Clone() const
{
  Pointer membershipFunction =
    DistanceToCentroidMembershipFunction< TVector >::New();

  membershipFunction->SetMeasurementVectorSize( this->GetMeasurementVectorSize() );
  membershipFunction->SetCentroid( this->GetCentroid() );

  MembershipFunctionPointer sptr = membershipFunction.GetPointer();
  return sptr;
}

template< class TVector >
void
DistanceToCentroidMembershipFunction< TVector >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Distance Metric: " << m_DistanceMetric.GetPointer() << std::endl;
}
} // end namespace Statistics
} // end of namespace itk
#endif
