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
#ifndef __itkFiniteCylinderSpatialFunction_hxx
#define __itkFiniteCylinderSpatialFunction_hxx

#include "itkFiniteCylinderSpatialFunction.h"
#include <math.h>

namespace itk
{
template< unsigned int VDimension, typename TInput >
FiniteCylinderSpatialFunction< VDimension, TInput >
::FiniteCylinderSpatialFunction()
{
  m_Orientation.Fill(1.0f);
  m_AxisLength = 1.0f; // Length of cylinder axis.
  m_Radius = 1.0f;     // Radius of cylinder.
  m_Center.Fill(0.0f); // Origin of cylinder
}

template< unsigned int VDimension, typename TInput >
FiniteCylinderSpatialFunction< VDimension, TInput >
::~FiniteCylinderSpatialFunction()
{}

template< unsigned int VDimension, typename TInput >
typename FiniteCylinderSpatialFunction< VDimension, TInput >::OutputType
FiniteCylinderSpatialFunction< VDimension, TInput >
::Evaluate(const InputType & position) const
{
  double halfAxisLength = 0.5 * m_AxisLength;

  Vector< double, VDimension > orientationVector;
  Vector< double, VDimension > pointVector;
  Vector< double, VDimension > medialAxisVector;

  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    pointVector[i] = position[i] - m_Center[i];
    }

  //take square root to normalize the orientation vector
  medialAxisVector[0] = vcl_sqrt(m_Orientation[0]);
  medialAxisVector[1] = vcl_sqrt(m_Orientation[1]);
  medialAxisVector[2] = vcl_sqrt(m_Orientation[2]);

  //if length_test is less than the length of the cylinder (half actually,
  // because its length from the center), than
  //the point is within the length of the cylinder along the medial axis
  const double distanceFromCenter = dot_product( medialAxisVector.GetVnlVector(), pointVector.GetVnlVector() );

  if ( vcl_fabs(distanceFromCenter) <= ( halfAxisLength )
       && m_Radius >= vcl_sqrt( vcl_pow(pointVector.GetVnlVector().magnitude(), 2.0) - vcl_pow(distanceFromCenter, 2.0) ) )
    {
    return 1;
    }
  else { return 0; }
}

template< unsigned int VDimension, typename TInput >
void FiniteCylinderSpatialFunction< VDimension, TInput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  unsigned int i;

  Superclass::PrintSelf(os, indent);

  os << indent << "Lengths of Axis: " << m_AxisLength << std::endl;
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "Origin of Cylinder: " << m_Center << std::endl;
  os << indent << "Orientation: " << std::endl;
  for ( i = 0; i < VDimension; i++ )
    {
    os << indent << indent <<  m_Orientation[i] << " ";
    }
  os << std::endl;
}
} // end namespace itk

#endif
