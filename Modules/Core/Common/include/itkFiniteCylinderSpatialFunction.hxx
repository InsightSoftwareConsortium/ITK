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
#ifndef itkFiniteCylinderSpatialFunction_hxx
#define itkFiniteCylinderSpatialFunction_hxx

#include "itkFiniteCylinderSpatialFunction.h"
#include <cmath>

namespace itk
{
template< unsigned int VDimension, typename TInput >
FiniteCylinderSpatialFunction< VDimension, TInput >
::FiniteCylinderSpatialFunction()
{
  // a normalized {1,1,...1} vector is
  // { 1.0 / sqrt( VDmim ), ... }
  const double orientationVal = 1.0 / std::sqrt(static_cast<double>(VDimension));
  m_Orientation.Fill(orientationVal);
  m_NormalizedOrientation.Fill(orientationVal);
  m_AxisLength = 1.0f; // Length of cylinder axis.
  m_Radius = 1.0f;     // Radius of cylinder.
  m_Center.Fill(0.0f); // Origin of cylinder}
}

template< unsigned int VDimension, typename TInput >
FiniteCylinderSpatialFunction< VDimension, TInput >
::~FiniteCylinderSpatialFunction()
{}

template< unsigned int VDimension, typename TInput >
void
FiniteCylinderSpatialFunction< VDimension, TInput >
::SetOrientation(const InputType _Orientation)
{
  itkDebugMacro("setting Orientation to " << _Orientation);
  if(this->m_Orientation != _Orientation)
    {
    this->m_Orientation = _Orientation;
    //
    // save normalizedOrientation, so it doesn't need to be recomputed
    // in every call of Evaluate.
    double norm = 0.0;
    for(unsigned int i = 0; i < VDimension; ++i)
      {
      norm += this->m_Orientation[i] * this->m_Orientation[i];
      }
    norm = std::sqrt(norm);
    if(norm == 0.0) // avoid divide by zero
      {
      itkExceptionMacro(<< "Degenerate orientation vector " << this->m_Orientation);
      }
    for(unsigned int i = 0; i < VDimension; ++i)
      {
      this->m_NormalizedOrientation[i] = this->m_Orientation[i] / norm;
      }
    this->Modified();
    }
}

template< unsigned int VDimension, typename TInput >
typename FiniteCylinderSpatialFunction< VDimension, TInput >::OutputType
FiniteCylinderSpatialFunction< VDimension, TInput >
::Evaluate(const InputType & position) const
{
  const double halfAxisLength = 0.5 * m_AxisLength;

  Vector< double, VDimension > orientationVector;
  Vector< double, VDimension > pointVector;
  Vector< double, VDimension > medialAxisVector;

  for(unsigned int i = 0; i < VDimension; ++i)
    {
    pointVector[i] = position[i] - m_Center[i];
    }
  for(unsigned int i = 0; i < VDimension; ++i)
    {
    medialAxisVector[i] = m_NormalizedOrientation[i];
    }

  //if length_test is less than the length of the cylinder (half actually,
  // because its length from the center), than
  //the point is within the length of the cylinder along the medial axis
  const double distanceFromCenter = dot_product( medialAxisVector.GetVnlVector(), pointVector.GetVnlVector() );

  if ( std::fabs(distanceFromCenter) <= ( halfAxisLength )
       && m_Radius >= std::sqrt( std::pow(pointVector.GetVnlVector().magnitude(), 2.0) - std::pow(distanceFromCenter, 2.0) ) )
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
  os << indent << "Normalized Orientation: " << std::endl;
  for ( i = 0; i < VDimension; i++ )
    {
    os << indent << indent <<  m_NormalizedOrientation[i] << " ";
    }
  os << std::endl;
}
} // end namespace itk

#endif
