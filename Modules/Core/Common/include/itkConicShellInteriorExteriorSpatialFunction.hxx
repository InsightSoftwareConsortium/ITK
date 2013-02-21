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
#ifndef __itkConicShellInteriorExteriorSpatialFunction_hxx
#define __itkConicShellInteriorExteriorSpatialFunction_hxx

#include "itkConicShellInteriorExteriorSpatialFunction.h"

namespace itk
{
template< unsigned int VDimension, typename TInput >
ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >
::ConicShellInteriorExteriorSpatialFunction()
{
  m_Origin.Fill(0.0);
  m_OriginGradient.Fill(0.0);

  m_DistanceMin = 0;
  m_DistanceMax = 0;
  m_Polarity = 0;
  m_Epsilon = 0;
}

template< unsigned int VDimension, typename TInput >
ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >
::~ConicShellInteriorExteriorSpatialFunction()
{}

template< unsigned int VDimension, typename TInput >
void
ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >
::SetOriginGradient(GradientType grad)
{
  m_OriginGradient = grad;

  // Normalize the origin gradient
  m_OriginGradient.GetVnlVector().normalize();
}

template< unsigned int VDimension, typename TInput >
typename ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >
::OutputType
ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >
::Evaluate(const InputType & position) const
{
  // As from the header...
  /**
   * We are creating search areas from BoundaryPoint1 in which to look for
   * candidate BoundaryPoint2's with which to form core atoms.  Assume the
   * "worst case" that BoundaryPoint2 is somewhere in that search area pointing
   * directly at BoundaryPoint1.
   *
   * The search area (ConicShell?) from each BoundaryPoint1 has the following
   * parameters:
   *
   * DistanceMax and DistanceMin from the location of the BoundaryPoint
   *
   * AngleMax from the line along the gradient at the boundary point.
   * This is determined in n dimensions by taking the dot product of
   * two vectors,
   * (1) the normalized gradient at BoundaryPoint1 and
   * (2) the normalized vector from BoundaryPoint1 to BoundaryPoint2.
   *
   * If the absolute value of that dot product is greater than (1 - epsilon)
   * then you are in the ConicShell.  This epsilon is the same one determining
   * face-to-faceness in the IEEE TMI paper.
   */

  // Set the direction of the gradient
  // O means the direction that the gradient is pointing,
  // 1 means the opposite direction

  typedef Vector< double, VDimension > VectorType;

  // Compute the vector from the origin to the point we're testing
  VectorType vecOriginToTest = position - m_Origin;

  // Compute the length of this vector
  // double vecDistance = vecOriginToTest.GetVnlVector().magnitude();
  double vecDistance = vecOriginToTest.GetNorm();

  // Check to see if this an allowed distance
  if ( !( ( vecDistance > m_DistanceMin ) && ( vecDistance < m_DistanceMax ) ) )
    {
    return 0; // not inside the conic shell
    }
  // Normalize it
  // vecOriginToTest.GetVnlVector().normalize();
  vecOriginToTest.Normalize();

  // Create a temp vector to get around const problems
  GradientType originGradient = m_OriginGradient;

  // Now compute the dot product
  double dotprod = originGradient * vecOriginToTest;

  if ( m_Polarity == 1 )
    {
    dotprod = dotprod * -1;
    }

  // Check to see if it meet's the angle criterior
  OutputType result;
  if ( dotprod > ( 1 - m_Epsilon ) )
    {
    result = 1; // it's inside the shell
    }
  else
    {
    result = 0; // it's not inside the shell
    }

  return result;
}

/** Set/Get the gradient at the origin of the function. */
template< unsigned int VDimension, typename TInput >
typename ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >::GradientType
ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >
::GetOriginGradient()
{
  return m_OriginGradient;
}

template< unsigned int VDimension, typename TInput >
void
ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i;
  os << indent << "Origin: [";
  for ( i = 0; i < VDimension - 1; i++ )
    {
    os << m_Origin[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Gradient at origin: [";
  for ( i = 0; i < VDimension - 1; i++ )
    {
    os << m_OriginGradient[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "DistanceMin: " << m_DistanceMin << std::endl;
  os << indent << "DistanceMax: " << m_DistanceMax << std::endl;
  os << indent << "Epsilon: " << m_Epsilon << std::endl;
  os << indent << "Polarity: " << m_Polarity << std::endl;
}
} // end namespace itk

#endif
