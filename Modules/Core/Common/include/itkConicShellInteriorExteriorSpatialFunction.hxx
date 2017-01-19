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
#ifndef itkConicShellInteriorExteriorSpatialFunction_hxx
#define itkConicShellInteriorExteriorSpatialFunction_hxx

#include "itkConicShellInteriorExteriorSpatialFunction.h"

namespace itk
{
template< unsigned int VDimension, typename TInput >
ConicShellInteriorExteriorSpatialFunction< VDimension, TInput >
::ConicShellInteriorExteriorSpatialFunction() :
  m_DistanceMin( 0.0 ),
  m_DistanceMax( 0.0 ),
  m_Epsilon( 0.0 ),
  m_Polarity( false )
{
  m_Origin.Fill(0.0);
  m_OriginGradient.Fill(0.0);
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
  typedef Vector< double, VDimension > VectorType;

  // Compute the vector from the origin to the point being tested
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

  // Check if it meets the angle criterion
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
