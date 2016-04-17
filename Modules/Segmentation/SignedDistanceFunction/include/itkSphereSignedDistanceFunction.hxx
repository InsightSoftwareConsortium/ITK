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
#ifndef itkSphereSignedDistanceFunction_hxx
#define itkSphereSignedDistanceFunction_hxx

#include "itkSphereSignedDistanceFunction.h"
#include "itkMath.h"

namespace itk
{
// Constructor with default arguments
template< typename TCoordRep, unsigned int VSpaceDimension >
SphereSignedDistanceFunction< TCoordRep, VSpaceDimension >
::SphereSignedDistanceFunction()
{
  this->GetParameters().SetSize(SpaceDimension + 1);
  this->GetParameters().Fill(0.0);
  this->GetParameters()[0] = 1.0;
  m_Translation.Fill(0.0);
  m_Radius = 1.0;
}

// Set the parameters
template< typename TCoordRep, unsigned int VSpaceDimension >
void
SphereSignedDistanceFunction< TCoordRep, VSpaceDimension >
::SetParameters(const ParametersType & parameters)
{
  if ( parameters != this->GetParameters() )
    {
    this->m_Parameters = parameters;

    m_Radius = parameters[0];

    for ( unsigned int i = 0; i < SpaceDimension; i++ )
      {
      m_Translation[i] = parameters[i + 1];
      }

    this->Modified();
    }
}

// Print self
template< typename TCoordRep, unsigned int VSpaceDimension >
void
SphereSignedDistanceFunction< TCoordRep, VSpaceDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Translation: " << m_Translation << std::endl;
  os << indent << "Radius: " << m_Radius << std::endl;
}

// Evaluate the signed distance
template< typename TCoordRep, unsigned int VSpaceDimension >
typename SphereSignedDistanceFunction< TCoordRep, VSpaceDimension >
::OutputType
SphereSignedDistanceFunction< TCoordRep, VSpaceDimension >
::Evaluate(const PointType & point) const
{
  typedef typename NumericTraits< OutputType >::RealType RealType;
  RealType output = 0.0;

  for ( unsigned int j = 0; j < SpaceDimension; j++ )
    {
    output += itk::Math::sqr( ( point[j] - m_Translation[j] ) );
    }

  output = std::sqrt(output) - m_Radius;

  return output;
}
} // end namespace itk

#endif
