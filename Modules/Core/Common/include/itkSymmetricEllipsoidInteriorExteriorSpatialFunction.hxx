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
#ifndef itkSymmetricEllipsoidInteriorExteriorSpatialFunction_hxx
#define itkSymmetricEllipsoidInteriorExteriorSpatialFunction_hxx

#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"
#include <cmath>

namespace itk
{
template< unsigned int VDimension, typename TInput >
SymmetricEllipsoidInteriorExteriorSpatialFunction< VDimension, TInput >
::SymmetricEllipsoidInteriorExteriorSpatialFunction() :
  m_UniqueAxis(10),       // Length of unique axis
  m_SymmetricAxes(5),     // Length of symmetric axes
  m_VectorRatio(0.0)      // Vector ratio
{
  m_Center.Fill(0.0);      // Origin of ellipsoid
  m_Orientation.Fill(1.0); // Orientation of unique axis
}

template< unsigned int VDimension, typename TInput >
SymmetricEllipsoidInteriorExteriorSpatialFunction< VDimension, TInput >
::~SymmetricEllipsoidInteriorExteriorSpatialFunction()
{}

template< unsigned int VDimension, typename TInput >
typename SymmetricEllipsoidInteriorExteriorSpatialFunction< VDimension, TInput >::OutputType
SymmetricEllipsoidInteriorExteriorSpatialFunction< VDimension, TInput >
::Evaluate(const InputType & position) const
{
  double uniqueTerm;     // Term in ellipsoid equation for unique axis
  double symmetricTerm;  // Term in ellipsoid equation for symmetric axes

  Vector< double, VDimension > pointVector;
  Vector< double, VDimension > symmetricVector;

  // Project the position onto the major axis, normalize by axis length,
  // and determine whether position is inside ellipsoid.
  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    pointVector[i] = position[i] - m_Center[i];
    }

  uniqueTerm = std::pow( static_cast< double >( ( ( pointVector * m_Orientation ) / ( .5 * m_UniqueAxis ) ) ),
                        static_cast< double >( 2 ) );
  symmetricVector = pointVector - ( m_Orientation * ( pointVector * m_Orientation ) );
  symmetricTerm = std::pow(
    static_cast< double >( ( ( symmetricVector.GetNorm() ) / ( .5 * m_SymmetricAxes ) ) ), static_cast< double >( 2 ) );

  if ( ( uniqueTerm + symmetricTerm ) >= 0 && ( uniqueTerm + symmetricTerm ) <= 1 )
    {
    return 1; // Inside the ellipsoid.
    }
  //Default return value assumes outside the ellipsoid
  return 0; // Outside the ellipsoid.
}

template< unsigned int VDimension, typename TInput >
void SymmetricEllipsoidInteriorExteriorSpatialFunction< VDimension, TInput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Origin of Ellipsoid: ";
  os << m_Center << std::endl;
  os << indent << "Unique Axis Orientation: ";
  os << m_Orientation << std::endl;
  os << indent << "Unique Axis Length: ";
  os << m_UniqueAxis << std::endl;
  os << indent << "Symmetric Axis Length: ";
  os << m_SymmetricAxes << std::endl;
}

template< unsigned int VDimension, typename TInput >
void SymmetricEllipsoidInteriorExteriorSpatialFunction< VDimension, TInput >
::SetOrientation(VectorType orientation, double uniqueAxis, double symmetricAxes)
{
  m_Orientation = orientation;     // Orientation of unique axis of ellipsoid
  m_SymmetricAxes = symmetricAxes; // Length of symmetric axes
  m_UniqueAxis = uniqueAxis;       // Length of unique axis
}
} // end namespace itk

#endif
