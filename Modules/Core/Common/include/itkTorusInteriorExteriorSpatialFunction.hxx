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
#ifndef itkTorusInteriorExteriorSpatialFunction_hxx
#define itkTorusInteriorExteriorSpatialFunction_hxx

#include "itkTorusInteriorExteriorSpatialFunction.h"

namespace itk
{
template< unsigned int VDimension, typename TInput >
TorusInteriorExteriorSpatialFunction< VDimension, TInput >
::TorusInteriorExteriorSpatialFunction() :
  m_MajorRadius( 3.0 ),
  m_MinorRadius( 1.0 )
{
  m_Origin.Fill(0.0);
}

template< unsigned int VDimension, typename TInput >
TorusInteriorExteriorSpatialFunction< VDimension, TInput >
::~TorusInteriorExteriorSpatialFunction()
{}

template< unsigned int VDimension, typename TInput >
typename TorusInteriorExteriorSpatialFunction< VDimension, TInput >::OutputType
TorusInteriorExteriorSpatialFunction< VDimension, TInput >
::Evaluate(const InputType & position) const
{
  double x = position[0] - m_Origin[0];
  double y = position[1] - m_Origin[1];
  double z = position[2] - m_Origin[2];

  double k = std::pow(m_MajorRadius - std::sqrt(x * x + y * y), 2.0) + z * z;

  if ( k <= ( m_MinorRadius * m_MinorRadius ) )
    {
    return true;
    }
  else
    {
    return false;
    }
}

template< unsigned int VDimension, typename TInput >
void
TorusInteriorExteriorSpatialFunction< VDimension, TInput >
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

  os << indent << "Major radius: " << m_MajorRadius << std::endl;

  os << indent << "Minor radius: " << m_MinorRadius << std::endl;
}
} // end namespace itk

#endif
