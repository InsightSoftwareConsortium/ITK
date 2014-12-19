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
#ifndef itkSphereSpatialFunction_hxx
#define itkSphereSpatialFunction_hxx

#include "itkSphereSpatialFunction.h"

namespace itk
{
template< unsigned int VImageDimension, typename TInput >
SphereSpatialFunction< VImageDimension, TInput >
::SphereSpatialFunction()
{
  m_Radius = 1.0;

  m_Center.Fill(0.0f);
}

template< unsigned int VImageDimension, typename TInput >
SphereSpatialFunction< VImageDimension, TInput >
::~SphereSpatialFunction()
{}

template< unsigned int VImageDimension, typename TInput >
typename SphereSpatialFunction< VImageDimension, TInput >::OutputType
SphereSpatialFunction< VImageDimension, TInput >
::Evaluate(const InputType & position) const
{
  double acc = 0;

  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    acc += ( position[i] - m_Center[i] ) * ( position[i] - m_Center[i] );
    }

  acc -= m_Radius * m_Radius;

  if ( acc <= 0 ) // inside the sphere
    {
    return 1;
    }
  else
    {
    return 0; // outside the sphere
    }
}

template< unsigned int VImageDimension, typename TInput >
void
SphereSpatialFunction< VImageDimension, TInput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int i;
  os << indent << "Center: [";
  for ( i = 0; i < VImageDimension - 1; i++ )
    {
    os << m_Center[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Radius: " << m_Radius << std::endl;
}
} // end namespace itk

#endif
