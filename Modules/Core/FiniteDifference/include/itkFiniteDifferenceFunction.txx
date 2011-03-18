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
#ifndef __itkFiniteDifferenceFunction_txx
#define __itkFiniteDifferenceFunction_txx

#include "itkFiniteDifferenceFunction.h"

namespace itk
{
template< class TImageType >
void
FiniteDifferenceFunction< TImageType >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "ScaleCoefficients: " << m_ScaleCoefficients;
}

template< class TImageType >
const typename FiniteDifferenceFunction< TImageType >::NeighborhoodScalesType
FiniteDifferenceFunction< TImageType >::ComputeNeighborhoodScales() const
{
  NeighborhoodScalesType neighborhoodScales;

  neighborhoodScales.Fill(0.0);
  typedef typename NeighborhoodScalesType::ComponentType NeighborhoodScaleType;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_Radius[i] > 0 )
      {
      neighborhoodScales[i] = this->m_ScaleCoefficients[i] / this->m_Radius[i];
      }
    }
  return neighborhoodScales;
}
} // end namespace itk

#endif
