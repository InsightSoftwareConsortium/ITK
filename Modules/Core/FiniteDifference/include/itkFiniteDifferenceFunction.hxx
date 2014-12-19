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
#ifndef itkFiniteDifferenceFunction_hxx
#define itkFiniteDifferenceFunction_hxx

#include "itkFiniteDifferenceFunction.h"

namespace itk
{
template< typename TImageType >
FiniteDifferenceFunction< TImageType >::
FiniteDifferenceFunction()
{
  // initialize variables
  m_Radius.Fill(0);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    m_ScaleCoefficients[i] = 1.0;
    }
}

template< typename TImageType >
void
FiniteDifferenceFunction< TImageType >::SetRadius(const RadiusType & r)
{
  m_Radius = r;
}

template< typename TImageType >
const typename FiniteDifferenceFunction< TImageType >::RadiusType &
FiniteDifferenceFunction< TImageType >::GetRadius() const
{
  return m_Radius;
}

template< typename TImageType >
void
FiniteDifferenceFunction< TImageType >::
SetScaleCoefficients(PixelRealType vals[ImageDimension])
{
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    m_ScaleCoefficients[i] = vals[i];
    }
}

template< typename TImageType >
void
FiniteDifferenceFunction< TImageType >::
GetScaleCoefficients(PixelRealType vals[ImageDimension]) const
{
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    vals[i] = m_ScaleCoefficients[i];
    }
}

template< typename TImageType >
void
FiniteDifferenceFunction< TImageType >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Radius: " << m_Radius << std::endl;
  os << indent << "ScaleCoefficients: " << m_ScaleCoefficients;
}

template< typename TImageType >
const typename FiniteDifferenceFunction< TImageType >::NeighborhoodScalesType
FiniteDifferenceFunction< TImageType >::ComputeNeighborhoodScales() const
{
  NeighborhoodScalesType neighborhoodScales;

  neighborhoodScales.Fill(0.0);
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
