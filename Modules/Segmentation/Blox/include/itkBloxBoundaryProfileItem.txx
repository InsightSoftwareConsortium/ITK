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
#ifndef __itkBloxBoundaryProfileItem_txx
#define __itkBloxBoundaryProfileItem_txx

#include "itkBloxBoundaryProfileItem.h"

namespace itk
{
template< unsigned int TImageDimension >
BloxBoundaryProfileItem< TImageDimension >
::BloxBoundaryProfileItem()
{
  m_LowerIntensity = 0;
  m_UpperIntensity = 0;
  m_Mean = 0;
  m_ProfileLength = 0;
  m_MeanNormalized = 0;
  m_StandardDeviation = 0;
  m_StandardDeviationNormalized = 0;
  m_BoundaryPoint = 0;
  m_Gradient = 0;
  m_Gradient2.Fill(0);
}

template< unsigned int TImageDimension >
BloxBoundaryProfileItem< TImageDimension >
::~BloxBoundaryProfileItem()
{}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetBoundaryPoint(BPItemType *point)
{
  m_BoundaryPoint = point;
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetLowerIntensity(double lowerIntensity)
{
  m_LowerIntensity = lowerIntensity;
}

template< unsigned int TImageDimension >
double
BloxBoundaryProfileItem< TImageDimension >
::GetLowerIntensity(void)
{
  return m_LowerIntensity;
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetUpperIntensity(double upperIntensity)
{
  m_UpperIntensity = upperIntensity;
}

template< unsigned int TImageDimension >
double
BloxBoundaryProfileItem< TImageDimension >
::GetUpperIntensity(void)
{
  return ( m_UpperIntensity );
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetMean(double mean)
{
  m_Mean = mean;
}

template< unsigned int TImageDimension >
double
BloxBoundaryProfileItem< TImageDimension >
::GetMean(void)
{
  return ( m_Mean );
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetProfileLength(unsigned int profileLength)
{
  m_ProfileLength = profileLength;
}

template< unsigned int TImageDimension >
unsigned int
BloxBoundaryProfileItem< TImageDimension >
::GetProfileLength(void)
{
  return ( m_ProfileLength );
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetMeanNormalized(void)
{
  m_MeanNormalized = m_Mean - m_ProfileLength / 2;
}

template< unsigned int TImageDimension >
double
BloxBoundaryProfileItem< TImageDimension >
::GetMeanNormalized(void)
{
  return ( m_MeanNormalized );
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetStandardDeviation(double standardDeviation)
{
  m_StandardDeviation = standardDeviation;
}

template< unsigned int TImageDimension >
double
BloxBoundaryProfileItem< TImageDimension >
::GetStandardDeviation(void)
{
  return ( m_StandardDeviation );
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetStandardDeviationNormalized(void)
{
  m_StandardDeviationNormalized = m_StandardDeviation / m_ProfileLength;
}

template< unsigned int TImageDimension >
double
BloxBoundaryProfileItem< TImageDimension >
::GetStandardDeviationNormalized(void)
{
  return ( m_StandardDeviationNormalized );
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetOptimalBoundaryLocation(VectorType spatialFunctionOriginVector,
                             VectorType orientation)
{
  VectorType optimalBoundaryLocation;

  optimalBoundaryLocation = m_MeanNormalized * orientation;
  optimalBoundaryLocation = spatialFunctionOriginVector
                            + optimalBoundaryLocation;
  for ( unsigned int i = 0; i < TImageDimension; i++ )
    {
    m_OptimalBoundaryLocation[i] = optimalBoundaryLocation[i];
    }
}

template< unsigned int TImageDimension >
typename BloxBoundaryProfileItem< TImageDimension >::PositionType
BloxBoundaryProfileItem< TImageDimension >
::GetOptimalBoundaryLocation(void)
{
  return ( m_OptimalBoundaryLocation );
}

template< unsigned int TImageDimension >
void
BloxBoundaryProfileItem< TImageDimension >
::SetGradient(GradientType *gradient)
{
  m_Gradient = gradient;
}

template< unsigned int TImageDimension >
typename BloxBoundaryProfileItem< TImageDimension >::GradientType *
BloxBoundaryProfileItem< TImageDimension >
::GetGradient()
{
  return ( m_Gradient );
}
} // end namespace itk

#endif
