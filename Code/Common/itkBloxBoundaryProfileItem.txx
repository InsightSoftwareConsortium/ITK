/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryProfileItem.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryProfileItem_txx
#define __itkBloxBoundaryProfileItem_txx

#include "itkBloxBoundaryProfileItem.h"

namespace itk
{
template <unsigned int TImageDimension>
BloxBoundaryProfileItem<TImageDimension>
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

template <unsigned int TImageDimension>
BloxBoundaryProfileItem<TImageDimension>
::~BloxBoundaryProfileItem()
{

}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetBoundaryPoint(BPItemType * point) 
{
  m_BoundaryPoint = point;
}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetLowerIntensity(double lowerIntensity)
{
  m_LowerIntensity = lowerIntensity;
}

template <unsigned int TImageDimension>
double
BloxBoundaryProfileItem<TImageDimension>
::GetLowerIntensity(void)
{
  return m_LowerIntensity;
}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetUpperIntensity(double upperIntensity)
{
  m_UpperIntensity = upperIntensity;
}

template <unsigned int TImageDimension>
double
BloxBoundaryProfileItem<TImageDimension>
::GetUpperIntensity(void)
{
  return(m_UpperIntensity);
}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetMean(double mean)
{
  m_Mean = mean;
}

template <unsigned int TImageDimension>
double
BloxBoundaryProfileItem<TImageDimension>
::GetMean(void)
{
  return(m_Mean);
}

template <unsigned int TImageDimension>
void 
BloxBoundaryProfileItem<TImageDimension>
::SetProfileLength(unsigned int profileLength)
{
  m_ProfileLength = profileLength;
}

template <unsigned int TImageDimension>
unsigned int
BloxBoundaryProfileItem<TImageDimension>
::GetProfileLength(void)
{
  return(m_ProfileLength);
}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetMeanNormalized(void)
{
  m_MeanNormalized = m_Mean - m_ProfileLength/2;
}

template <unsigned int TImageDimension>
double
BloxBoundaryProfileItem<TImageDimension>
::GetMeanNormalized(void)
{
  return(m_MeanNormalized);
}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetStandardDeviation(double standardDeviation)
{
  m_StandardDeviation = standardDeviation;
}

template <unsigned int TImageDimension>
double
BloxBoundaryProfileItem<TImageDimension>
::GetStandardDeviation(void)
{
  return(m_StandardDeviation);
}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetStandardDeviationNormalized(void)
{
  m_StandardDeviationNormalized = m_StandardDeviation / m_ProfileLength;
}

template <unsigned int TImageDimension>
double
BloxBoundaryProfileItem<TImageDimension>
::GetStandardDeviationNormalized(void)
{
  return(m_StandardDeviationNormalized);
}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetOptimalBoundaryLocation(VectorType spatialFunctionOriginVector, 
                                                       VectorType orientation)
{
  VectorType optimalBoundaryLocation;
  optimalBoundaryLocation = m_MeanNormalized * orientation;
  optimalBoundaryLocation = spatialFunctionOriginVector + 
                                                      optimalBoundaryLocation;
  for(unsigned int i = 0; i < TImageDimension; i++)
    m_OptimalBoundaryLocation[i] = optimalBoundaryLocation[i];
}

template <unsigned int TImageDimension>
typename BloxBoundaryProfileItem<TImageDimension>::PositionType
BloxBoundaryProfileItem<TImageDimension>
::GetOptimalBoundaryLocation(void)
{
  return(m_OptimalBoundaryLocation);
}

template <unsigned int TImageDimension>
void
BloxBoundaryProfileItem<TImageDimension>
::SetGradient(GradientType * gradient)
{
  m_Gradient = gradient;
}

template <unsigned int TImageDimension>
typename BloxBoundaryProfileItem<TImageDimension>::GradientType *
BloxBoundaryProfileItem<TImageDimension>
::GetGradient()
{
  return(m_Gradient);
}

} // end namespace itk

#endif
