/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSegmentationLevelSetFunction_txx_
#define __itkSegmentationLevelSetFunction_txx_

#include "itkSegmentationLevelSetFunction.h"

namespace itk {

template <class TImageType, class TFeatureImageType>
void SegmentationLevelSetFunction<TImageType, TFeatureImageType>
::ReverseExpansionDirection()
{
  this->SetPropagationWeight( -1.0 * this->GetPropagationWeight() );
  this->SetAdvectionWeight( -1.0 * this->GetAdvectionWeight() );
}

template <class TImageType, class TFeatureImageType>
void SegmentationLevelSetFunction<TImageType, TFeatureImageType>
::Initialize(const RadiusType &r)
{
  Superclass::Initialize(r);  
}

template <class TImageType, class TFeatureImageType>
void SegmentationLevelSetFunction<TImageType, TFeatureImageType>
::AllocateSpeedImage()
{
  m_SpeedImage->SetRequestedRegion(m_FeatureImage->GetRequestedRegion());
  m_SpeedImage->SetBufferedRegion(m_FeatureImage->GetBufferedRegion());
  m_SpeedImage->SetLargestPossibleRegion(m_FeatureImage->GetLargestPossibleRegion());
  m_SpeedImage->Allocate();
  m_Interpolator->SetInputImage(m_SpeedImage);
}

template <class TImageType, class TFeatureImageType>
void SegmentationLevelSetFunction<TImageType, TFeatureImageType>
::AllocateAdvectionImage()
{
  m_AdvectionImage->SetRequestedRegion(m_FeatureImage->GetRequestedRegion());
  m_AdvectionImage->SetBufferedRegion(m_FeatureImage->GetBufferedRegion());
  m_AdvectionImage->SetLargestPossibleRegion(m_FeatureImage->GetLargestPossibleRegion());
  m_AdvectionImage->Allocate();
  m_VectorInterpolator->SetInputImage(m_AdvectionImage);
}

template <class TImageType, class TFeatureImageType>
typename SegmentationLevelSetFunction<TImageType, TFeatureImageType>::ScalarValueType
SegmentationLevelSetFunction<TImageType, TFeatureImageType>
::PropagationSpeed(const NeighborhoodType &neighborhood,
                   const FloatOffsetType &offset) const
{
  IndexType idx = neighborhood.GetIndex();
  ContinuousIndexType cdx;
  for (unsigned i = 0; i < ImageDimension; ++i)
    {
    cdx[i] = static_cast<double>(idx[i]) - offset[i];
    }
  if ( m_Interpolator->IsInsideBuffer(cdx) )
    {
    return (static_cast<ScalarValueType>(
              m_Interpolator->EvaluateAtContinuousIndex(cdx)));
    }
  else return ( static_cast<ScalarValueType>(m_SpeedImage->GetPixel(idx)) );
}

template <class TImageType, class TFeatureImageType>
typename SegmentationLevelSetFunction<TImageType, TFeatureImageType>::VectorType
SegmentationLevelSetFunction<TImageType, TFeatureImageType>
::AdvectionField(const NeighborhoodType &neighborhood,
                 const FloatOffsetType &offset)  const
{
  IndexType idx = neighborhood.GetIndex();
  ContinuousIndexType cdx;
  for (unsigned i = 0; i < ImageDimension; ++i)
    {
    cdx[i] = static_cast<double>(idx[i]) - offset[i];
    }
  if ( m_VectorInterpolator->IsInsideBuffer(cdx) )
    {
    return ( m_VectorCast(m_VectorInterpolator->EvaluateAtContinuousIndex(cdx)));
    }
  else return ( m_AdvectionImage->GetPixel(idx) );  
}

} // end namespace itk

#endif
