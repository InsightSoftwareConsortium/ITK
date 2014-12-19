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
#ifndef itkSegmentationLevelSetFunction_hxx
#define itkSegmentationLevelSetFunction_hxx

#include "itkSegmentationLevelSetFunction.h"

namespace itk
{
template< typename TImageType, typename TFeatureImageType >
void SegmentationLevelSetFunction< TImageType, TFeatureImageType >
::SetSpeedImage(ImageType *s)
{
  m_SpeedImage = s;
  m_Interpolator->SetInputImage(m_SpeedImage);
}

template< typename TImageType, typename TFeatureImageType >
void SegmentationLevelSetFunction< TImageType, TFeatureImageType >
::SetAdvectionImage(VectorImageType *s)
{
  m_AdvectionImage = s;
  m_VectorInterpolator->SetInputImage(m_AdvectionImage);
}

template< typename TImageType, typename TFeatureImageType >
void SegmentationLevelSetFunction< TImageType, TFeatureImageType >
::ReverseExpansionDirection()
{
  this->SetPropagationWeight( -1.0 * this->GetPropagationWeight() );
  this->SetAdvectionWeight( -1.0 * this->GetAdvectionWeight() );
}

template< typename TImageType, typename TFeatureImageType >
void SegmentationLevelSetFunction< TImageType, TFeatureImageType >
::Initialize(const RadiusType & r)
{
  Superclass::Initialize(r);
}

template< typename TImageType, typename TFeatureImageType >
void SegmentationLevelSetFunction< TImageType, TFeatureImageType >
::AllocateSpeedImage()
{
  m_SpeedImage->SetRequestedRegion( m_FeatureImage->GetRequestedRegion() );
  m_SpeedImage->SetBufferedRegion( m_FeatureImage->GetBufferedRegion() );
  m_SpeedImage->SetLargestPossibleRegion( m_FeatureImage->GetLargestPossibleRegion() );
  m_SpeedImage->Allocate();
  m_Interpolator->SetInputImage(m_SpeedImage);
}

template< typename TImageType, typename TFeatureImageType >
void SegmentationLevelSetFunction< TImageType, TFeatureImageType >
::AllocateAdvectionImage()
{
  m_AdvectionImage->SetRequestedRegion( m_FeatureImage->GetRequestedRegion() );
  m_AdvectionImage->SetBufferedRegion( m_FeatureImage->GetBufferedRegion() );
  m_AdvectionImage->SetLargestPossibleRegion( m_FeatureImage->GetLargestPossibleRegion() );
  m_AdvectionImage->Allocate();
  m_VectorInterpolator->SetInputImage(m_AdvectionImage);
}

template< typename TImageType, typename TFeatureImageType >
typename SegmentationLevelSetFunction< TImageType, TFeatureImageType >::ScalarValueType
SegmentationLevelSetFunction< TImageType, TFeatureImageType >
::PropagationSpeed(const NeighborhoodType & neighborhood,
                   const FloatOffsetType & offset, GlobalDataStruct *) const
{
  IndexType idx = neighborhood.GetIndex();

  ContinuousIndexType cdx;

  for ( unsigned i = 0; i < ImageDimension; ++i )
    {
    cdx[i] = static_cast< double >( idx[i] ) - offset[i];
    }

  if ( m_Interpolator->IsInsideBuffer(cdx) )
    {
    return ( static_cast< ScalarValueType >(
               m_Interpolator->EvaluateAtContinuousIndex(cdx) ) );
    }
  else { return ( static_cast< ScalarValueType >( m_SpeedImage->GetPixel(idx) ) ); }
}

template< typename TImageType, typename TFeatureImageType >
typename SegmentationLevelSetFunction< TImageType, TFeatureImageType >::VectorType
SegmentationLevelSetFunction< TImageType, TFeatureImageType >
::AdvectionField(const NeighborhoodType & neighborhood,
                 const FloatOffsetType & offset, GlobalDataStruct *)  const
{
  IndexType           idx = neighborhood.GetIndex();
  ContinuousIndexType cdx;

  for ( unsigned i = 0; i < ImageDimension; ++i )
    {
    cdx[i] = static_cast< double >( idx[i] ) - offset[i];
    }
  if ( m_VectorInterpolator->IsInsideBuffer(cdx) )
    {
    return ( m_VectorCast( m_VectorInterpolator->EvaluateAtContinuousIndex(cdx) ) );
    }
  //Just return the default else
  return ( m_AdvectionImage->GetPixel(idx) );
}
} // end namespace itk

#endif
