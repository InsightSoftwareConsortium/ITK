/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionBasedLevelSetFunctionData.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegionBasedLevelSetFunctionData_txx
#define __itkRegionBasedLevelSetFunctionData_txx

#include "itkRegionBasedLevelSetFunctionData.h"

namespace itk
{
template< class TInputImage, class TFeatureImage >
RegionBasedLevelSetFunctionData< TInputImage, TFeatureImage >
::RegionBasedLevelSetFunctionData()
{
  m_WeightedNumberOfPixelsInsideLevelSet  = 0.;
  m_WeightedNumberOfPixelsOutsideLevelSet = 0.;

  m_HeavisideFunctionOfLevelSetImage = 0;
}

template< class TInputImage, class TFeatureImage >
void
RegionBasedLevelSetFunctionData< TInputImage, TFeatureImage >
::CreateHeavisideFunctionOfLevelSetImage(const InputImageType *image)
{
  const InputRegionType region = image->GetLargestPossibleRegion();

  this->m_HeavisideFunctionOfLevelSetImage = InputImageType::New();
  this->m_HeavisideFunctionOfLevelSetImage->CopyInformation(image);
  this->m_HeavisideFunctionOfLevelSetImage->SetRegions(region);
  this->m_HeavisideFunctionOfLevelSetImage->Allocate();
  this->m_HeavisideFunctionOfLevelSetImage->FillBuffer(0);

  const InputPointType origin = image->GetOrigin();

  this->m_HeavisideFunctionOfLevelSetImage->TransformPhysicalPointToIndex(origin, this->m_Start);

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    this->m_End[i] = this->m_Start[i] + static_cast< InputIndexValueType >( region.GetSize()[i] ) - 1;
    }
}

template< class TInputImage, class TFeatureImage >
typename RegionBasedLevelSetFunctionData< TInputImage, TFeatureImage >::InputIndexType
RegionBasedLevelSetFunctionData< TInputImage, TFeatureImage >
::GetIndex(const FeatureIndexType & featureIndex)
{
  InputIndexType index;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    index[i] = featureIndex[i] - static_cast< InputIndexValueType >( this->m_Start[i] );
    }

  return index;
}

template< class TInputImage, class TFeatureImage >
typename RegionBasedLevelSetFunctionData< TInputImage, TFeatureImage >::FeatureIndexType
RegionBasedLevelSetFunctionData< TInputImage, TFeatureImage >
::GetFeatureIndex(const InputIndexType & inputIndex)
{
  FeatureIndexType index;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    index[i] = inputIndex[i] + static_cast< InputIndexValueType >( this->m_Start[i] );
    }

  return index;
}
} //end namespace itk

#endif
