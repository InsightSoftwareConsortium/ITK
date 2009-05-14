/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarRegionBasedLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScalarRegionBasedLevelSetFunction_txx
#define __itkScalarRegionBasedLevelSetFunction_txx

#include "itkScalarRegionBasedLevelSetFunction.h"

namespace itk {

template < class TInputImage, class TFeatureImage, class TSharedData >
typename ScalarRegionBasedLevelSetFunction< TInputImage, TFeatureImage, TSharedData >::ScalarValueType
ScalarRegionBasedLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ComputeOverlapParameters( const FeatureIndexType& globalIndex, ScalarValueType& product )
{
// This conditional statement computes the amount of overlap s
// and the presence of background pr
  unsigned int fId = this->m_FunctionId;

  // accumulates the overlap across all functions
  ScalarValueType sum = 0;
  product = 1.;

  ListPixelType L;
  L = this->m_SharedData->m_NearestNeighborListImage->GetPixel( globalIndex );

  InputPixelType hVal;
  InputIndexType otherIndex;

  for ( ListPixelIterator it = L.begin(); it != L.end(); ++it )
    {
    unsigned int id = *it;
    if ( id != fId )
      {
      otherIndex = this->m_SharedData->m_LevelSetDataPointerVector[fId]->GetIndex( globalIndex );
      hVal = this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_HeavisideFunctionOfLevelSetImage->GetPixel( otherIndex );

      sum += hVal;
      product *= hVal;
      }
    }
  return sum;
}

/* Performs the narrow-band update of the Heaviside function for each voxel. The
characteristic function of each region is recomputed (note the shared
data which contains information from the other level sets). Using the
new H values, the previous c_i are updated. */
template < class TInputImage, class TFeatureImage, class TSharedData >
void
ScalarRegionBasedLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::UpdatePixel( const unsigned int& idx, NeighborhoodIterator< TInputImage >
&iterator, InputPixelType &newValue, bool & itkNotUsed(status) )
{
  unsigned int fId = this->m_FunctionId;

  // For each affected h val: h val = new hval (this will dirty some cvals)
  InputIndexType inputIndex = iterator.GetIndex( idx );
  FeatureIndexType globalIndex = this->m_SharedData->m_LevelSetDataPointerVector[fId]->GetFeatureIndex( inputIndex );

  FeaturePixelType featureVal = this->m_FeatureImage->GetPixel( inputIndex );

  ScalarValueType oldH = this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_HeavisideFunctionOfLevelSetImage->GetPixel( inputIndex );
  ScalarValueType newH = this->m_DomainFunction->Evaluate( - newValue );

  // Check if it is in other foreground
  ListPixelType L = this->m_SharedData->m_NearestNeighborListImage->GetPixel( globalIndex );
  InputIndexType itInputIndex;
  ScalarValueType hVal;

  bool inBgrnd = true; // assume the pixel is in background
  for( ListPixelType::const_iterator it = L.begin(); it != L.end(); ++it )
    {
    itInputIndex = this->m_SharedData->m_LevelSetDataPointerVector[*it]->GetIndex( globalIndex );
    hVal = this->m_SharedData->m_LevelSetDataPointerVector[*it]->m_HeavisideFunctionOfLevelSetImage->GetPixel( itInputIndex );

    if ( ( hVal > 0.5 ) && ( *it != fId ) )
      {
      inBgrnd = false; // belongs to foreground elsewhere
      }
    }

  // if pixel belonged to current foreground but not anymore so
  if ( ( oldH > 0.5 ) && ( newH <= 0.5 ) )
    {
    UpdateSharedDataInsideParameters( fId, false, featureVal, newH );

    // have to update level-set backgrounds overlapping
    // at the current pixel
    if ( inBgrnd )
      {
      for( ListPixelType::const_iterator it = L.begin(); it != L.end(); ++it )
        {
        UpdateSharedDataOutsideParameters( *it, true, featureVal, newH );
        }
      }
    }

  // if pixel entered the foreground
  if ( ( oldH <= 0.5 ) && ( newH > 0.5 ) )
    {
    UpdateSharedDataInsideParameters( fId, true, featureVal, newH );
    // have to update level-set backgrounds overlapping
    // at the current pixel
    if ( inBgrnd )
      {
      for( ListPixelType::const_iterator it = L.begin(); it != L.end(); ++it )
        {
        UpdateSharedDataOutsideParameters( *it, false, featureVal, newH );
        }
      }
    }

  this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_HeavisideFunctionOfLevelSetImage->SetPixel( inputIndex, newH );
}


} // end namespace itk
#endif
