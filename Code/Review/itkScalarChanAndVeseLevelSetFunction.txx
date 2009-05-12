/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScalarChanAndVeseLevelSetFunction_txx
#define __itkScalarChanAndVeseLevelSetFunction_txx

#include "itkScalarChanAndVeseLevelSetFunction.h"

namespace itk {

template < class TInputImage, class TFeatureImage, class TSharedData >
void
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::UpdateSharedDataParameters()
{
  unsigned int fId = this->m_FunctionId;

  if ( this->m_SharedData->m_NumberOfPixelsInsideLevelSet[fId] == 0 )
    {
    this->m_SharedData->m_ForegroundConstantValues[fId] = 0;
    }
  else
    {
    this->m_SharedData->m_ForegroundConstantValues[fId] =
      this->m_SharedData->m_SumOfPixelValuesInsideLevelSet[fId] /
      this->m_SharedData->m_NumberOfPixelsInsideLevelSet[fId];
    }

  if ( this->m_SharedData->m_NumberOfPixelsOutsideLevelSet[fId] == 0 )
    {
    this->m_SharedData->m_BackgroundConstantValues[fId] = 0;
    }
  else
    {
    this->m_SharedData->m_BackgroundConstantValues[fId] =
      this->m_SharedData->m_SumOfPixelValuesOutsideLevelSet[fId] /
      this->m_SharedData->m_NumberOfPixelsOutsideLevelSet[fId];
    }
}
/* Calculates the numerator and denominator for c_i for each region. As part of
the optimization, it is called once at the beginning of the code, and then the
cNum and cDen are updated during the evolution without iterating through the
entire image. */
template < class TInputImage, class TFeatureImage, class TSharedData >
void
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ComputeParameters()
{
  unsigned int fId = this->m_FunctionId;

  this->m_SharedData->m_NumberOfPixelsInsideLevelSet[fId] = 0;
  this->m_SharedData->m_SumOfPixelValuesInsideLevelSet[fId] = 0;
  this->m_SharedData->m_ForegroundConstantValues[fId] = 0;
  this->m_SharedData->m_NumberOfPixelsOutsideLevelSet[fId] = 0;
  this->m_SharedData->m_SumOfPixelValuesOutsideLevelSet[fId] = 0;
  this->m_SharedData->m_BackgroundConstantValues[fId] = 0;

  FeatureImageConstPointer featureImage = this->m_FeatureImage;

  ImageIteratorType It( this->m_SharedData->m_HeavisideFunctionOfLevelSetImage[fId],
    this->m_SharedData->m_HeavisideFunctionOfLevelSetImage[fId]->GetLargestPossibleRegion() );
  ConstFeatureIteratorType fIt( this->m_FeatureImage,
    this->m_FeatureImage->GetLargestPossibleRegion() );

  FeaturePixelType featureVal;
  FeatureIndexType globalIndex;
  InputIndexType itInputIndex, inputIndex;
  InputPixelType hVal;
  ListPixelType L;

  for( It.GoToBegin(), fIt.GoToBegin(); !It.IsAtEnd();
    ++It, ++fIt )
    {
    featureVal = fIt.Get();
    inputIndex = It.GetIndex();

    globalIndex = this->m_SharedData->GetFeatureIndex( fId, inputIndex );

    L = this->m_SharedData->m_NearestNeighborListImage->GetPixel( globalIndex );

    bool inBgrnd = true; // assume the pixel is in background
    for( ListPixelConstIterator it = L.begin(); it != L.end(); ++it )
      {
      itInputIndex = this->m_SharedData->GetIndex( *it, globalIndex );
      hVal = this->m_SharedData->m_HeavisideFunctionOfLevelSetImage[*it]->GetPixel( itInputIndex );

      if ( hVal > 0.5 )
        {
        // inside the level-set function
        inBgrnd = false;

        if (*it == fId)
          {
          this->m_SharedData->m_SumOfPixelValuesInsideLevelSet[fId] += featureVal;
          this->m_SharedData->m_NumberOfPixelsInsideLevelSet[fId] += 1.;
          }
        }
      }

    // if the pixel belongs to the background
    if ( inBgrnd )
      {
      this->m_SharedData->m_SumOfPixelValuesOutsideLevelSet[fId] += featureVal;
      this->m_SharedData->m_NumberOfPixelsOutsideLevelSet[fId] += 1.;
      }
    }
}


template < class TInputImage, class TFeatureImage, class TSharedData >
typename ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ScalarValueType
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >::
computeInternalTerm( const FeaturePixelType& iValue,
  const FeatureIndexType& itkNotUsed(iIdx), const unsigned int& fId )
{
  const ScalarValueType cVals = this->m_SharedData->m_ForegroundConstantValues[fId];
  const ScalarValueType t = ( iValue - cVals );
  return t * t;
}


template < class TInputImage, class TFeatureImage, class TSharedData >
typename ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ScalarValueType
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::computeExternalTerm( const FeaturePixelType& iValue,
    const FeatureIndexType& itkNotUsed(iIdx), const unsigned int& pr )
{
  const unsigned int fId = this->m_FunctionId;
  const ScalarValueType cBgrnd = this->m_SharedData->m_BackgroundConstantValues[fId]; // background
  const ScalarValueType t = ( iValue - cBgrnd );

  return pr * t * t;
}


} // end namespace itk
#endif
