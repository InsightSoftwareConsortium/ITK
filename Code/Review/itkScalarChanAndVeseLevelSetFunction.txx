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

  if ( this->m_SharedData->m_WeightedNumberOfPixelsInsideLevelSet[fId] > vnl_math::eps )
    {
    this->m_SharedData->m_ForegroundConstantValues[fId] =
      this->m_SharedData->m_WeightedSumOfPixelValuesInsideLevelSet[fId] /
      this->m_SharedData->m_WeightedNumberOfPixelsOutsideLevelSet[fId];
    }
  else
    {
    this->m_SharedData->m_ForegroundConstantValues[fId] = 0;
    }

  if ( this->m_SharedData->m_WeightedNumberOfPixelsOutsideLevelSet[fId] > vnl_math::eps )
    {
    this->m_SharedData->m_BackgroundConstantValues[fId] =
      this->m_SharedData->m_WeightedSumOfPixelValuesOutsideLevelSet[fId] /
      this->m_SharedData->m_WeightedNumberOfPixelsOutsideLevelSet[fId];
    }
  else
    {
    this->m_SharedData->m_BackgroundConstantValues[fId] = 0;
    }
}

template < class TInputImage, class TFeatureImage, class TSharedData >
void
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::UpdateSharedDataInsideParameters( const unsigned int& iId,
    const bool& iA, const FeaturePixelType& iVal, const ScalarValueType& iH )
{
  if( iA )
    {
    this->m_SharedData->m_WeightedNumberOfPixelsInsideLevelSet[iId] += iH;
    this->m_SharedData->m_WeightedSumOfPixelValuesInsideLevelSet[iId] += iVal * iH;
    }
  else
    {
    this->m_SharedData->m_WeightedNumberOfPixelsInsideLevelSet[iId] -= iH;
    this->m_SharedData->m_WeightedSumOfPixelValuesInsideLevelSet[iId] -= iVal * iH;
    }
}

template < class TInputImage, class TFeatureImage, class TSharedData >
void
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::UpdateSharedDataOutsideParameters( const unsigned int& iId,
    const bool& iA, const FeaturePixelType& iVal, const ScalarValueType& iH )
{
  if( iA )
    {
    this->m_SharedData->m_WeightedNumberOfPixelsOutsideLevelSet[iId] += iH;
    this->m_SharedData->m_WeightedSumOfPixelValuesOutsideLevelSet[iId] += iVal * iH;
    }
  else
    {
    this->m_SharedData->m_WeightedNumberOfPixelsOutsideLevelSet[iId] -= iH;
    this->m_SharedData->m_WeightedSumOfPixelValuesOutsideLevelSet[iId] -= iVal * iH;
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

  this->m_SharedData->m_WeightedNumberOfPixelsInsideLevelSet[fId] = 0;
  this->m_SharedData->m_WeightedSumOfPixelValuesInsideLevelSet[fId] = 0;
  this->m_SharedData->m_ForegroundConstantValues[fId] = 0;
  this->m_SharedData->m_WeightedNumberOfPixelsOutsideLevelSet[fId] = 0;
  this->m_SharedData->m_WeightedSumOfPixelValuesOutsideLevelSet[fId] = 0;
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
          this->m_SharedData->m_WeightedSumOfPixelValuesInsideLevelSet[fId] += featureVal;
          this->m_SharedData->m_WeightedNumberOfPixelsInsideLevelSet[fId] += hVal;
          }
        }
      }

    // if the pixel belongs to the background
    if ( inBgrnd )
      {
      this->m_SharedData->m_WeightedSumOfPixelValuesOutsideLevelSet[fId] += featureVal;
      this->m_SharedData->m_WeightedNumberOfPixelsOutsideLevelSet[fId] += hVal;
      }
    }
}


template < class TInputImage, class TFeatureImage, class TSharedData >
typename ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ScalarValueType
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >::
ComputeInternalTerm( const FeaturePixelType& iValue,
  const FeatureIndexType& itkNotUsed(iIdx) )
{
  const unsigned int fId = this->m_FunctionId;
  const ScalarValueType cVals = this->m_SharedData->m_ForegroundConstantValues[fId];
  const ScalarValueType t = ( iValue - cVals );
  return t * t;
}


template < class TInputImage, class TFeatureImage, class TSharedData >
typename ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ScalarValueType
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::ComputeExternalTerm( const FeaturePixelType& iValue,
    const FeatureIndexType& itkNotUsed(iIdx) )
{
  const unsigned int fId = this->m_FunctionId;
  const ScalarValueType cBgrnd = this->m_SharedData->m_BackgroundConstantValues[fId]; // background
  const ScalarValueType t = ( iValue - cBgrnd );

  return t * t;
}


} // end namespace itk
#endif
