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

  if ( this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedNumberOfPixelsInsideLevelSet > vnl_math::eps )
    {
    this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_ForegroundConstantValues =
      this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedSumOfPixelValuesInsideLevelSet /
      this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedNumberOfPixelsOutsideLevelSet;
    }
  else
    {
    this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_ForegroundConstantValues = 0;
    }

  if ( this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedNumberOfPixelsOutsideLevelSet > vnl_math::eps )
    {
    this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_BackgroundConstantValues =
      this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedSumOfPixelValuesOutsideLevelSet /
      this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedNumberOfPixelsOutsideLevelSet;
    }
  else
    {
    this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_BackgroundConstantValues = 0;
    }
  /* std::cout <<this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_ForegroundConstantValues
    <<"  **  "
    <<this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_BackgroundConstantValues
    <<std::endl; */
}

template < class TInputImage, class TFeatureImage, class TSharedData >
void
ScalarChanAndVeseLevelSetFunction< TInputImage, TFeatureImage, TSharedData >
::UpdateSharedDataInsideParameters( const unsigned int& iId,
    const bool& iA, const FeaturePixelType& iVal, const ScalarValueType& iH )
{
  if( iA )
    {
    this->m_SharedData->m_LevelSetDataPointerVector[iId]->m_WeightedNumberOfPixelsInsideLevelSet += iH;
    this->m_SharedData->m_LevelSetDataPointerVector[iId]->m_WeightedSumOfPixelValuesInsideLevelSet += iVal * iH;
    }
  else
    {
    this->m_SharedData->m_LevelSetDataPointerVector[iId]->m_WeightedNumberOfPixelsInsideLevelSet -= iH;
    this->m_SharedData->m_LevelSetDataPointerVector[iId]->m_WeightedSumOfPixelValuesInsideLevelSet -= iVal * iH;
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
    this->m_SharedData->m_LevelSetDataPointerVector[iId]->m_WeightedNumberOfPixelsOutsideLevelSet += iH;
    this->m_SharedData->m_LevelSetDataPointerVector[iId]->m_WeightedSumOfPixelValuesOutsideLevelSet += iVal * iH;
    }
  else
    {
    this->m_SharedData->m_LevelSetDataPointerVector[iId]->m_WeightedNumberOfPixelsOutsideLevelSet -= iH;
    this->m_SharedData->m_LevelSetDataPointerVector[iId]->m_WeightedSumOfPixelValuesOutsideLevelSet -= iVal * iH;
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

  this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedNumberOfPixelsInsideLevelSet = 0;
  this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedSumOfPixelValuesInsideLevelSet = 0;
  this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_ForegroundConstantValues = 0;
  this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedNumberOfPixelsOutsideLevelSet = 0;
  this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedSumOfPixelValuesOutsideLevelSet = 0;
  this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_BackgroundConstantValues = 0;

  FeatureImageConstPointer featureImage = this->m_FeatureImage;

  ImageIteratorType It( this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_HeavisideFunctionOfLevelSetImage,
    this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_HeavisideFunctionOfLevelSetImage->GetLargestPossibleRegion() );
  ConstFeatureIteratorType fIt( this->m_FeatureImage,
    this->m_FeatureImage->GetLargestPossibleRegion() );

  FeaturePixelType featureVal;
  FeatureIndexType globalIndex;
  InputIndexType itInputIndex, inputIndex;
  InputPixelType hVal = 0.5;
  ListPixelType L;

  for( It.GoToBegin(), fIt.GoToBegin(); !It.IsAtEnd();
    ++It, ++fIt )
    {
    featureVal = fIt.Get();
    inputIndex = It.GetIndex();

    globalIndex = this->m_SharedData->m_LevelSetDataPointerVector[fId]->GetFeatureIndex( inputIndex );

    L = this->m_SharedData->m_NearestNeighborListImage->GetPixel( globalIndex );

    bool inBgrnd = true; // assume the pixel is in background
    for( ListPixelConstIterator it = L.begin(); it != L.end(); ++it )
      {
      itInputIndex = this->m_SharedData->m_LevelSetDataPointerVector[*it]->GetIndex( globalIndex );
      hVal = this->m_SharedData->m_LevelSetDataPointerVector[*it]->m_HeavisideFunctionOfLevelSetImage->GetPixel( itInputIndex );

      if ( hVal > 0.5 )
        {
        // inside the level-set function
        inBgrnd = false;

        if (*it == fId)
          {
          this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedSumOfPixelValuesInsideLevelSet += featureVal * hVal;
          this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedNumberOfPixelsInsideLevelSet += hVal;
          }
        }
      }

    // if the pixel belongs to the background
    //     if ( inBgrnd )
      {
      this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedSumOfPixelValuesOutsideLevelSet += featureVal * ( 1. - hVal );
      this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_WeightedNumberOfPixelsOutsideLevelSet += 1. - hVal;
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
  const ScalarValueType cVals = this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_ForegroundConstantValues;
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
  const ScalarValueType cBgrnd = this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_BackgroundConstantValues; // background
  const ScalarValueType t = ( iValue - cBgrnd );

  return t * t;
}


} // end namespace itk
#endif
