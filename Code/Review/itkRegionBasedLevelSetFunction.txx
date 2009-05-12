/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionBasedLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkRegionBasedLevelSetFunction_txx
#define __itkRegionBasedLevelSetFunction_txx

#include "itkRegionBasedLevelSetFunction.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
template < class TInput,
  class TFeature,
  class TSharedData >
double
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::m_WaveDT = 1.0/(2.0 * ImageDimension);

template < class TInput,
  class TFeature,
  class TSharedData >
double
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::m_DT     = 1.0/(2.0 * ImageDimension);

template < class TInput,
  class TFeature,
  class TSharedData >
RegionBasedLevelSetFunction< TInput,
  TFeature,
  TSharedData >
::RegionBasedLevelSetFunction()
{
  m_AreaWeight = NumericTraits<ScalarValueType>::Zero;
  m_Lambda1 = 1;
  m_Lambda2 = 1;
  m_OverlapPenaltyWeight = NumericTraits<ScalarValueType>::Zero;
  m_VolumeMatchingWeight = NumericTraits<ScalarValueType>::Zero;
  m_Volume = 0;
  m_SharedData = 0;
  m_UpdatedC = false;
  m_UpdatedH = false;

  m_CurvatureWeight = NumericTraits<ScalarValueType>::Zero;
}

/* Computes the Heaviside function and stores it in m_HeavisideFunctionOfLevelSetImage */
template < class TInput,
  class TFeature,
  class TSharedData >
void RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeHImage()
{
  // The phi function
  InputImageConstPointer contourImage = this->m_InitialImage;
  InputImagePointer hBuffer = this->m_SharedData->m_HeavisideFunctionOfLevelSetImage[this->m_FunctionId];

  // Iterator for the phi function
  typedef ImageRegionConstIteratorWithIndex< InputImageType > ConstImageIteratorType;
  ConstImageIteratorType constIt( contourImage, contourImage->GetRequestedRegion() );

  typedef ImageRegionIteratorWithIndex< InputImageType > ImageIteratorType;
  ImageIteratorType It( hBuffer, hBuffer->GetRequestedRegion() );

  It.GoToBegin(),
  constIt.GoToBegin();

  while(  !constIt.IsAtEnd() )
    {
    ScalarValueType hVal = m_DomainFunction->Evaluate( - constIt.Get() );
    It.Set( hVal );
    ++It;
    ++constIt;
    }
}

template < class TInput,
  class TFeature,
  class TSharedData >
void
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::UpdateSharedData( bool forceUpdate )
{
  if ( forceUpdate )
    {
    m_UpdatedC = false;
    m_UpdatedH = false;
    }

  if ( !m_UpdatedH )
    {
    // update H
    m_UpdatedH = true;
    this->ComputeHImage();

    // Must update all H before updating C
    return;
    }

  if ( !m_UpdatedC )
    {
    m_UpdatedC = true;
    this->ComputeParameters();
    }

  this->SpecialProcessing();

  this->UpdateSharedDataParameters();
}

template < class TInput,
  class TFeature,
  class TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >::TimeStepType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeGlobalTimeStep(void *GlobalData) const
{
/* Computing the time-step for stable curve evolution */

  TimeStepType dt;

  GlobalDataStruct *d = (GlobalDataStruct *)GlobalData;

  if (vnl_math_abs(d->m_MaxCurvatureChange) > vnl_math::eps)
    {
    if (d->m_MaxGlobalChange > vnl_math::eps)
      {
      dt = vnl_math_min( ( this->m_WaveDT / d->m_MaxGlobalChange ),
      ( this->m_DT / d->m_MaxCurvatureChange ) );
      }
    else
      {
      dt = this->m_DT / d->m_MaxCurvatureChange;
      }
    }
  else
    {
    if (d->m_MaxGlobalChange > vnl_math::eps)
      {
      //NOTE: What's the difference between this->m_WaveDT and this->m_DT?
      dt = this->m_WaveDT / d->m_MaxGlobalChange;
      }
    else
      {
      dt = 0.0;
      }
    }

  // Reset the values
  d->m_MaxCurvatureChange   = NumericTraits<ScalarValueType>::Zero;
  d->m_MaxGlobalChange      = NumericTraits<ScalarValueType>::Zero;

  return dt;
}

template < class TInput,
  class TFeature, class TSharedData >
typename RegionBasedLevelSetFunction< TInput,
  TFeature, TSharedData >::
ScalarValueType
RegionBasedLevelSetFunction< TInput,
  TFeature, TSharedData >::
ComputeCurvatureTerm(
  const NeighborhoodType &itkNotUsed(neighborhood),
  const FloatOffsetType &itkNotUsed(offset), GlobalDataStruct *gd)
{
  // Calculate the mean curvature
  ScalarValueType curvature_term = NumericTraits<ScalarValueType>::Zero;
  unsigned int i, j;

  for (i = 0; i < ImageDimension; i++)
    {
    for(j = 0; j < ImageDimension; j++)
      {
      if(j != i)
        {
        curvature_term -= gd->m_dx[i] * gd->m_dx[j] * gd->m_dxy[i][j];
        curvature_term += gd->m_dxy[j][j] * gd->m_dx[i] * gd->m_dx[i];
        }
      }
    }

  if( gd->m_GradMagSqr > vnl_math::eps )
    return (curvature_term / gd->m_GradMagSqr );
  else
    return 0.;
}

// Compute the Hessian matrix and various other derivatives.  Some of these
// derivatives may be used by overloaded virtual functions.
template < class TInput,
  class TFeature,
  class TSharedData >
void
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeHessian( const NeighborhoodType &it, GlobalDataStruct *gd )
{
  const ScalarValueType inputValue = it.GetCenterPixel();

  gd->m_GradMagSqr = 0.;
  unsigned int i, j;

  for (i = 0; i < ImageDimension; i++)
    {
    const unsigned int positionA =
      static_cast< unsigned int >( this->m_Center + this->m_xStride[i] );
    const unsigned int positionB =
      static_cast< unsigned int >( this->m_Center - this->m_xStride[i] );

    gd->m_dx[i] = 0.5 * ( it.GetPixel( positionA ) - it.GetPixel( positionB ) );
    gd->m_dxy[i][i] =
      it.GetPixel( positionA ) + it.GetPixel( positionB ) - 2.0 * inputValue;
    gd->m_dx_forward[i]  = it.GetPixel( positionA ) - inputValue;
    gd->m_dx_backward[i] = inputValue - it.GetPixel( positionB );
    gd->m_GradMagSqr += gd->m_dx[i] * gd->m_dx[i];

    for (j = i+1; j < ImageDimension; j++ )
      {
      const unsigned int positionAa = static_cast<unsigned int>(
        this->m_Center - this->m_xStride[i] - this->m_xStride[j] );
      const unsigned int positionBa = static_cast<unsigned int>(
        this->m_Center - this->m_xStride[i] + this->m_xStride[j] );
      const unsigned int positionCa = static_cast<unsigned int>(
        this->m_Center + this->m_xStride[i] - this->m_xStride[j] );
      const unsigned int positionDa = static_cast<unsigned int>(
        this->m_Center + this->m_xStride[i] + this->m_xStride[j] );

      gd->m_dxy[i][j] = gd->m_dxy[j][i] = 0.25 *(
        it.GetPixel( positionAa ) -
        it.GetPixel( positionBa ) -
        it.GetPixel( positionCa ) +
        it.GetPixel( positionDa ) );
      }
    }
}

template < class TInput,
  class TFeature,
  class TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >::PixelType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeUpdate( const NeighborhoodType &it, void *globalData,
  const FloatOffsetType& offset )
{
  // Access the neighborhood center pixel of phi
  const ScalarValueType inputValue = it.GetCenterPixel();

  ScalarValueType laplacian_term = 0.;
  ScalarValueType curvature_term = 0.;
  ScalarValueType globalTerm = 0.;

  // Access the global data structure
  GlobalDataStruct *gd = (GlobalDataStruct *)globalData;

  ComputeHessian( it, gd );

  ScalarValueType dh = m_DomainFunction->EvaluateDerivative( - inputValue );

  // Computing the curvature term
  // Used to regularized using the length of contour
  // What is CurvatureSpeed?
  if ( ( dh != 0. ) &&
    ( this->m_CurvatureWeight != NumericTraits< ScalarValueType >::Zero ) )
    {
    curvature_term =
      this->m_CurvatureWeight *
      this->CurvatureSpeed(it, offset) *
      this->ComputeCurvatureTerm( it, offset, gd ) *
      gd->m_GradMagSqr * dh;

    gd->m_MaxCurvatureChange =
      vnl_math_max( gd->m_MaxCurvatureChange, vnl_math_abs( curvature_term ) );
    }

  // Computing the laplacian term
  // Used in maintaining squared distance function
  if( this->m_LaplacianSmoothingWeight != NumericTraits<ScalarValueType>::Zero)
    {
    ScalarValueType laplacian = NumericTraits<ScalarValueType>::Zero;

    // Compute the laplacian using the existing second derivative values
    for(unsigned int i = 0; i < ImageDimension; i++)
      {
      laplacian += gd->m_dxy[i][i];
      }

    // Use the laplacian to maintain signed distance function
    // What is LaplacianSmoothingSpeed ?
    // Why do we have 0.1 * LaplacianSmoothingWeight ?
    // Why do we have to subtract the curvature_term ?
    laplacian_term =
      0.1*( this->m_LaplacianSmoothingWeight *
      LaplacianSmoothingSpeed(it,offset, gd) * laplacian - curvature_term);
    }

  // Update value from curvature length and laplacian term
  PixelType updateVal =
    static_cast< PixelType >( curvature_term + laplacian_term );

  /* Compute the globalTerm - rms difference of image with c_0 or c_1*/
  if ( dh != 0. )
    {
    globalTerm = dh * this->ComputeGlobalTerm( inputValue, it.GetIndex() );
    }

  /* Final update value is the local terms of curvature lengths and laplacian
  squared distances - global terms of rms differences of image and piecewise
  constant regions*/
  updateVal = updateVal - globalTerm;

  /* If MaxGlobalChange recorded is lower than the current globalTerm */
  if( vnl_math_abs( gd->m_MaxGlobalChange) < vnl_math_abs( globalTerm ) )
    {
    gd->m_MaxGlobalChange = globalTerm;
    }

  return updateVal;
}

template < class TInput, class TFeature, class TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ScalarValueType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeVolumeRegularizationTerm( )
{
  return 2 * ( this->m_SharedData->m_NumberOfPixelsInsideLevelSet[this->m_FunctionId] - this->m_Volume );
}

/* Computes the fidelity term (eg: (intensity - mean)2 ).
Most of the code is concerned with using the appropriate combination
of Heaviside and dirac delta for each part of the fidelity term.
- the final dH is the dirac delta term corresponding to the current
level set we are updating. */
template < class TInput, class TFeature, class TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ScalarValueType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeGlobalTerm(
const ScalarValueType& itkNotUsed( inputPixel ),
const InputIndexType& inputIndex )
{
  unsigned int fId = this->m_FunctionId;
  ScalarValueType product = 1; // computes if it belongs to background

  // Assuming only 1 level set function to be present
  FeatureIndexType featIndex = static_cast< FeatureIndexType >( inputIndex );

  const FeaturePixelType featureVal =
    this->m_FeatureImage->GetPixel ( inputIndex );

  ScalarValueType overlapTerm = 0.;

  // This conditional statement computes the amount of overlap s
  // and the presence of background pr
  if ( this->m_SharedData->m_FunctionCount > 1 )
    {
    featIndex = this->m_SharedData->GetFeatureIndex( fId, inputIndex );
    overlapTerm = this->m_OverlapPenaltyWeight *
      ComputeOverlapParameters( featIndex, product );
    }

  ScalarValueType inTerm = this->m_Lambda1 * this->ComputeInternalTerm( featureVal, featIndex );
  ScalarValueType outTerm = this->m_Lambda2 * product * this->ComputeExternalTerm( featureVal, featIndex );

  ScalarValueType regularizationTerm = this->m_VolumeMatchingWeight *
    ComputeVolumeRegularizationTerm();

  //regularizationTerm -= this->m_Nu;
  //NOTE: regularizationTerm here MUST take into account the curvature term!!!

  ScalarValueType globalTerm = - inTerm + outTerm - overlapTerm - regularizationTerm;

  return globalTerm;
}

} // end namespace

#endif
