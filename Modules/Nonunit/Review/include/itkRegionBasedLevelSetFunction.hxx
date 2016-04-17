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
#ifndef itkRegionBasedLevelSetFunction_hxx
#define itkRegionBasedLevelSetFunction_hxx

#include "itkRegionBasedLevelSetFunction.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
template< typename TInput,
          typename TFeature,
          typename TSharedData >
double
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::m_WaveDT = 1.0 / ( 2.0 * ImageDimension );

template< typename TInput,
          typename TFeature,
          typename TSharedData >
double
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::m_DT     = 1.0 / ( 2.0 * ImageDimension );

template< typename TInput,
          typename TFeature,
          typename TSharedData >
RegionBasedLevelSetFunction< TInput,
                             TFeature,
                             TSharedData >
::RegionBasedLevelSetFunction()
{
  m_Lambda1 = NumericTraits< ScalarValueType >::OneValue();
  m_Lambda2 = NumericTraits< ScalarValueType >::OneValue();

  m_OverlapPenaltyWeight = NumericTraits< ScalarValueType >::ZeroValue();
  m_AreaWeight = NumericTraits< ScalarValueType >::ZeroValue();
  m_VolumeMatchingWeight = NumericTraits< ScalarValueType >::ZeroValue();
  m_ReinitializationSmoothingWeight = NumericTraits< ScalarValueType >::ZeroValue();
  m_CurvatureWeight = m_AdvectionWeight = NumericTraits< ScalarValueType >::ZeroValue();
  m_Volume = NumericTraits< ScalarValueType >::ZeroValue();

  m_FunctionId = 0;

  m_SharedData = ITK_NULLPTR;
  m_InitialImage = ITK_NULLPTR;
  m_FeatureImage = ITK_NULLPTR;
  m_UpdateC = false;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    m_InvSpacing[i] = 1;
    }
}

template< typename TInput, typename TFeature, typename TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >::VectorType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::InitializeZeroVectorConstant()
{
  VectorType ans;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    ans[i] = NumericTraits< ScalarValueType >::ZeroValue();
    }

  return ans;
}

template< typename TInput, typename TFeature, typename TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >::VectorType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::m_ZeroVectorConstant =
  RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >::InitializeZeroVectorConstant();

/* Computes the Heaviside function and stores it in
  m_HeavisideFunctionOfLevelSetImage */
template< typename TInput,
          typename TFeature,
          typename TSharedData >
void RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeHImage()
{
  // The phi function
  InputImageConstPointer contourImage = this->m_InitialImage;
  InputImagePointer      hBuffer =
    this->m_SharedData->m_LevelSetDataPointerVector[this->m_FunctionId]->m_HeavisideFunctionOfLevelSetImage;

  // Iterator for the phi function
  typedef ImageRegionConstIteratorWithIndex< InputImageType > ConstImageIteratorType;
  ConstImageIteratorType constIt( contourImage, contourImage->GetRequestedRegion() );

  typedef ImageRegionIteratorWithIndex< InputImageType > ImageIteratorType;
  ImageIteratorType It( hBuffer, hBuffer->GetRequestedRegion() );

  It.GoToBegin(),
    constIt.GoToBegin();

  while ( !constIt.IsAtEnd() )
    {
    // Convention is inside of level-set function is negative
    ScalarValueType hVal = m_DomainFunction->Evaluate( -constIt.Get() );
    It.Set(hVal);
    ++It;
    ++constIt;
    }
}

template< typename TInput,
          typename TFeature,
          typename TSharedData >
void
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::UpdateSharedData(bool forceUpdate)
{
  if ( forceUpdate )
    {
    // Must update all H before updating C
    this->ComputeHImage();
    this->m_UpdateC = false;
    }
  else
    {
    if ( !this->m_UpdateC )
      {
      this->ComputeParameters();
      this->m_UpdateC = true;
      }
    this->UpdateSharedDataParameters();
    }
}

template< typename TInput,
          typename TFeature,
          typename TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >::TimeStepType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeGlobalTimeStep(void *GlobalData) const
{
/* Computing the time-step for stable curve evolution */

  TimeStepType dt = 0.0;

  GlobalDataStruct *d = (GlobalDataStruct *)GlobalData;

  if ( itk::Math::abs(d->m_MaxCurvatureChange) > itk::Math::eps )
    {
    if ( d->m_MaxAdvectionChange > itk::Math::eps )
      {
      dt = std::min( ( m_WaveDT / d->m_MaxAdvectionChange ),
                         ( this->m_DT / d->m_MaxCurvatureChange ) );
      }
    else
      {
      dt = this->m_DT / d->m_MaxCurvatureChange;
      }
    }
  else
    {
    if ( d->m_MaxAdvectionChange > itk::Math::eps )
      {
      //NOTE: What's the difference between this->m_WaveDT and this->m_DT?
      dt = this->m_WaveDT / d->m_MaxAdvectionChange;
      }
    }

  // Reset the values
  d->m_MaxCurvatureChange   = NumericTraits< ScalarValueType >::ZeroValue();
  d->m_MaxGlobalChange      = NumericTraits< ScalarValueType >::ZeroValue();
  d->m_MaxAdvectionChange   = NumericTraits< ScalarValueType >::ZeroValue();

  return dt;
}

template< typename TInput,
          typename TFeature, typename TSharedData >
typename RegionBasedLevelSetFunction< TInput,
                                      TFeature, TSharedData >::
ScalarValueType
RegionBasedLevelSetFunction< TInput,
                             TFeature, TSharedData >::ComputeCurvature(
  const NeighborhoodType & itkNotUsed(it),
  const FloatOffsetType & itkNotUsed(offset), GlobalDataStruct *gd)
{
  // Calculate the mean curvature
  ScalarValueType curvature = NumericTraits< ScalarValueType >::ZeroValue();

  unsigned int i, j;

  for ( i = 0; i < ImageDimension; i++ )
    {
    for ( j = 0; j < ImageDimension; j++ )
      {
      if ( j != i )
        {
        curvature -= gd->m_dx[i] * gd->m_dx[j] * gd->m_dxy[i][j];
        curvature += gd->m_dxy[j][j] * gd->m_dx[i] * gd->m_dx[i];
        }
      }
    }

  if ( gd->m_GradMag > itk::Math::eps )
    {
    curvature /= gd->m_GradMag * gd->m_GradMag * gd->m_GradMag;
    }
  else
    {
    curvature /= 1 + gd->m_GradMagSqr;
    }

  return curvature;
}

// Compute the Hessian matrix and various other derivatives.  Some of these
// derivatives may be used by overloaded virtual functions.
template< typename TInput,
          typename TFeature,
          typename TSharedData >
void
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeHessian(const NeighborhoodType & it, GlobalDataStruct *gd)
{
  const ScalarValueType inputValue = it.GetCenterPixel();

  gd->m_GradMagSqr = 0.;
  gd->m_GradMag = 0.;
  unsigned int i, j;

  for ( i = 0; i < ImageDimension; i++ )
    {
    const unsigned int positionA =
      static_cast< unsigned int >( this->m_Center + this->m_xStride[i] );
    const unsigned int positionB =
      static_cast< unsigned int >( this->m_Center - this->m_xStride[i] );

    gd->m_dx[i] = 0.5 * ( this->m_InvSpacing[i] )
                  * ( it.GetPixel(positionA) - it.GetPixel(positionB) );
    gd->m_dx_forward[i]  = ( this->m_InvSpacing[i] )
                           * ( it.GetPixel(positionA) - inputValue );
    gd->m_dx_backward[i] = ( this->m_InvSpacing[i] )
                           * ( inputValue - it.GetPixel(positionB) );

    gd->m_GradMagSqr += gd->m_dx[i] * gd->m_dx[i];

    gd->m_dxy[i][i] = ( this->m_InvSpacing[i] )
                      * ( gd->m_dx_forward[i] - gd->m_dx_backward[i] );

    for ( j = i + 1; j < ImageDimension; j++ )
      {
      const unsigned int positionAa = static_cast< unsigned int >(
        this->m_Center - this->m_xStride[i] - this->m_xStride[j] );
      const unsigned int positionBa = static_cast< unsigned int >(
        this->m_Center - this->m_xStride[i] + this->m_xStride[j] );
      const unsigned int positionCa = static_cast< unsigned int >(
        this->m_Center + this->m_xStride[i] - this->m_xStride[j] );
      const unsigned int positionDa = static_cast< unsigned int >(
        this->m_Center + this->m_xStride[i] + this->m_xStride[j] );

      gd->m_dxy[i][j] = gd->m_dxy[j][i] = 0.25
                                          * ( this->m_InvSpacing[i] ) * ( this->m_InvSpacing[j] )
                                          * ( it.GetPixel(positionAa) - it.GetPixel(positionBa)
                                              + it.GetPixel(positionDa) - it.GetPixel(positionCa) );
      }
    }
  gd->m_GradMag = std::sqrt(gd->m_GradMagSqr);
}

template< typename TInput,
          typename TFeature,
          typename TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >::PixelType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeUpdate(const NeighborhoodType & it, void *globalData,
                const FloatOffsetType & offset)
{
  // Access the neighborhood center pixel of phi
  const ScalarValueType inputValue = it.GetCenterPixel();

  ScalarValueType laplacian_term = NumericTraits< ScalarValueType >::ZeroValue();
  ScalarValueType curvature_term = NumericTraits< ScalarValueType >::ZeroValue();
  ScalarValueType curvature = NumericTraits< ScalarValueType >::ZeroValue();
  ScalarValueType globalTerm = NumericTraits< ScalarValueType >::ZeroValue();
  VectorType      advection_field;
  ScalarValueType x_energy, advection_term = NumericTraits< ScalarValueType >::ZeroValue();

  // Access the global data structure
  GlobalDataStruct *gd = (GlobalDataStruct *)globalData;

  ComputeHessian(it, gd);

  ScalarValueType dh = m_DomainFunction->EvaluateDerivative(-inputValue);

  // Computing the curvature term
  // Used to regularized using the length of contour
  if ( ( dh != 0. )
       && ( this->m_CurvatureWeight != NumericTraits< ScalarValueType >::ZeroValue() ) )
    {
    curvature = this->ComputeCurvature(it, offset, gd);
    curvature_term = this->m_CurvatureWeight * curvature
                     * this->CurvatureSpeed(it, offset, gd) * dh;

    gd->m_MaxCurvatureChange =
      std::max( gd->m_MaxCurvatureChange, itk::Math::abs(curvature_term) );
    }

  // Computing the laplacian term
  // Used in maintaining squared distance function
  if ( this->m_ReinitializationSmoothingWeight != NumericTraits< ScalarValueType >::ZeroValue() )
    {
    laplacian_term = this->ComputeLaplacian(gd) - curvature;

    laplacian_term *= this->m_ReinitializationSmoothingWeight
                      * this->LaplacianSmoothingSpeed(it, offset, gd);
    }

  if ( ( dh != 0. ) && ( m_AdvectionWeight != NumericTraits< ScalarValueType >::ZeroValue() ) )
    {
    advection_field = this->AdvectionField(it, offset, gd);

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      x_energy = m_AdvectionWeight * advection_field[i];

      // TODO: Is this condition right ?
      if ( x_energy > NumericTraits< ScalarValueType >::ZeroValue() )
        {
        advection_term += advection_field[i] * gd->m_dx_backward[i];
        }
      else
        {
        advection_term += advection_field[i] * gd->m_dx_forward[i];
        }

      gd->m_MaxAdvectionChange =
        std::max( gd->m_MaxAdvectionChange, itk::Math::abs(x_energy) );
      }
    advection_term *= m_AdvectionWeight * dh;
    }

  /* Compute the globalTerm - rms difference of image with c_0 or c_1*/
  if ( dh != 0. )
    {
    globalTerm = dh * this->ComputeGlobalTerm( inputValue, it.GetIndex() );
    }

  /* Final update value is the local terms of curvature lengths and laplacian
  squared distances - global terms of rms differences of image and piecewise
  constant regions*/
  PixelType updateVal =
    static_cast< PixelType >( curvature_term + laplacian_term + globalTerm + advection_term );

  /* If MaxGlobalChange recorded is lower than the current globalTerm */
  if ( itk::Math::abs(gd->m_MaxGlobalChange) < itk::Math::abs(globalTerm) )
    {
    gd->m_MaxGlobalChange = globalTerm;
    }

  return updateVal;
}

template< typename TInput, typename TFeature, typename TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ScalarValueType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeLaplacian(GlobalDataStruct *gd)
{
  ScalarValueType laplacian = 0.;

  // Compute the laplacian using the existing second derivative values
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    laplacian += gd->m_dxy[i][i];
    }

  return laplacian;
}

template< typename TInput, typename TFeature, typename TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ScalarValueType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeVolumeRegularizationTerm()
{
  return 2
         * ( this->m_SharedData->m_LevelSetDataPointerVector[this->m_FunctionId]->
             m_WeightedNumberOfPixelsInsideLevelSet
             - this->m_Volume );
}

/* Computes the fidelity term (eg: (intensity - mean)2 ).
Most of the code is concerned with using the appropriate combination
of Heaviside and dirac delta for each part of the fidelity term.
- the final dH is the dirac delta term corresponding to the current
level set we are updating. */
template< typename TInput, typename TFeature, typename TSharedData >
typename RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ScalarValueType
RegionBasedLevelSetFunction< TInput, TFeature, TSharedData >
::ComputeGlobalTerm(
  const ScalarValueType & itkNotUsed(inputPixel),
  const InputIndexType & inputIndex)
{
  // computes if it belongs to background
  ScalarValueType product = 1;

  // Assuming only 1 level set function to be present
  FeatureIndexType featIndex = static_cast< FeatureIndexType >( inputIndex );

  const FeaturePixelType featureVal =
    this->m_FeatureImage->GetPixel (inputIndex);

  ScalarValueType overlapTerm = 0.;

  // This conditional statement computes the amount of overlap s
  // and the presence of background pr
  if ( this->m_SharedData->m_FunctionCount > 1 )
    {
    featIndex = this->m_SharedData->m_LevelSetDataPointerVector[this->m_FunctionId]->GetFeatureIndex(inputIndex);
    overlapTerm = this->m_OverlapPenaltyWeight *
                  ComputeOverlapParameters(featIndex, product);
    }

  ScalarValueType interim = this->m_Lambda1 * this->ComputeInternalTerm(featureVal, featIndex);
  ScalarValueType outTerm = this->m_Lambda2 * product * this->ComputeExternalTerm(featureVal, featIndex);

  ScalarValueType regularizationTerm = this->m_VolumeMatchingWeight *
                                       ComputeVolumeRegularizationTerm() - this->m_AreaWeight;

  ScalarValueType globalTerm = +interim - outTerm + overlapTerm + regularizationTerm;

  return globalTerm;
}
} // end namespace

#endif
