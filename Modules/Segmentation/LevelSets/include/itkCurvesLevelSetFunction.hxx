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
#ifndef itkCurvesLevelSetFunction_hxx
#define itkCurvesLevelSetFunction_hxx

#include "itkMath.h"
#include "itkCurvesLevelSetFunction.h"
#include "itkImageRegionIterator.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkVectorCastImageFilter.h"
#include "itkImageAlgorithm.h"

namespace itk
{
template< typename TImageType, typename TFeatureImageType >
void
CurvesLevelSetFunction< TImageType, TFeatureImageType >
::Initialize(const RadiusType & r)
{
  Superclass::Initialize(r);

  this->SetAdvectionWeight(NumericTraits< ScalarValueType >::OneValue());
  this->SetPropagationWeight(NumericTraits< ScalarValueType >::OneValue());
  this->SetCurvatureWeight(NumericTraits< ScalarValueType >::OneValue());
}

template< typename TImageType, typename TFeatureImageType >
void CurvesLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
{
  /* copy the feature image into the speed image */
  ImageAlgorithm::Copy( this->GetFeatureImage(),
                        this->GetSpeedImage(),
                        this->GetFeatureImage()->GetRequestedRegion(),
                        this->GetFeatureImage()->GetRequestedRegion() );

}

template< typename TImageType, typename TFeatureImageType >
void CurvesLevelSetFunction< TImageType, TFeatureImageType >
::CalculateAdvectionImage()
{
  /* compute the gradient of the feature image. */

  typename VectorImageType::Pointer gradientImage;

  if ( Math::NotAlmostEquals( m_DerivativeSigma, NumericTraits< float >::ZeroValue() ) )
    {
    typedef GradientRecursiveGaussianImageFilter< FeatureImageType, VectorImageType >
    DerivativeFilterType;

    typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
    derivative->SetInput( this->GetFeatureImage() );
    derivative->SetSigma(m_DerivativeSigma);
    derivative->Update();

    gradientImage = derivative->GetOutput();
    }
  else
    {
    typedef GradientImageFilter< FeatureImageType > DerivativeFilterType;

    typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
    derivative->SetInput( this->GetFeatureImage() );
    derivative->SetUseImageSpacingOn();
    derivative->Update();

    typedef typename DerivativeFilterType::OutputImageType                      DerivativeOutputImageType;
    typedef VectorCastImageFilter< DerivativeOutputImageType, VectorImageType > GradientCasterType;

    typename GradientCasterType::Pointer caster = GradientCasterType::New();
    caster->SetInput( derivative->GetOutput() );
    caster->Update();

    gradientImage = caster->GetOutput();
    }

  /* copy negative gradient into the advection image. */
  ImageRegionIterator< VectorImageType >
  dit( gradientImage, this->GetFeatureImage()->GetRequestedRegion() );
  ImageRegionIterator< VectorImageType >
  ait( this->GetAdvectionImage(), this->GetFeatureImage()->GetRequestedRegion() );

  for ( dit.GoToBegin(), ait.GoToBegin(); !dit.IsAtEnd(); ++dit, ++ait )
    {
    typename VectorImageType::PixelType v = dit.Get();
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      v[j] *= -1.0L;
      }
    ait.Set(v);
    }
}
} // end namespace itk

#endif
