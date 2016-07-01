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
#ifndef itkGeodesicActiveContourLevelSetFunction_hxx
#define itkGeodesicActiveContourLevelSetFunction_hxx

#include "itkGeodesicActiveContourLevelSetFunction.h"
#include "itkImageRegionIterator.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkVectorCastImageFilter.h"
#include "itkImageAlgorithm.h"
#include "itkMath.h"

namespace itk
{
template< typename TImageType, typename TFeatureImageType >
void GeodesicActiveContourLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
{
  // Copy the feature image into the speed image
  ImageAlgorithm::Copy( this->GetFeatureImage(),
                        this->GetSpeedImage(),
                        this->GetFeatureImage()->GetRequestedRegion(),
                        this->GetFeatureImage()->GetRequestedRegion() );
}

template< typename TImageType, typename TFeatureImageType >
void GeodesicActiveContourLevelSetFunction< TImageType, TFeatureImageType >
::CalculateAdvectionImage()
{
  // Compute the gradient of the feature image

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

  // Copy negative gradient into the advection image
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

template< typename TImageType, typename TFeatureImageType >
void GeodesicActiveContourLevelSetFunction< TImageType, TFeatureImageType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "DerivativeSigma: " << m_DerivativeSigma << std::endl;
}

} // end namespace itk

#endif
