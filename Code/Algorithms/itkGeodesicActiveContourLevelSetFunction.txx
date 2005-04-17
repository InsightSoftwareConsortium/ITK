/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGeodesicActiveContourLevelSetFunction_txx_
#define __itkGeodesicActiveContourLevelSetFunction_txx_

#include "itkGeodesicActiveContourLevelSetFunction.h"
#include "itkImageRegionIterator.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkVectorCastImageFilter.h"

namespace itk {

template <class TImageType, class TFeatureImageType>
void GeodesicActiveContourLevelSetFunction<TImageType, TFeatureImageType>
::CalculateSpeedImage()
{
  /* copy the feature image into the speed image */
  ImageRegionConstIterator<FeatureImageType>
    fit(this->GetFeatureImage(), this->GetFeatureImage()->GetRequestedRegion());
  ImageRegionIterator<ImageType>
    sit(this->GetSpeedImage(), this->GetFeatureImage()->GetRequestedRegion());

  for ( fit.GoToBegin(), sit.GoToBegin(); ! fit.IsAtEnd(); ++sit, ++fit)
    {
    sit.Set( static_cast<ScalarValueType>( fit.Get() ) );
    }
}


template <class TImageType, class TFeatureImageType>
void GeodesicActiveContourLevelSetFunction<TImageType, TFeatureImageType>
::CalculateAdvectionImage()
{
  /* compute the gradient of the feature image. */

  typename VectorImageType::Pointer gradientImage;

  if (m_DerivativeSigma != NumericTraits<float>::Zero)
    {
    typedef GradientRecursiveGaussianImageFilter<FeatureImageType,VectorImageType>
      DerivativeFilterType;

    typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
    derivative->SetInput( this->GetFeatureImage() );
    derivative->SetSigma( m_DerivativeSigma );
    derivative->Update();

    gradientImage = derivative->GetOutput();
    }
  else
    {
    typedef GradientImageFilter<FeatureImageType> DerivativeFilterType;

    typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
    derivative->SetInput( this->GetFeatureImage() );
    derivative->SetUseImageSpacingOn();
    derivative->Update();

    typedef typename DerivativeFilterType::OutputImageType DerivativeOutputImageType;
    typedef VectorCastImageFilter<DerivativeOutputImageType,VectorImageType> GradientCasterType;

    typename GradientCasterType::Pointer caster = GradientCasterType::New();
    caster->SetInput(derivative->GetOutput());
    caster->Update();

    gradientImage = caster->GetOutput();
    }
  
  /* copy negative gradient into the advection image. */
  ImageRegionIterator<VectorImageType>
    dit( gradientImage, this->GetFeatureImage()->GetRequestedRegion() );
  ImageRegionIterator<VectorImageType>
    ait( this->GetAdvectionImage(), this->GetFeatureImage()->GetRequestedRegion() );

  for( dit.GoToBegin(), ait.GoToBegin(); !dit.IsAtEnd(); ++dit, ++ait )
    {
    typename VectorImageType::PixelType v = dit.Get();
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      v[j] *= -1.0L;
      }
    ait.Set( v);
    }
}

} // end namespace itk


#endif
