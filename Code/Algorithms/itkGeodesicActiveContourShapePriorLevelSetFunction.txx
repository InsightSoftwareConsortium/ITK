/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourShapePriorLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGeodesicActiveContourShapePriorLevelSetFunction_txx_
#define __itkGeodesicActiveContourShapePriorLevelSetFunction_txx_

#include "itkGeodesicActiveContourShapePriorLevelSetFunction.h"
#include "itkGradientRecursiveGaussianImageFilter.h"

namespace itk {

/**
 * Calculate the speed image.
 */
template <class TImageType, class TFeatureImageType>
void
GeodesicActiveContourShapePriorLevelSetFunction<TImageType, TFeatureImageType>
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

/**
 * Calculate the advection speed image
 */
template <class TImageType, class TFeatureImageType>
void GeodesicActiveContourShapePriorLevelSetFunction<TImageType, TFeatureImageType>
::CalculateAdvectionImage()
{
  /* compoute the gradient of the feature image. */
  typedef GradientRecursiveGaussianImageFilter<FeatureImageType,VectorImageType>
    DerivativeFilterType;

  typename DerivativeFilterType::Pointer derivative = DerivativeFilterType::New();
  derivative->SetInput( this->GetFeatureImage() );
  derivative->SetSigma( m_DerivativeSigma );
  derivative->Update();
  
  /* copy negative gradient into the advection image. */
  ImageRegionIterator<VectorImageType>
    dit( derivative->GetOutput(), this->GetFeatureImage()->GetRequestedRegion() );
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
