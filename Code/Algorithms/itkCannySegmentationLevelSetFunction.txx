/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannySegmentationLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCannySegmentationLevelSetFunction_txx_
#define __itkCannySegmentationLevelSetFunction_txx_

#include "itkCannySegmentationLevelSetFunction.h"
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

namespace itk {

template <class TImageType, class TFeatureImageType>
void CannySegmentationLevelSetFunction<TImageType, TFeatureImageType>
::CalculateAdvectionImage()
{
  typename CannyEdgeDetectionImageFilter<ImageType, ImageType>::Pointer
    canny = CannyEdgeDetectionImageFilter<ImageType, ImageType>::New();

  typename DanielssonDistanceMapImageFilter<ImageType, ImageType>::Pointer
    distance = DanielssonDistanceMapImageFilter<ImageType, ImageType>::New();
  
  typename CastImageFilter<FeatureImageType, ImageType>::Pointer
    caster = CastImageFilter<FeatureImageType, ImageType>::New();

  typename GradientImageFilter<ImageType, ScalarValueType, ScalarValueType>::Pointer
    gradient = GradientImageFilter<ImageType, ScalarValueType, ScalarValueType>::New();

  typedef typename GradientImageFilter<ImageType, ScalarValueType,
    ScalarValueType>::OutputImageType CovariantVectorImageType;

  typename MultiplyImageFilter<CovariantVectorImageType, ImageType,
    CovariantVectorImageType>::Pointer multiply =
    MultiplyImageFilter<CovariantVectorImageType, ImageType, CovariantVectorImageType>::New();
  
  canny->SetThreshold(m_Threshold);
  canny->SetVariance(m_Variance);
  canny->SetMaximumError(0.01);
  canny->SetOutsideValue(NumericTraits<ScalarValueType>::Zero);
  
  caster->SetInput(this->GetFeatureImage());
  canny->SetInput(caster->GetOutput());
  distance->SetInput(canny->GetOutput());

  gradient->SetInput(distance->GetOutput());

  gradient->Update();

  multiply->SetInput1(gradient->GetOutput());
  multiply->SetInput2(distance->GetOutput());
  
  //  multiply->GraftOutput(dynamic_cast<CovariantVectorImageType *>(this->GetAdvectionImage()));
  multiply->Update();  

  // Copy output to Advection Image
  ImageRegionIterator<VectorImageType> it(this->GetAdvectionImage(),
                                          this->GetAdvectionImage()->GetRequestedRegion());
  ImageRegionConstIterator<CovariantVectorImageType> it_a(multiply->GetOutput(),
                                                          this->GetAdvectionImage()->GetRequestedRegion());
  
  for (; ! it.IsAtEnd(); ++it, ++it_a)
    {
    it.Set(it_a.Get());
    }

  // Copy the distance transform into the speed image.  This causes the level
  // set to grow in zero gradient directions (i.e. expand along an isosurface).
  ImageRegionIterator<ImageType> it_d(this->GetSpeedImage(),
                                      this->GetSpeedImage()->GetRequestedRegion());
  ImageRegionConstIterator<ImageType> it_da(distance->GetOutput(),
                                            this->GetSpeedImage()->GetRequestedRegion());
  
  for (; ! it_d.IsAtEnd(); ++it_d, ++it_da)
    {
    it_d.Set(it_da.Get());
    }

}

} // end namespace itk


#endif
