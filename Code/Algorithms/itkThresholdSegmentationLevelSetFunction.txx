/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdSegmentationLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkThresholdSegmentationLevelSetFunction_txx_
#define __itkThresholdSegmentationLevelSetFunction_txx_

#include "itkThresholdSegmentationLevelSetFunction.h"
#include "itkImageRegionIterator.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkImageFileWriter.h"

namespace itk {

template <class TImageType, class TFeatureImageType>
void ThresholdSegmentationLevelSetFunction<TImageType, TFeatureImageType>
::CalculateSpeedImage()
{
  typename GradientAnisotropicDiffusionImageFilter<TFeatureImageType, TFeatureImageType>::Pointer
    diffusion  = GradientAnisotropicDiffusionImageFilter<TFeatureImageType, TFeatureImageType>::New();
  typename LaplacianImageFilter<TFeatureImageType, TFeatureImageType>::Pointer
    laplacian = LaplacianImageFilter<TFeatureImageType, TFeatureImageType>::New();
  
  ImageRegionIterator<FeatureImageType> lit;
  ImageRegionConstIterator<FeatureImageType>
    fit(this->GetFeatureImage(), this->GetFeatureImage()->GetRequestedRegion());
  ImageRegionIterator<ImageType>
    sit(this->GetSpeedImage(), this->GetFeatureImage()->GetRequestedRegion());

  if (m_EdgeWeight != 0.0)
    {
    diffusion->SetInput(this->GetFeatureImage());
    diffusion->SetConductanceParameter(m_SmoothingConductance);
    diffusion->SetTimeStep(m_SmoothingTimeStep);
    diffusion->SetNumberOfIterations(m_SmoothingIterations);
    
    laplacian->SetInput(diffusion->GetOutput());
    laplacian->Update();
     
    lit = ImageRegionIterator<FeatureImageType>(laplacian->GetOutput(),
                                          this->GetFeatureImage()->GetRequestedRegion());
    lit.GoToBegin();
    }

  ScalarValueType upper_threshold = static_cast<ScalarValueType>(m_UpperThreshold);
  ScalarValueType lower_threshold = static_cast<ScalarValueType>(m_LowerThreshold);
  ScalarValueType mid = ( (upper_threshold - lower_threshold) / 2.0 ) + lower_threshold;
  ScalarValueType threshold;
  for ( fit.GoToBegin(), sit.GoToBegin(); ! fit.IsAtEnd(); ++sit, ++fit)
    {
    if (static_cast<ScalarValueType>(fit.Get()) < mid)
      {
      threshold = fit.Get() - lower_threshold;
      }
    else
      {
      threshold = upper_threshold - fit.Get();
      }
    
    if ( m_EdgeWeight != 0.0)
      {
      sit.Set( static_cast<ScalarValueType>(threshold + m_EdgeWeight * lit.Get()) );
      ++lit;
      }
    else
      {
      sit.Set( static_cast<ScalarValueType>(threshold) );
      }
    }
 
}

} // end namespace itk


#endif
