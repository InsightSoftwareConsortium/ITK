/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorThresholdSegmentationLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorThresholdSegmentationLevelSetFunction_txx_
#define __itkVectorThresholdSegmentationLevelSetFunction_txx_

#include "itkVectorThresholdSegmentationLevelSetFunction.h"
#include "itkImageRegionIterator.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkImageFileWriter.h"

namespace itk {

template <class TImageType, class TFeatureImageType>
void VectorThresholdSegmentationLevelSetFunction<TImageType, TFeatureImageType>
::CalculateSpeedImage()
{
  
  ImageRegionConstIterator<FeatureImageType>
    fit(this->GetFeatureImage(), this->GetFeatureImage()->GetRequestedRegion());
  ImageRegionIterator<ImageType>
    sit(this->GetSpeedImage(), this->GetFeatureImage()->GetRequestedRegion());

  
  
  ScalarValueType threshold;
  for ( fit.GoToBegin(), sit.GoToBegin(); ! fit.IsAtEnd(); ++sit, ++fit)
    {
    threshold = m_Threshold - vcl_sqrt(m_Mahalanobis->Evaluate(fit.Get()));
      sit.Set( static_cast<ScalarValueType>(threshold) );
    
    }
 
}

} // end namespace itk


#endif
