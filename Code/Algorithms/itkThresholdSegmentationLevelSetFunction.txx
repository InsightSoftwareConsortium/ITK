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

namespace itk {

template <class TImageType, class TFeatureImageType>
void ThresholdSegmentationLevelSetFunction<TImageType, TFeatureImageType>
::CalculateSpeedImage()
{
  ImageRegionConstIterator<FeatureImageType>
    fit(this->GetFeatureImage(), this->GetFeatureImage()->GetRequestedRegion());
  ImageRegionIterator<ImageType>
    sit(this->GetSpeedImage(), this->GetFeatureImage()->GetRequestedRegion());

  ScalarValueType upper_threshold = static_cast<ScalarValueType>(m_UpperThreshold);
  ScalarValueType lower_threshold = static_cast<ScalarValueType>(m_LowerThreshold);
  ScalarValueType mid = ( (upper_threshold - lower_threshold) / 2.0 ) + lower_threshold;

  for ( fit = fit.Begin(), sit = sit.Begin(); ! fit.IsAtEnd(); ++sit, ++fit)
    {
    if (static_cast<ScalarValueType>(fit.Get()) < mid)
      {
      sit.Set(static_cast<ScalarValueType>(fit.Get()) - lower_threshold);
      }
    else
      {
      sit.Set(upper_threshold - static_cast<ScalarValueType>(fit.Get()));
      }
    }
}

} // end namespace itk


#endif
