/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianSegmentationLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianSegmentationLevelSetFunction_txx_
#define __itkLaplacianSegmentationLevelSetFunction_txx_

#include "itkLaplacianSegmentationLevelSetFunction.h"
#include "itkLaplacianImageFilter.h"
#include "itkCastImageFilter.h"

namespace itk {

template <class TImageType, class TFeatureImageType>
void LaplacianSegmentationLevelSetFunction<TImageType, TFeatureImageType>
::CalculateSpeedImage()
{

  typename LaplacianImageFilter<ImageType, ImageType>::Pointer
    filter = LaplacianImageFilter<ImageType, ImageType>::New();

  typename CastImageFilter<FeatureImageType, ImageType>::Pointer
    caster = CastImageFilter<FeatureImageType, ImageType>::New();

  caster->SetInput(this->GetFeatureImage());
  filter->SetInput(caster->GetOutput());
  filter->GraftOutput(this->GetSpeedImage());
  filter->Update();
}

} // end namespace itk


#endif
