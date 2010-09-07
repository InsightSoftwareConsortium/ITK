/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianSegmentationLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianSegmentationLevelSetFunction_txx
#define __itkLaplacianSegmentationLevelSetFunction_txx

#include "itkLaplacianSegmentationLevelSetFunction.h"
#include "itkLaplacianImageFilter.h"
#include "itkCastImageFilter.h"

namespace itk
{
template< class TImageType, class TFeatureImageType >
void LaplacianSegmentationLevelSetFunction< TImageType, TFeatureImageType >
::CalculateSpeedImage()
{
  typename LaplacianImageFilter< ImageType, ImageType >::Pointer
  filter = LaplacianImageFilter< ImageType, ImageType >::New();

  typename CastImageFilter< FeatureImageType, ImageType >::Pointer
  caster = CastImageFilter< FeatureImageType, ImageType >::New();

  caster->SetInput( this->GetFeatureImage() );
  filter->SetInput( caster->GetOutput() );

  // make the laplacian filter use the pixel container from the speed image
  filter->GraftOutput( this->GetSpeedImage() );

  filter->Update();

  // move the meta information (mostly the spacing and origin) back to
  // the speed image.
  //
  // unfortunately functions are not filters so we can't graft back
  this->GetSpeedImage()->Graft( filter->GetOutput() );
}
} // end namespace itk

#endif
