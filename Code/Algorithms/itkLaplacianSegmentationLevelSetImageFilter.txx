/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianSegmentationLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianSegmentationLevelSetImageFilter_txx_
#define __itkLaplacianSegmentationLevelSetImageFilter_txx_

#include "itkLaplacianSegmentationLevelSetImageFilter.h"

namespace itk {

template <class TInputImage, class TFeatureImage, class TOutputPixelType>
LaplacianSegmentationLevelSetImageFilter<TInputImage, TFeatureImage,
                                         TOutputPixelType>
::LaplacianSegmentationLevelSetImageFilter()
{
  m_LaplacianFunction = LaplacianFunctionType::New();

  this->SetSegmentationFunction(m_LaplacianFunction.GetPointer());
}
  
template <class TInputImage, class TFeatureImage, class TOutputPixelType>
void
LaplacianSegmentationLevelSetImageFilter<TInputImage, TFeatureImage,
                                         TOutputPixelType>
::PrintSelf(std::ostream &os, Indent indent) const
{
  //   Superclass::PrintSelf(os, indent);
}


}// end namespace itk




#endif
