/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeDetectionLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeDetectionLevelSetImageFilter_txx_
#define __itkShapeDetectionLevelSetImageFilter_txx_

#include "itkShapeDetectionLevelSetImageFilter.h"

namespace itk {


template <class TInputImage, class TFeatureImage, class TOutputType>
ShapeDetectionLevelSetImageFilter<TInputImage, TFeatureImage, TOutputType>
::ShapeDetectionLevelSetImageFilter()
{
  /* Instantiate a shape detection function and set it as the segmentation function. */
  m_ShapeDetectionFunction = ShapeDetectionFunctionType::New();

  this->SetSegmentationFunction(m_ShapeDetectionFunction);

  /* Use negative features by default. */
  this->SetUseNegativeFeatures( true );

  /* Turn off interpolation. */
  this->InterpolateSurfaceLocationOff();
}
 
template <class TInputImage, class TFeatureImage, class TOutputType>
void
ShapeDetectionLevelSetImageFilter<TInputImage, TFeatureImage, TOutputType>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "ShapeDetectionFunction: " << m_ShapeDetectionFunction.GetPointer();
}


}// end namespace itk




#endif
