/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNarrowBandThresholdSegmentationLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNarrowBandThresholdSegmentationLevelSetImageFilter_txx_
#define __itkNarrowBandThresholdSegmentationLevelSetImageFilter_txx_

#include "itkNarrowBandThresholdSegmentationLevelSetImageFilter.h"

namespace itk {


template <class TInputImage, class TFeatureImage, class TOutputType>
NarrowBandThresholdSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputType>
::NarrowBandThresholdSegmentationLevelSetImageFilter()
{
  m_ThresholdFunction = ThresholdFunctionType::New();
  m_ThresholdFunction->SetUpperThreshold(0);
  m_ThresholdFunction->SetLowerThreshold(0);

  this->SetSegmentationFunction(m_ThresholdFunction);
}
 
template <class TInputImage, class TFeatureImage, class TOutputType>
void
NarrowBandThresholdSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputType>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "ThresholdFunction: " << m_ThresholdFunction;
}


}// end namespace itk

#endif
