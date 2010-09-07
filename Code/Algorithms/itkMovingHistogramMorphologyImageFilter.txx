/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMovingHistogramMorphologyImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMovingHistogramMorphologyImageFilter_txx
#define __itkMovingHistogramMorphologyImageFilter_txx

#include "itkMovingHistogramMorphologyImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
MovingHistogramMorphologyImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::MovingHistogramMorphologyImageFilter()
{
  // default m_boundary should be set by subclasses. Just provide a default
  // value to always get the same behavior if it is not done
  m_Boundary = NumericTraits< PixelType >::Zero;
}

template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
THistogram *
MovingHistogramMorphologyImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::NewHistogram()
{
  THistogram *histogram = Superclass::NewHistogram();

  histogram->SetBoundary(m_Boundary);
  return histogram;
}

template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
void
MovingHistogramMorphologyImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Boundary: " << m_Boundary << std::endl;
}
} // end namespace itk
#endif
