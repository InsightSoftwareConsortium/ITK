/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNarrowBandCurvesLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNarrowBandCurvesLevelSetImageFilter_txx
#define __itkNarrowBandCurvesLevelSetImageFilter_txx

#include "itkNarrowBandCurvesLevelSetImageFilter.h"

namespace itk
{
template< class TInputImage, class TFeatureImage, class TOutputType >
NarrowBandCurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::NarrowBandCurvesLevelSetImageFilter()
{
  /* Instantiate a geodesic active contour function and set it as the
    segmentation function. */
  m_CurvesFunction = CurvesFunctionType::New();

  this->SetSegmentationFunction(m_CurvesFunction);

  /* Use negative features by default. */
  this->ReverseExpansionDirectionOff();
}

template< class TInputImage, class TFeatureImage, class TOutputType >
void
NarrowBandCurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "CurvesFunction: " << m_CurvesFunction.GetPointer();
}

template< class TInputImage, class TFeatureImage, class TOutputType >
void
NarrowBandCurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::GenerateData()
{
  // Make sure the SpeedImage is setup for the case when PropagationScaling
  // is zero. This image is used by the curvature term.
  if ( this->GetSegmentationFunction()
       && this->GetSegmentationFunction()->GetPropagationWeight() == 0 )
    {
    this->GetSegmentationFunction()->AllocateSpeedImage();
    this->GetSegmentationFunction()->CalculateSpeedImage();
    }

  // Continue with Superclass implementation
  Superclass::GenerateData();
}
} // end namespace itk

#endif
