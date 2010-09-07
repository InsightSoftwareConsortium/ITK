/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvesLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCurvesLevelSetImageFilter_txx
#define __itkCurvesLevelSetImageFilter_txx

#include "itkCurvesLevelSetImageFilter.h"

namespace itk
{
template< class TInputImage, class TFeatureImage, class TOutputType >
CurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::CurvesLevelSetImageFilter()
{
  /* Instantiate a geodesic active contour function and set it as the
    segmentation function. */
  m_CurvesFunction = CurvesFunctionType::New();

  this->SetSegmentationFunction(m_CurvesFunction);

  /* Use negative features by default. */
  this->ReverseExpansionDirectionOff();

  /* Turn off interpolation. */
  this->InterpolateSurfaceLocationOff();
}

template< class TInputImage, class TFeatureImage, class TOutputType >
void
CurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "CurvesFunction: \n";
  m_CurvesFunction->Print( os, indent.GetNextIndent() );
}

template< class TInputImage, class TFeatureImage, class TOutputType >
void
CurvesLevelSetImageFilter< TInputImage, TFeatureImage, TOutputType >
::GenerateData()
{
  // Make sure the SpeedImage is setup for the case when PropagationScaling
  // is zero
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
