/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSegmentationLevelSetImageFilter_txx
#define __itkSegmentationLevelSetImageFilter_txx

#include "itkSegmentationLevelSetImageFilter.h"

namespace itk
{
template< class TInputImage, class TFeatureImage, class TOutputPixelType >
void
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_ReverseExpansionDirection = " << m_ReverseExpansionDirection << std::endl;
  os << indent << "m_AutoGenerateSpeedAdvection = " << m_AutoGenerateSpeedAdvection << std::endl;
  os << indent << "m_SegmentationFunction = " << m_SegmentationFunction << std::endl;
}

template< class TInputImage, class TFeatureImage, class TOutputPixelType >
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::SegmentationLevelSetImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  this->SetNumberOfLayers(TInputImage::ImageDimension);
  m_SegmentationFunction = 0;
  m_AutoGenerateSpeedAdvection = true;
  this->SetIsoSurfaceValue(NumericTraits< ValueType >::Zero);

  // Provide some reasonable defaults which will at least prevent infinite
  // looping.
  this->SetMaximumRMSError(0.02);
  this->SetNumberOfIterations(1000);
  m_ReverseExpansionDirection = false;
}

template< class TInputImage, class TFeatureImage, class TOutputPixelType >
void
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::GenerateSpeedImage()
{
  m_SegmentationFunction->AllocateSpeedImage();
  m_SegmentationFunction->CalculateSpeedImage();
}

template< class TInputImage, class TFeatureImage, class TOutputPixelType >
void
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::GenerateAdvectionImage()
{
  m_SegmentationFunction->AllocateAdvectionImage();
  m_SegmentationFunction->CalculateAdvectionImage();
}

template< class TInputImage, class TFeatureImage, class TOutputPixelType >
void
SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::GenerateData()
{
  if ( m_SegmentationFunction == 0 )
    {
    itkExceptionMacro("No finite difference function was specified.");
    }

  // A positive speed value causes surface expansion, the opposite of the
  // default.  Flip the sign of the propagation and advection weights.
  if ( m_ReverseExpansionDirection == true )
    {
    this->GetSegmentationFunction()->ReverseExpansionDirection();
    }

  // Allocate the images from which speeds will be sampled.
  if ( this->GetState() == Superclass::UNINITIALIZED && m_AutoGenerateSpeedAdvection == true )
    {
    if ( this->GetSegmentationFunction()->GetPropagationWeight() != 0 )
      {
      this->GenerateSpeedImage();
      }

    if ( this->GetSegmentationFunction()->GetAdvectionWeight() != 0 )
      {
      this->GenerateAdvectionImage();
      }
    }

  // Start the solver
  Superclass::GenerateData();

  // Reset all the signs of the weights.
  if ( m_ReverseExpansionDirection == true )
    {
    this->GetSegmentationFunction()->ReverseExpansionDirection();
    }
}
} // end namespace itk

#endif
