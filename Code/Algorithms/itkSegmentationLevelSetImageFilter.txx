/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSegmentationLevelSetImageFilter_txx_
#define __itkSegmentationLevelSetImageFilter_txx_

#include "itkSegmentationLevelSetImageFilter.h"

namespace itk {

template <class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage>
void
SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, TOutputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_MaximumRMSError = "     << m_MaximumRMSError     << std::endl;
  os << indent << "m_MaximumIterations = "   << m_MaximumIterations   << std::endl;
  os << indent << "m_UseNegativeFeatures = " << m_UseNegativeFeatures << std::endl;
  os << indent << "m_PropagationScaling = "  << m_PropagationScaling  << std::endl;
  os << indent << "m_AdvectionScaling = "      << m_AdvectionScaling  << std::endl;
  os << indent << "m_CurvatureScaling = "    << m_CurvatureScaling  << std::endl;
}

template <class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage>
SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, TOutputImage>
::SegmentationLevelSetImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  this->SetNumberOfLayers(ImageDimension);
  m_SegmentationFunction = 0;
  m_PropagationScaling = NumericTraits<ValueType>::One;
  m_AdvectionScaling = NumericTraits<ValueType>::One;
  m_CurvatureScaling = NumericTraits<ValueType>::One;
  
  this->SetIsoSurfaceValue(NumericTraits<ValueType>::Zero);
  
  // Provide some reasonable defaults which will at least prevent infinite
  // looping.
  m_MaximumRMSError   = 0.02;
  m_MaximumIterations = 1000;
  m_UseNegativeFeatures = false;
}

template <class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage>
void
SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, TOutputImage>
::SetSegmentationFunction(SegmentationFunctionType *s)
{
  unsigned int i;

  m_SegmentationFunction = s; 
  
  typename SegmentationFunctionType::RadiusType r;
  for (i = 0; i < ImageDimension; ++i)  r[i] = 1;
  
  m_SegmentationFunction->Initialize(r);
  this->SetDifferenceFunction(m_SegmentationFunction);
  this->Modified();
}

template <class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage>
void
SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, TOutputImage>
::GenerateData()
{
  if (m_SegmentationFunction == 0)
    {itkExceptionMacro("No finite difference function was specified.");}  

  // Set the propagation speed and/or  advection field scaling.
  if ( m_SegmentationFunction->GetPropagationWeight() != 0 )
    {
    m_SegmentationFunction->SetPropagationWeight(m_PropagationScaling);
    }
  if ( m_SegmentationFunction->GetAdvectionWeight() != 0 )
    {
    m_SegmentationFunction->SetAdvectionWeight(m_AdvectionScaling);
    }

  // Set the propagation speed scaling
  m_SegmentationFunction->SetCurvatureWeight(m_CurvatureScaling);
  
  // A positive speed value implies positive outside, negative inside: the
  //  opposite of the default.
  if ( (m_UseNegativeFeatures == true &&
        this->GetSegmentationFunction()->GetPropagationWeight() < 0)
       || (m_UseNegativeFeatures == false &&
           this->GetSegmentationFunction()->GetPropagationWeight() > 0) )
    {
    this->GetSegmentationFunction()->SetPropagationWeight(
      -1.0 * this->GetSegmentationFunction()->GetPropagationWeight() );
    }

  if ( (m_UseNegativeFeatures == true &&
        this->GetSegmentationFunction()->GetAdvectionWeight() < 0)
       || (m_UseNegativeFeatures == false &&
           this->GetSegmentationFunction()->GetAdvectionWeight() > 0) )
    {
    this->GetSegmentationFunction()->SetAdvectionWeight(
      -1.0 * this->GetSegmentationFunction()->GetAdvectionWeight() );
    }

  // Allocate the images from which speeds will be sampled.

  if (this->GetSegmentationFunction()->GetPropagationWeight() != 0)
    {
    m_SegmentationFunction->AllocateSpeedImage();
    m_SegmentationFunction->CalculateSpeedImage();
    }
  if (this->GetSegmentationFunction()->GetAdvectionWeight() != 0)
    {
    m_SegmentationFunction->AllocateAdvectionImage();
    m_SegmentationFunction->CalculateAdvectionImage();
    }

  // Start the solver
  Superclass::GenerateData();
}

template <class TInputImage, class TFeatureImage, class TOutputPixelType, class TOutputImage>
bool
SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, TOutputImage>
::Halt()
{
  if (this->GetElapsedIterations() >= m_MaximumIterations)
    {      return true;    }
  else if ( this->GetElapsedIterations() == 0)
    { return false; }
  else if ( this->GetRMSChange() <= m_MaximumRMSError )
    { return true; }
  else
    { return false; }
}

} // end namespace itk

#endif
