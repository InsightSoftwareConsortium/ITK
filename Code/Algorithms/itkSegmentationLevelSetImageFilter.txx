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

template <class TInputImage, class TOutputImage>
void
SegmentationLevelSetImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_MaximumRMSError = "    << m_MaximumRMSError    << std::endl;
  os << indent << "m_MaximumIterations = "  << m_MaximumIterations  << std::endl;
}

template <class TInputImage, class TOutputImage>
SegmentationLevelSetImageFilter<TInputImage, TOutputImage>
::SegmentationLevelSetImageFilter()
{
  this->SetNumberOfLayers(ImageDimension);
  
  m_SegmentationFunction = 0;
  
  this->SetIsoSurfaceValue(0.0);
  
  // Provide some reasonable defaults which will at least prevent infinite
  // looping.
  m_MaximumRMSError   = 0.02;
  m_MaximumIterations = 1000;
}

template <class TInputImage, class TOutputImage>
void
SegmentationLevelSetImageFilter<TInputImage, TOutputImage>
::SetSegmentationFunction(SegmentationFunctionType *s)
{
  unsigned int i;

  m_SegmentationFunction = s; 
  
  SegmentationFunctionType::RadiusType r;
  for (i = 0; i < ImageDimension; ++i)  r[i] = 1;
  
  m_SegmentationFunction->Initialize(r);
  m_SegmentationFunction->SetAdvectionWeight( NumericTraits<ValueType>::Zero);
  m_SegmentationFunction->SetPropagationWeight( NumericTraits<ValueType>::One);
  m_SegmentationFunction->SetCurvatureWeight(NumericTraits<ValueType>::One);
  this->SetDifferenceFunction(m_SegmentationFunction);
  this->Modified();
}

template <class TInputImage, class TOutputImage>
void
SegmentationLevelSetImageFilter<TInputImage, TOutputImage>
::GenerateData()
{
  if (m_SegmentationFunction == 0)
    {itkExceptionMacro("No finite difference function was specified.");}  

  m_SegmentationFunction->AllocateSpeedImage();
  m_SegmentationFunction->CalculateSpeedImage();

  // Start the solver
  Superclass::GenerateData();
}


template <class TInputImage, class TOutputImage>
bool
SegmentationLevelSetImageFilter<TInputImage, TOutputImage>
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
