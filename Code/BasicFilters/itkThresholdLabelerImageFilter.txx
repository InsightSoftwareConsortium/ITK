/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdLabelerImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkThresholdLabelerImageFilter_txx
#define _itkThresholdLabelerImageFilter_txx

#include "itkThresholdLabelerImageFilter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ThresholdLabelerImageFilter<TInputImage, TOutputImage>
::ThresholdLabelerImageFilter()
{
  m_Thresholds.clear();
  m_RealThresholds.clear();
  m_LabelOffset = NumericTraits<OutputPixelType>::Zero;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ThresholdLabelerImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Thresholds: ";
  for (unsigned long j=0; j<m_Thresholds.size(); j++)
    {
    os << m_Thresholds[j] << " ";
    }
  os << std::endl;

  os << indent << "Real Thresholds: ";
  for (unsigned long i=0; i<m_RealThresholds.size(); i++)
    {
    os << m_RealThresholds[i] << " ";
    }
  os << std::endl;


  os << indent << "LabelOffset: " << m_LabelOffset << std::endl;
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ThresholdLabelerImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  unsigned int size = m_Thresholds.size();
  for (unsigned int i=0; i<size-1; i++)
    {
    if (m_Thresholds[i] > m_Thresholds[i+1])
      {
      itkExceptionMacro(<<"Thresholds must be sorted.");
      }
    }

  // set up the functor values
  this->GetFunctor().SetThresholds( m_RealThresholds );
  this->GetFunctor().SetLabelOffset( m_LabelOffset );
}

} // end namespace itk

#endif
