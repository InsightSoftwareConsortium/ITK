/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryThresholdImageFilter_txx
#define _itkBinaryThresholdImageFilter_txx

#include "itkBinaryThresholdImageFilter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
BinaryThresholdImageFilter<TInputImage, TOutputImage>
::BinaryThresholdImageFilter()
{
  m_OutsideValue   = NumericTraits<OutputPixelType>::Zero;
  m_InsideValue    = NumericTraits<OutputPixelType>::max();
  m_LowerThreshold = NumericTraits<InputPixelType>::NonpositiveMin();
  m_UpperThreshold = NumericTraits<InputPixelType>::max();
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
BinaryThresholdImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "OutsideValue: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutsideValue) << std::endl;
  os << indent << "InsideValue: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_InsideValue) << std::endl;
  os << indent << "LowerThreshold: "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_LowerThreshold) << std::endl;
  os << indent << "UpperThreshold: "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_UpperThreshold) << std::endl;
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
BinaryThresholdImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{

  if ( m_LowerThreshold > m_UpperThreshold )
    {
    itkExceptionMacro(<<"Lower threshold cannot be greater than upper threshold.");
    }

  // set up the functor values
  this->GetFunctor().SetLowerThreshold( m_LowerThreshold );
  this->GetFunctor().SetUpperThreshold( m_UpperThreshold );
  this->GetFunctor().SetInsideValue( m_InsideValue );
  this->GetFunctor().SetOutsideValue( m_OutsideValue );
  
}


} // end namespace itk

#endif
