/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRescaleIntensityImageFilter.txx
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
#ifndef _itkRescaleIntensityImageFilter_txx
#define _itkRescaleIntensityImageFilter_txx

#include "itkRescaleIntensityImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
RescaleIntensityImageFilter<TInputImage, TOutputImage>
::RescaleIntensityImageFilter()
{
  m_OutputMaximum   = NumericTraits<OutputPixelType>::Zero;
  m_OutputMinimum   = NumericTraits<OutputPixelType>::max();

  m_InputMaximum   = NumericTraits<InputPixelType>::Zero;
  m_InputMinimum   = NumericTraits<InputPixelType>::max();
  
  m_Scale = 1.0;
  m_Shift = 0.0;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
RescaleIntensityImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Output Minimum: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutputMinimum)
     << std::endl;
  os << indent << "Output Maximum: "
     << static_cast<typename NumericTraits<OutputPixelType>::PrintType>(m_OutputMaximum)
     << std::endl;
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
RescaleIntensityImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{

  if ( m_OutputMinimum > m_OutputMaximum )
    {
    itkExceptionMacro(<<"Minimum output value cannot be greater than Maximum output value.");
    return;
    }

  typedef MinimumMaximumImageCalculator< TInputImage >  CalculatorType;
  
  typename CalculatorType::Pointer calculator = CalculatorType::New();

  calculator->SetImage( this->GetInput() );

  calculator->Compute();

  m_InputMinimum = calculator->GetMinimum();
  m_InputMaximum = calculator->GetMaximum();

  m_Scale = 
          static_cast<RealType>( m_OutputMaximum - m_OutputMinimum ) /
          static_cast<RealType>( m_InputMaximum - m_InputMinimum );

  m_Shift =
          static_cast<RealType>( m_OutputMinimum ) - 
          static_cast<RealType>( m_InputMinimum ) * m_Scale;
  
  // set up the functor values
  this->GetFunctor().SetMinimum( m_OutputMinimum );
  this->GetFunctor().SetMaximum( m_OutputMaximum );
  this->GetFunctor().SetFactor( m_Scale );
  this->GetFunctor().SetOffset( m_Shift );
  
}


} // end namespace itk

#endif
