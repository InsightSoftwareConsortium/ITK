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
     << static_cast<NumericTraits<OutputPixelType>::PrintType>(m_OutputMinimum) << std::endl;
  os << indent << "Output Maximum: "
     << static_cast<NumericTraits<OutputPixelType>::PrintType>(m_OutputMaximum) << std::endl;
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
  
  CalculatorType::Pointer calculator = CalculatorType::New();

//  calculator->SetImage( this->GetInput() );

  calculator->Compute();

  const InputPixelType minimum = calculator->GetMinimum();
  const InputPixelType maximum = calculator->GetMaximum();

  const RealType factor = 
          static_cast<RealType>( m_OutputMaximum - m_OutputMinimum ) /
          static_cast<RealType>( maximum - minimum );

  const RealType offset =
          static_cast<RealType>( m_OutputMinimum ) - 
          static_cast<RealType>( minimum );
  
  // set up the functor values
  this->GetFunctor().SetMinimum( m_OutputMinimum );
  this->GetFunctor().SetMaximum( m_OutputMaximum );
  this->GetFunctor().SetFactor( factor );
  this->GetFunctor().SetOffset( offset );
  
}


} // end namespace itk

#endif
