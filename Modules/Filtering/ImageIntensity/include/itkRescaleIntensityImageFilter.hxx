/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef __itkRescaleIntensityImageFilter_hxx
#define __itkRescaleIntensityImageFilter_hxx

#include "itkRescaleIntensityImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage >
RescaleIntensityImageFilter< TInputImage, TOutputImage >
::RescaleIntensityImageFilter()
{
  m_OutputMaximum   = NumericTraits< OutputPixelType >::max();
  m_OutputMinimum   = NumericTraits< OutputPixelType >::NonpositiveMin();

  m_InputMaximum   = NumericTraits< InputPixelType >::Zero;
  m_InputMinimum   = NumericTraits< InputPixelType >::max();

  m_Scale = 1.0;
  m_Shift = 0.0;
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
RescaleIntensityImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Output Minimum: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutputMinimum )
     << std::endl;
  os << indent << "Output Maximum: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutputMaximum )
     << std::endl;
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
RescaleIntensityImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  if ( m_OutputMinimum > m_OutputMaximum )
    {
    itkExceptionMacro(<< "Minimum output value cannot be greater than Maximum output value.");
    return;
    }

  typedef MinimumMaximumImageCalculator< TInputImage > CalculatorType;

  typename CalculatorType::Pointer calculator = CalculatorType::New();

  calculator->SetImage( this->GetInput() );

  calculator->Compute();

  m_InputMinimum = calculator->GetMinimum();
  m_InputMaximum = calculator->GetMaximum();

  if ( m_InputMinimum != m_InputMaximum )
    {
    m_Scale =
      ( static_cast< RealType >( m_OutputMaximum )
        - static_cast< RealType >( m_OutputMinimum ) )
      / ( static_cast< RealType >( m_InputMaximum )
          - static_cast< RealType >( m_InputMinimum ) );
    }
  else if ( m_InputMaximum != NumericTraits< InputPixelType >::Zero )
    {
    m_Scale =
      ( static_cast< RealType >( m_OutputMaximum )
        - static_cast< RealType >( m_OutputMinimum ) )
      / static_cast< RealType >( m_InputMaximum );
    }
  else
    {
    m_Scale = 0.0;
    }

  m_Shift =
    static_cast< RealType >( m_OutputMinimum )
    - static_cast< RealType >( m_InputMinimum ) * m_Scale;

  // set up the functor values
  this->GetFunctor().SetMinimum(m_OutputMinimum);
  this->GetFunctor().SetMaximum(m_OutputMaximum);
  this->GetFunctor().SetFactor(m_Scale);
  this->GetFunctor().SetOffset(m_Shift);
}
} // end namespace itk

#endif
