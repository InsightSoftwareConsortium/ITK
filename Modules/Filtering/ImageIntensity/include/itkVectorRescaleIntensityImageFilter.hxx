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
#ifndef itkVectorRescaleIntensityImageFilter_hxx
#define itkVectorRescaleIntensityImageFilter_hxx

#include "itkVectorRescaleIntensityImageFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
VectorRescaleIntensityImageFilter< TInputImage, TOutputImage >
::VectorRescaleIntensityImageFilter() :
  m_Scale(1.0),
  m_Shift(1.0),
  m_InputMaximumMagnitude(NumericTraits< InputRealType >::ZeroValue()),
  m_OutputMaximumMagnitude(NumericTraits< OutputRealType >::ZeroValue())
{
}

template< typename TInputImage, typename TOutputImage >
void
VectorRescaleIntensityImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  if ( m_OutputMaximumMagnitude < NumericTraits< OutputRealType >::ZeroValue() )
    {
    itkExceptionMacro(<< "Maximum output value cannot be negative. You are passing " << m_OutputMaximumMagnitude);
    return;
    }

  InputImagePointer inputImage = this->GetInput();

  typedef ImageRegionConstIterator< InputImageType > InputIterator;

  InputIterator it( inputImage, inputImage->GetBufferedRegion() );

  it.GoToBegin();

  InputRealType maximumSquaredMagnitude = NumericTraits< InputRealType >::ZeroValue();

  while ( !it.IsAtEnd() )
    {
    InputRealType magnitude = it.Get().GetSquaredNorm();
    if ( magnitude > maximumSquaredMagnitude )
      {
      maximumSquaredMagnitude = magnitude;
      }
    ++it;
    }

  m_InputMaximumMagnitude = std::sqrt(maximumSquaredMagnitude);

  m_Scale = static_cast< InputRealType >( m_OutputMaximumMagnitude )
            / static_cast< InputRealType >( m_InputMaximumMagnitude  );

  // Set up the functor values
  this->GetFunctor().SetFactor(m_Scale);
}

template< typename TInputImage, typename TOutputImage >
void
VectorRescaleIntensityImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Output Maximum Magnitude: "
     << static_cast< typename NumericTraits< OutputRealType >::PrintType >( m_OutputMaximumMagnitude )
     << std::endl;
  os << indent << "Input Maximum Magnitude: "
     << static_cast< typename NumericTraits< InputRealType >::PrintType >( m_InputMaximumMagnitude )
     << std::endl;
  os << indent << "Internal Scale : "
     << static_cast< typename NumericTraits< InputRealType >::PrintType >( m_Scale )
     << std::endl;
  os << indent << "Internal Shift : "
     << static_cast< typename NumericTraits< InputRealType >::PrintType >( m_Shift )
     << std::endl;
}
} // end namespace itk

#endif
