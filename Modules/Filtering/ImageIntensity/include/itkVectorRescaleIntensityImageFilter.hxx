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
#ifndef __itkVectorRescaleIntensityImageFilter_hxx
#define __itkVectorRescaleIntensityImageFilter_hxx

#include "itkVectorRescaleIntensityImageFilter.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage >
VectorRescaleIntensityImageFilter< TInputImage, TOutputImage >
::VectorRescaleIntensityImageFilter()
{
  m_OutputMaximumMagnitude   = NumericTraits< OutputRealType >::Zero;
  m_InputMaximumMagnitude    = NumericTraits< InputRealType  >::Zero;

  m_Scale = 1.0;
}

/**
 *
 */
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
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
VectorRescaleIntensityImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  if ( m_OutputMaximumMagnitude < NumericTraits< OutputRealType >::Zero )
    {
    itkExceptionMacro(<< "Maximum output value cannot be negative. You are passing " << m_OutputMaximumMagnitude);
    return;
    }

  InputImagePointer inputImage =   this->GetInput();

  typedef ImageRegionConstIterator< InputImageType > InputIterator;

  InputIterator it( inputImage, inputImage->GetBufferedRegion() );

  it.GoToBegin();

  InputRealType maximumSquaredMagnitude = NumericTraits< InputRealType >::Zero;

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

  // set up the functor values
  this->GetFunctor().SetFactor(m_Scale);
}
} // end namespace itk

#endif
