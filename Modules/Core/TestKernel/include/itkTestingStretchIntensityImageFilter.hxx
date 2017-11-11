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
#ifndef itkTestingStretchIntensityImageFilter_hxx
#define itkTestingStretchIntensityImageFilter_hxx

#include "itkTestingStretchIntensityImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkMath.h"

namespace itk
{
namespace Testing
{

template< typename TInputImage, typename TOutputImage >
StretchIntensityImageFilter< TInputImage, TOutputImage >
::StretchIntensityImageFilter() :
  m_Scale( 1.0 ),
  m_Shift( 0.0 ),
  m_InputMinimum( NumericTraits< InputPixelType >::max() ),
  m_InputMaximum( NumericTraits< InputPixelType >::ZeroValue() ),
  m_OutputMinimum( NumericTraits< OutputPixelType >::NonpositiveMin() ),
  m_OutputMaximum( NumericTraits< OutputPixelType >::max() )
{
}

template< typename TInputImage, typename TOutputImage >
void
StretchIntensityImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  if ( m_OutputMinimum > m_OutputMaximum )
    {
    itkExceptionMacro(<< "Minimum output value cannot be greater than Maximum output value.");
    return;
    }

  const TInputImage * inputImage = this->GetInput();

  ImageRegionConstIteratorWithIndex< TInputImage > it( inputImage, inputImage->GetBufferedRegion() );

  m_InputMaximum = NumericTraits< InputPixelType >::NonpositiveMin();
  m_InputMinimum = NumericTraits< InputPixelType >::max();

  while ( !it.IsAtEnd() )
    {
    const InputPixelType value = it.Get();
    if ( value > m_InputMaximum )
      {
      m_InputMaximum = value;
      }
    if ( value < m_InputMinimum )
      {
      m_InputMinimum = value;
      }
    ++it;
    }

  if (itk::Math::abs( m_InputMaximum - m_InputMinimum ) >
      itk::Math::abs( NumericTraits< InputPixelType >::epsilon() ) )
    {
    m_Scale =
      ( static_cast< RealType >( m_OutputMaximum )
        - static_cast< RealType >( m_OutputMinimum ) )
      / ( static_cast< RealType >( m_InputMaximum )
          - static_cast< RealType >( m_InputMinimum ) );
    }
  else if ( m_InputMaximum > NumericTraits< InputPixelType >::epsilon() )
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

}

template< typename TInputImage, typename TOutputImage >
void
StretchIntensityImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  const TInputImage *  inputPtr = this->GetInput();
  TOutputImage      *  outputPtr = this->GetOutput(0);

  InputImageRegionType    inputRegionForThread = outputRegionForThread;

  // Define the iterators
  ImageRegionConstIterator< TInputImage > inputIt(inputPtr, inputRegionForThread);
  ImageRegionIterator< TOutputImage >     outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() )
    {
    const InputPixelType x = inputIt.Get();

    const RealType value  = static_cast< RealType >( x ) * m_Scale + m_Shift;

    OutputPixelType  result =  Math::Round< OutputPixelType >( value );

    result = ( result > m_OutputMaximum ) ? m_OutputMaximum : result;
    result = ( result < m_OutputMinimum ) ? m_OutputMinimum : result;

    outputIt.Set( result );

    ++inputIt;
    ++outputIt;

    progress.CompletedPixel();  // potential exception thrown here
    }
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
StretchIntensityImageFilter< TInputImage, TOutputImage >
::SetInput(const TInputImage *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0, const_cast< TInputImage * >( input ) );
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
const TInputImage *
StretchIntensityImageFilter< TInputImage, TOutputImage >
::GetInput(void) const
{
  return itkDynamicCastInDebugMode< const TInputImage * >( this->GetPrimaryInput() );
}

template< typename TInputImage, typename TOutputImage >
void
StretchIntensityImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Scale: "
     << static_cast< typename NumericTraits< RealType >::PrintType >( m_Scale )
     << std::endl;
  os << indent << "Shift: "
     << static_cast< typename NumericTraits< RealType >::PrintType >( m_Shift )
     << std::endl;

  os << indent << "Input Minimum: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_InputMinimum )
     << std::endl;
  os << indent << "Input Maximum: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_InputMaximum )
     << std::endl;
  os << indent << "Output Minimum: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutputMinimum )
     << std::endl;
  os << indent << "Output Maximum: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutputMaximum )
     << std::endl;
}
} // end namespace Testing
} // end namespace itk

#endif
