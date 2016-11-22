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
#ifndef itkThresholdImageFilter_hxx
#define itkThresholdImageFilter_hxx

#include "itkThresholdImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkNumericTraits.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{

template< typename TImage >
ThresholdImageFilter< TImage >
::ThresholdImageFilter() :
  m_OutsideValue( NumericTraits< PixelType >::ZeroValue() ),
  m_Lower( NumericTraits< PixelType >::NonpositiveMin() ),
  m_Upper( NumericTraits< PixelType >::max() )
{
  this->InPlaceOff();
}

template< typename TImage >
void
ThresholdImageFilter< TImage >
::ThresholdAbove(const PixelType & thresh)
{
  if ( Math::NotExactlyEquals(m_Upper, thresh)
       || m_Lower > NumericTraits< PixelType >::NonpositiveMin() )
    {
    m_Lower = NumericTraits< PixelType >::NonpositiveMin();
    m_Upper = thresh;
    this->Modified();
    }
}

template< typename TImage >
void
ThresholdImageFilter< TImage >
::ThresholdBelow(const PixelType & thresh)
{
  if ( Math::NotExactlyEquals(m_Lower, thresh) || m_Upper < NumericTraits< PixelType >::max() )
    {
    m_Lower = thresh;
    m_Upper = NumericTraits< PixelType >::max();
    this->Modified();
    }
}

template< typename TImage >
void
ThresholdImageFilter< TImage >
::ThresholdOutside(const PixelType & lower, const PixelType & upper)
{
  if ( lower > upper )
    {
    itkExceptionMacro(<< "Lower threshold cannot be greater than upper threshold.");
    return;
    }

  if ( Math::NotExactlyEquals(m_Lower, lower) || Math::NotExactlyEquals(m_Upper, upper) )
    {
    m_Lower = lower;
    m_Upper = upper;
    this->Modified();
    }
}

template< typename TImage >
void
ThresholdImageFilter< TImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if( size0 == 0 )
    {
    return;
    }

  // Get the input and output pointers
  InputImagePointer  inputPtr  = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput(0);

  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef ImageScanlineConstIterator< TImage > InputIterator;
  typedef ImageScanlineIterator< TImage >      OutputIterator;

  InputIterator  inIt(inputPtr, outputRegionForThread);
  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Support progress methods/callbacks
  const size_t numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / size0;
  ProgressReporter progress( this, threadId, static_cast<SizeValueType>(  numberOfLinesToProcess ) );

  // Walk the regions; threshold each pixel
  while ( !outIt.IsAtEnd() )
    {
    while ( !outIt.IsAtEndOfLine() )
      {
      const PixelType value = inIt.Get();
      if ( m_Lower <= value && value <= m_Upper )
        {
        // Pixel passes to output unchanged and is replaced by m_OutsideValue in
        // the inverse output image
        outIt.Set( inIt.Get() );
        }
      else
        {
        outIt.Set(m_OutsideValue);
        }
      ++inIt;
      ++outIt;
      }
    inIt.NextLine();
    outIt.NextLine();
    progress.CompletedPixel();
    }
}

template< typename TImage >
void
ThresholdImageFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "OutsideValue: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_OutsideValue )
     << std::endl;
  os << indent << "Lower: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_Lower )
     << std::endl;
  os << indent << "Upper: "
     << static_cast< typename NumericTraits< PixelType >::PrintType >( m_Upper )
     << std::endl;
}
} // end namespace itk

#endif
