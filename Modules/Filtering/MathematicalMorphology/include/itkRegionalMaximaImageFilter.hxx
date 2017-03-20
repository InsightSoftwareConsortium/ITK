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
#ifndef itkRegionalMaximaImageFilter_hxx
#define itkRegionalMaximaImageFilter_hxx

#include "itkRegionalMaximaImageFilter.h"
#include "itkValuedRegionalMaximaImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkNumericTraits.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIterator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
RegionalMaximaImageFilter< TInputImage, TOutputImage >
::RegionalMaximaImageFilter():
  m_FullyConnected( false ),
  m_FlatIsMaxima( true ),
  m_ForegroundValue( NumericTraits< OutputImagePixelType >::max() ),
  m_BackgroundValue( NumericTraits< OutputImagePixelType >::NonpositiveMin() )
{
}


template< typename TInputImage, typename TOutputImage >
void
RegionalMaximaImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImageType * input = const_cast< InputImageType * >( this->GetInput() );
  if ( !input )
    {
    return;
    }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}


template< typename TInputImage, typename TOutputImage >
void
RegionalMaximaImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  OutputImageType * output = this->GetOutput();
  output->SetRequestedRegion( output->GetLargestPossibleRegion() );
}


template< typename TInputImage, typename TOutputImage >
void
RegionalMaximaImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  const InputImageType * input = this->GetInput();
  OutputImageType * output = this->GetOutput();

  // Delegate to the valued filter to find the minima
  typename ValuedRegionalMaximaImageFilter< TInputImage, TInputImage >::Pointer
  regionalMax = ValuedRegionalMaximaImageFilter< TInputImage, TInputImage >::New();
  regionalMax->SetInput( input );
  regionalMax->SetFullyConnected( m_FullyConnected );
  progress->RegisterInternalFilter( regionalMax, 0.67f );
  regionalMax->Update();

  if( regionalMax->GetFlat() )
    {
    ProgressReporter progress2(this, 0,
                               output->GetRequestedRegion().GetNumberOfPixels(),
                               33, 0.67, 0.33);

    ImageRegionIterator< TOutputImage >
    outIt( output, output->GetRequestedRegion() );

    if ( m_FlatIsMaxima )
      {
      for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
        {
        outIt.Set( m_ForegroundValue );
        progress2.CompletedPixel();
        }
      }
    else
      {
      for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
        {
        outIt.Set( m_BackgroundValue );
        progress2.CompletedPixel();
        }
      }
    }
  else
    {
    typedef BinaryThresholdImageFilter< InputImageType, OutputImageType > ThresholdType;
    typename ThresholdType::Pointer thresholder = ThresholdType::New();
    thresholder->SetInput( regionalMax->GetOutput() );
    thresholder->SetUpperThreshold( regionalMax->GetMarkerValue() );
    thresholder->SetLowerThreshold( regionalMax->GetMarkerValue() );
    thresholder->SetOutsideValue( m_ForegroundValue );
    thresholder->SetInsideValue( m_BackgroundValue );
    progress->RegisterInternalFilter( thresholder, 0.33f );

    thresholder->GraftOutput( output );
    thresholder->Update();
    this->GraftOutput( thresholder->GetOutput() );
    }
}


template< typename TInputImage, typename TOutputImage >
void
RegionalMaximaImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "FlatIsMaxima: "    << m_FlatIsMaxima << std::endl;
  os << indent << "ForegroundValue: "
    << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >(
    m_ForegroundValue ) << std::endl;
  os << indent << "BackgroundValue: "
    << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >(
    m_BackgroundValue ) << std::endl;
}

} // end namespace itk
#endif
