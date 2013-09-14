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
#ifndef __itkRegionalMinimaImageFilter_hxx
#define __itkRegionalMinimaImageFilter_hxx

#include "itkRegionalMinimaImageFilter.h"
#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkNumericTraits.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIterator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
RegionalMinimaImageFilter< TInputImage, TOutputImage >
::RegionalMinimaImageFilter()
{
  m_FullyConnected = false;
  m_FlatIsMinima = true;
  m_ForegroundValue = NumericTraits< OutputImagePixelType >::max();
  m_BackgroundValue = NumericTraits< OutputImagePixelType >::NonpositiveMin();
}

template< typename TInputImage, typename TOutputImage >
void
RegionalMinimaImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( !input )
    {
    return;
    }
  input->SetRequestedRegion( input->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
RegionalMinimaImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
RegionalMinimaImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to the valued filter to find the minima
  typename ValuedRegionalMinimaImageFilter< TInputImage, TInputImage >::Pointer
  rmin = ValuedRegionalMinimaImageFilter< TInputImage, TInputImage >::New();
  rmin->SetInput( this->GetInput() );
  rmin->SetFullyConnected(m_FullyConnected);
  progress->RegisterInternalFilter(rmin, 0.67f);
  rmin->Update();

  OutputImageType *outputImage = this->GetOutput();

  if ( rmin->GetFlat() )
    {
    ProgressReporter progress2(
      this, 0,
      outputImage->GetRequestedRegion().GetNumberOfPixels(),
      33, 0.67, 0.33);

    typedef ImageRegionIterator< TOutputImage > IteratorType;

    IteratorType outIt( outputImage, outputImage->GetRequestedRegion() );

    if ( m_FlatIsMinima )
      {
      for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
        {
        outIt.Set(m_ForegroundValue);
        progress2.CompletedPixel();
        }
      }
    else
      {
      for ( outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt )
        {
        outIt.Set(m_BackgroundValue);
        progress2.CompletedPixel();
        }
      }
    }
  else
    {
    typedef BinaryThresholdImageFilter<
      InputImageType, OutputImageType > ThresholdType;

    typename ThresholdType::Pointer threshold = ThresholdType::New();

    threshold->SetInput( rmin->GetOutput() );
    threshold->SetUpperThreshold( rmin->GetMarkerValue() );
    threshold->SetLowerThreshold( rmin->GetMarkerValue() );
    threshold->SetOutsideValue(m_ForegroundValue);
    threshold->SetInsideValue(m_BackgroundValue);
    progress->RegisterInternalFilter(threshold, 0.33f);

    threshold->GraftOutput(outputImage);
    threshold->Update();
    this->GraftOutput( threshold->GetOutput() );
    }
}

template< typename TInputImage, typename TOutputImage >
void
RegionalMinimaImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "FlatIsMinima: "  << m_FlatIsMinima << std::endl;
  os << indent << "ForegroundValue: " << m_ForegroundValue << std::endl;
  os << indent << "BackgroundValue: " << m_BackgroundValue << std::endl;
}
} // end namespace itk
#endif
