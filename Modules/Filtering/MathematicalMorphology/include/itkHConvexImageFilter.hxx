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
#ifndef itkHConvexImageFilter_hxx
#define itkHConvexImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkHConvexImageFilter.h"
#include "itkHMaximaImageFilter.h"
#include "itkSubtractImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
HConvexImageFilter< TInputImage, TOutputImage >
::HConvexImageFilter() :
  m_Height( 2 ),
  m_NumberOfIterationsUsed( 1 ),
  m_FullyConnected( false )
{
}

template< typename TInputImage, typename TOutputImage >
void
HConvexImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template< typename TInputImage, typename TOutputImage >
void
HConvexImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
HConvexImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to a H-Maxima filter.
  //
  //
  typename HMaximaImageFilter< TInputImage, TInputImage >::Pointer
  hmax = HMaximaImageFilter< TInputImage, TInputImage >::New();

  hmax->SetInput( this->GetInput() );
  hmax->SetHeight(m_Height);
  hmax->SetFullyConnected(m_FullyConnected);

  // Need to subtract the H-Maxima image from the input
  typename SubtractImageFilter< TInputImage, TInputImage, TOutputImage >::Pointer
  subtract = SubtractImageFilter< TInputImage, TInputImage, TOutputImage >::New();

  subtract->SetInput1( this->GetInput() );
  subtract->SetInput2( hmax->GetOutput() );

  // Graft our output to the subtract filter to force the proper regions
  // to be generated
  subtract->GraftOutput( this->GetOutput() );

  // Run the algorithm
  progress->RegisterInternalFilter(hmax, .9f);
  progress->RegisterInternalFilter(subtract, .1f);

  subtract->Update();

  // Graft the output of the subtract filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( subtract->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
HConvexImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Height of local maxima (contrast): "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_Height )
     << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
} // end namespace itk
#endif
