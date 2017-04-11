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
#ifndef itkHMinimaImageFilter_hxx
#define itkHMinimaImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkHMinimaImageFilter.h"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
HMinimaImageFilter< TInputImage, TOutputImage >
::HMinimaImageFilter() :
  m_Height( 2 ),
  m_NumberOfIterationsUsed( 1 ),
  m_FullyConnected( false )
{
}

template< typename TInputImage, typename TOutputImage >
void
HMinimaImageFilter< TInputImage, TOutputImage >
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
HMinimaImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
HMinimaImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  // Construct a marker image to manipulate using reconstruction by
  // erosion. the marker image is the input image minus the height
  // parameter.
  typedef ShiftScaleImageFilter< TInputImage, TInputImage > ShiftFilterType;
  typename ShiftFilterType::Pointer shift = ShiftFilterType::New();
  shift->SetInput( this->GetInput() );
  shift->SetShift( static_cast< typename ShiftFilterType::RealType >( m_Height ) );

  // Delegate to a geodesic erosion filter.
  //
  //
  typename ReconstructionByErosionImageFilter< TInputImage, TInputImage >::Pointer
  erode =
    ReconstructionByErosionImageFilter< TInputImage, TInputImage >::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, 1.0f);

  // Set up the erode filter
  //erode->RunOneIterationOff();             // run to convergence
  erode->SetMarkerImage( shift->GetOutput() );
  erode->SetMaskImage( this->GetInput() );
  erode->SetFullyConnected(m_FullyConnected);

  // Must cast to the output type
  typename CastImageFilter< TInputImage, TOutputImage >::Pointer cast =
    CastImageFilter< TInputImage, TOutputImage >::New();
  cast->SetInput( erode->GetOutput() );
  cast->InPlaceOn();

  // Graft our output to the cast filter to force the proper regions
  // to be generated
  cast->GraftOutput( this->GetOutput() );

  // Reconstruction by erosion
  cast->Update();

  // Graft the output of the erode filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( cast->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
HMinimaImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Depth of local maxima (contrast): "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_Height )
     << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
} // end namespace itk
#endif
