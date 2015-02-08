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
#ifndef itkDoubleThresholdImageFilter_hxx
#define itkDoubleThresholdImageFilter_hxx

#include "itkDoubleThresholdImageFilter.h"
#include "itkReconstructionByDilationImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
DoubleThresholdImageFilter< TInputImage, TOutputImage >
::DoubleThresholdImageFilter():
  m_NumberOfIterationsUsed(1)
{
  m_Threshold1 = NumericTraits< InputPixelType >::NonpositiveMin();
  m_Threshold2 = NumericTraits< InputPixelType >::NonpositiveMin();
  m_Threshold3 = NumericTraits< InputPixelType >::max();
  m_Threshold4 = NumericTraits< InputPixelType >::max();

  m_OutsideValue   = NumericTraits< OutputPixelType >::ZeroValue();
  m_InsideValue    = NumericTraits< OutputPixelType >::max();

  m_FullyConnected = false;
}

template< typename TInputImage, typename TOutputImage >
void
DoubleThresholdImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
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
DoubleThresholdImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
DoubleThresholdImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  // Build a mini-pipeline that involves two thresholds filters and a
  // geodesic dilation.
  typedef BinaryThresholdImageFilter< TInputImage, TOutputImage >           ThresholdFilterType;
  typedef ReconstructionByDilationImageFilter< TOutputImage, TOutputImage > DilationFilterType;

  typename ThresholdFilterType::Pointer narrowThreshold = ThresholdFilterType::New();

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  narrowThreshold->SetLowerThreshold(m_Threshold2);
  narrowThreshold->SetUpperThreshold(m_Threshold3);
  narrowThreshold->SetInsideValue(m_InsideValue);
  narrowThreshold->SetOutsideValue(m_OutsideValue);
  narrowThreshold->SetInput( this->GetInput() );

  typename ThresholdFilterType::Pointer wideThreshold = ThresholdFilterType::New();
  wideThreshold->SetLowerThreshold(m_Threshold1);
  wideThreshold->SetUpperThreshold(m_Threshold4);
  wideThreshold->SetInsideValue(m_InsideValue);
  wideThreshold->SetOutsideValue(m_OutsideValue);
  wideThreshold->SetInput( this->GetInput() );

  typename DilationFilterType::Pointer dilate = DilationFilterType::New();
  dilate->SetMarkerImage( narrowThreshold->GetOutput() );
  dilate->SetMaskImage( wideThreshold->GetOutput() );
  dilate->SetFullyConnected(m_FullyConnected);
  //dilate->RunOneIterationOff();   // run to convergence

  progress->RegisterInternalFilter(narrowThreshold, .1f);
  progress->RegisterInternalFilter(wideThreshold, .1f);
  progress->RegisterInternalFilter(dilate, .8f);

  // graft our output to the dilate filter to force the proper regions
  // to be generated
  dilate->GraftOutput( this->GetOutput() );

  // reconstruction by dilation
  dilate->Update();

  // graft the output of the dilate filter back onto this filter's
  // output. this is needed to get the appropriate regions passed
  // back.
  this->GraftOutput( dilate->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
DoubleThresholdImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold1: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Threshold1 )
     << std::endl;
  os << indent << "Threshold2: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Threshold2 )
     << std::endl;
  os << indent << "Threshold3: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Threshold3 )
     << std::endl;
  os << indent << "Threshold4: "
     << static_cast< typename NumericTraits< InputPixelType >::PrintType >( m_Threshold4 )
     << std::endl;
  os << indent << "InsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_InsideValue )
     << std::endl;
  os << indent << "OutsideValue: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutsideValue )
     << std::endl;
  os << indent << "Number of iterations used to produce current output: "
     << m_NumberOfIterationsUsed << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}
} // end namespace itk
#endif
