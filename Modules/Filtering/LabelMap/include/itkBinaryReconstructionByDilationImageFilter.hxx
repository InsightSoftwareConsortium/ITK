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
#ifndef itkBinaryReconstructionByDilationImageFilter_hxx
#define itkBinaryReconstructionByDilationImageFilter_hxx

#include "itkBinaryReconstructionByDilationImageFilter.h"
#include "itkProgressAccumulator.h"


namespace itk {

template<typename TInputImage>
BinaryReconstructionByDilationImageFilter<TInputImage>
::BinaryReconstructionByDilationImageFilter()
{
  m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
  m_ForegroundValue = NumericTraits<OutputImagePixelType>::max();
  m_FullyConnected = false;
  this->SetPrimaryInputName( "MarkerImage" );
  this->AddRequiredInputName( "MaskImage", 1 );
}

template<typename TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::SetMarkerImage( const InputImageType * input )
{
  // Process object is not const-correct, so the const casting is required.
  this->ProcessObject::SetInput( "MarkerImage", const_cast< InputImageType * >( input ));
}

template<typename TInputImage>
typename BinaryReconstructionByDilationImageFilter<TInputImage>::InputImageType *
BinaryReconstructionByDilationImageFilter<TInputImage>
::GetMarkerImage()
{
  return static_cast<InputImageType*>(const_cast<DataObject *>(this->ProcessObject::GetInput( "MarkerImage" )));
}

template<typename TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::SetMaskImage( const InputImageType * input )
{
  // Process object is not const-correct, so the const casting is required.
  this->ProcessObject::SetInput( "MaskImage", const_cast< InputImageType * >( input ));
}

template<typename TInputImage>
typename BinaryReconstructionByDilationImageFilter<TInputImage>::InputImageType *
BinaryReconstructionByDilationImageFilter<TInputImage>
::GetMaskImage()
{
  return static_cast<InputImageType*>(const_cast<DataObject *>(this->ProcessObject::GetInput( "MaskImage" )));
}

template<typename TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetMarkerImage());
  if( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }

  input = const_cast<InputImageType *>(this->GetMaskImage());
  if( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}


template<typename TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
    ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}


template<typename TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  typename LabelizerType::Pointer labelizer = LabelizerType::New();
  labelizer->SetInput( this->GetMaskImage() );
  labelizer->SetInputForegroundValue( m_ForegroundValue );
  labelizer->SetOutputBackgroundValue( m_BackgroundValue );
  labelizer->SetFullyConnected( m_FullyConnected );
  labelizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(labelizer, .25f);

  typename ReconstructionType::Pointer reconstruction = ReconstructionType::New();
  reconstruction->SetInput( labelizer->GetOutput() );
  reconstruction->SetMarkerImage( this->GetMarkerImage() );
  reconstruction->SetForegroundValue( m_ForegroundValue );
  reconstruction->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(reconstruction, .25f);

  typename OpeningType::Pointer opening = OpeningType::New();
  opening->SetInput( reconstruction->GetOutput() );
  opening->SetLambda( true );
  opening->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(opening, .25f);

  typename BinarizerType::Pointer binarizer = BinarizerType::New();
  binarizer->SetInput( opening->GetOutput() );
  binarizer->SetForegroundValue( m_ForegroundValue );
  binarizer->SetBackgroundValue( m_BackgroundValue );
  binarizer->SetBackgroundImage( this->GetMaskImage() );
  binarizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(binarizer, .5f);

  binarizer->GraftOutput( this->GetOutput() );
  binarizer->Update();
  this->GraftOutput( binarizer->GetOutput() );
}


template<typename TInputImage>
void
BinaryReconstructionByDilationImageFilter<TInputImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "BackgroundValue: "  << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue) << std::endl;
  os << indent << "ForegroundValue: "  << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ForegroundValue) << std::endl;
}

}// end namespace itk
#endif
