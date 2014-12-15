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
#ifndef itkBinaryOpeningByReconstructionImageFilter_hxx
#define itkBinaryOpeningByReconstructionImageFilter_hxx

#include "itkBinaryOpeningByReconstructionImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryReconstructionByDilationImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template<typename TInputImage, typename TKernel>
BinaryOpeningByReconstructionImageFilter<TInputImage, TKernel>
::BinaryOpeningByReconstructionImageFilter()
{
  m_ForegroundValue = NumericTraits<PixelType>::max();
  m_BackgroundValue = NumericTraits<PixelType>::ZeroValue();
  m_FullyConnected = false;
}

template <typename TInputImage, typename TKernel>
void
BinaryOpeningByReconstructionImageFilter<TInputImage, TKernel>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template<typename TInputImage, typename TKernel>
void
BinaryOpeningByReconstructionImageFilter<TInputImage, TKernel>
::GenerateData()
{
  // Allocate the outputs
  this->AllocateOutputs();

  /** set up erosion and dilation methods */
  typename BinaryErodeImageFilter<InputImageType, OutputImageType, TKernel>::Pointer
    erode = BinaryErodeImageFilter<InputImageType, OutputImageType, TKernel>::New();
  erode->SetErodeValue( m_ForegroundValue );
  erode->SetBackgroundValue( m_BackgroundValue );
  erode->SetKernel( this->GetKernel() );
  erode->SetInput( this->GetInput() );
  erode->ReleaseDataFlagOn();
  erode->SetNumberOfThreads( this->GetNumberOfThreads() );

  typename BinaryReconstructionByDilationImageFilter<OutputImageType>::Pointer
    dilate = BinaryReconstructionByDilationImageFilter<OutputImageType>::New();
  dilate->SetForegroundValue( m_ForegroundValue );
  dilate->SetBackgroundValue( m_BackgroundValue );
  dilate->SetMarkerImage( erode->GetOutput() );
  dilate->SetMaskImage( this->GetInput() );
  dilate->SetFullyConnected( m_FullyConnected );
  dilate->ReleaseDataFlagOn();
  dilate->SetNumberOfThreads( this->GetNumberOfThreads() );

  /** set up the minipipeline */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, .8f);
  progress->RegisterInternalFilter(dilate, .2f);

  /** execute the minipipeline */
  dilate->GraftOutput( this->GetOutput() );
  dilate->Update();
  this->GraftOutput( dilate->GetOutput() );
}

template<typename TInputImage, typename TKernel>
void
BinaryOpeningByReconstructionImageFilter<TInputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_ForegroundValue) << std::endl;
  os << indent << "BackgroundValue: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_BackgroundValue) << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}

}// end namespace itk
#endif
