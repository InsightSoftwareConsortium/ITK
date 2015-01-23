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
#ifndef itkBinaryClosingByReconstructionImageFilter_hxx
#define itkBinaryClosingByReconstructionImageFilter_hxx

#include "itkBinaryClosingByReconstructionImageFilter.h"
#include "itkBinaryReconstructionByErosionImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkProgressAccumulator.h"
#include "itkCropImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"

namespace itk {

template<typename TInputImage, typename TKernel>
BinaryClosingByReconstructionImageFilter<TInputImage, TKernel>
::BinaryClosingByReconstructionImageFilter()
{
  m_ForegroundValue = NumericTraits<InputPixelType>::max();
  m_FullyConnected = false;
}

template <typename TInputImage, typename TKernel>
void
BinaryClosingByReconstructionImageFilter<TInputImage, TKernel>
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
BinaryClosingByReconstructionImageFilter<TInputImage, TKernel>
::GenerateData()
{
  // Allocate the outputs
  this->AllocateOutputs();

  // let choose a background value. Background value should not be given by user
  // because closing is extensive so no background pixels will be added
  // it is just needed for internal erosion filter and constant padder
  InputPixelType backgroundValue = NumericTraits<InputPixelType>::ZeroValue();
  if ( m_ForegroundValue == backgroundValue )
    {
    // current background value is already used for foreground value
    // choose another one
    backgroundValue = NumericTraits<InputPixelType>::max();
    }

  /** set up erosion and dilation methods */
  typename BinaryDilateImageFilter<TInputImage, TInputImage, TKernel>::Pointer
    dilate = BinaryDilateImageFilter<TInputImage, TInputImage, TKernel>::New();

  typename BinaryReconstructionByErosionImageFilter<OutputImageType>::Pointer
    erode = BinaryReconstructionByErosionImageFilter<OutputImageType>::New();

  // create the pipeline without input and output image
  dilate->ReleaseDataFlagOn();
  dilate->SetKernel( this->GetKernel() );
  dilate->SetDilateValue( m_ForegroundValue );
  dilate->SetBackgroundValue( backgroundValue );
  dilate->SetInput( this->GetInput() );
  dilate->SetNumberOfThreads( this->GetNumberOfThreads() );

  erode->ReleaseDataFlagOn();
  erode->SetForegroundValue( m_ForegroundValue );
  erode->SetBackgroundValue( backgroundValue );
  erode->SetMarkerImage( dilate->GetOutput() );
  erode->SetFullyConnected( m_FullyConnected );
  erode->SetMaskImage( this->GetInput() );
  erode->SetNumberOfThreads( this->GetNumberOfThreads() );


  /** set up the minipipeline */
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(erode, .8f);
  progress->RegisterInternalFilter(dilate, .2f);

  /** execute the minipipeline */
  erode->GraftOutput( this->GetOutput() );
  erode->Update();
  this->GraftOutput( erode->GetOutput() );
}

template<typename TInputImage, typename TKernel>
void
BinaryClosingByReconstructionImageFilter<TInputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: " << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_ForegroundValue) << std::endl;
  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
}

}// end namespace itk
#endif
