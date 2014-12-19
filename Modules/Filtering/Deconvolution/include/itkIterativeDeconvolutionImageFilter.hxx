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
#ifndef itkIterativeDeconvolutionImageFilter_hxx
#define itkIterativeDeconvolutionImageFilter_hxx

#include "itkCastImageFilter.h"
#include "itkIterativeDeconvolutionImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::IterativeDeconvolutionImageFilter()
{
  m_NumberOfIterations = 1;
  m_Iteration = 0;
  m_StopIteration = false;
  m_TransferFunction = ITK_NULLPTR;
  m_CurrentEstimate = ITK_NULLPTR;
  m_InputMTime = 0L;
  m_KernelMTime = 0L;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::~IterativeDeconvolutionImageFilter()
{
  m_TransferFunction = ITK_NULLPTR;
  m_CurrentEstimate = ITK_NULLPTR;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::Initialize(ProgressAccumulator * progress, float progressWeight,
             float itkNotUsed(iterationProgressWeight))
{
  // Generate an estimate if there is none or if the input has changed.
  if ( !this->m_CurrentEstimate ||
       m_InputMTime != this->GetInput()->GetMTime() )
    {
    this->PadInput( this->GetInput(), m_CurrentEstimate, progress,
                    0.5f * progressWeight );
    m_CurrentEstimate->DisconnectPipeline();

    m_InputMTime = this->GetInput()->GetMTime();
    }

  // Generate the transfer function if there is none or if the kernel
  // input has changed.
  if ( !this->m_TransferFunction ||
       m_KernelMTime != this->GetKernelImage()->GetMTime() )
    {
    this->PrepareKernel( this->GetKernelImage(), m_TransferFunction,
                         progress, 0.5f * progressWeight );
    m_TransferFunction->DisconnectPipeline();

    m_KernelMTime = this->GetKernelImage()->GetMTime();
    }
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::Finish(ProgressAccumulator * progress, float progressWeight)
{
  this->CropOutput( m_CurrentEstimate, progress, progressWeight );

  m_CurrentEstimate = ITK_NULLPTR;
  m_TransferFunction = ITK_NULLPTR;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GenerateInputRequestedRegion()
{
  // Request the largest possible region for both input images.
  if ( this->GetInput() )
    {
    typename InputImageType::Pointer imagePtr =
      const_cast< InputImageType * >( this->GetInput() );
    imagePtr->SetRequestedRegionToLargestPossibleRegion();
    }

  if ( this->GetKernelImage() )
    {
    // Input kernel is an image, cast away the constness so we can set
    // the requested region.
    typename KernelImageType::Pointer kernelPtr =
      const_cast< KernelImageType * >( this->GetKernelImage() );
    kernelPtr->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter( this );

  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput(0);

  outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  outputPtr->Allocate();

  // Set up progress tracking
  float iterationWeight = 0.8f / static_cast< float >( m_NumberOfIterations );
  this->Initialize( progress, 0.1f, iterationWeight );

  for ( m_Iteration = 0; m_Iteration < m_NumberOfIterations; ++m_Iteration )
    {
    this->InvokeEvent( IterationEvent() );
    if ( m_StopIteration ) break;

    this->Iteration( progress, iterationWeight );
    }

  this->Finish(progress, 0.1f);
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
IterativeDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf( os, indent );

  os << indent << "NumberOfIterations: " << m_NumberOfIterations << std::endl;
  os << indent << "Iteration: " << m_Iteration << std::endl;
  os << indent << "StopIteration: " << m_StopIteration << std::endl;
  os << indent << "InputMTime: " << m_InputMTime << std::endl;
  os << indent << "KernelMTime: " << m_KernelMTime << std::endl;
}

} // end namespace itk

#endif
