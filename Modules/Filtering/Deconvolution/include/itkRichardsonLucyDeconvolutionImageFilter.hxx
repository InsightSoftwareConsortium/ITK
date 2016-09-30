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
#ifndef itkRichardsonLucyDeconvolutionImageFilter_hxx
#define itkRichardsonLucyDeconvolutionImageFilter_hxx

#include "itkRichardsonLucyDeconvolutionImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
RichardsonLucyDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::RichardsonLucyDeconvolutionImageFilter()
{
  m_PaddedInput = ITK_NULLPTR;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
RichardsonLucyDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::~RichardsonLucyDeconvolutionImageFilter()
{
  m_PaddedInput = ITK_NULLPTR;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
RichardsonLucyDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::Initialize(ProgressAccumulator * progress, float progressWeight,
             float iterationProgressWeight)
{
  this->Superclass::Initialize( progress, 0.5f * progressWeight,
                                iterationProgressWeight );

  this->PadInput( this->GetInput(), m_PaddedInput, progress,
                  0.5f * progressWeight );

  // Set up minipipeline to compute estimate at each iteration
  m_ComplexMultiplyFilter1 = ComplexMultiplyType::New();
  m_ComplexMultiplyFilter1->SetNumberOfThreads( this->GetNumberOfThreads() );
  // Transformed estimate will be set as input 1 in Iteration()
  m_ComplexMultiplyFilter1->SetInput2( this->m_TransferFunction );
  m_ComplexMultiplyFilter1->InPlaceOn();
  m_ComplexMultiplyFilter1->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( m_ComplexMultiplyFilter1,
                                    0.07f * iterationProgressWeight );

  m_IFFTFilter1 = IFFTFilterType::New();
  m_IFFTFilter1->SetNumberOfThreads( this->GetNumberOfThreads() );
  m_IFFTFilter1->SetActualXDimensionIsOdd( this->GetXDimensionIsOdd() );
  m_IFFTFilter1->SetInput( m_ComplexMultiplyFilter1->GetOutput() );
  m_IFFTFilter1->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( m_IFFTFilter1,
                                    0.2f * iterationProgressWeight );

  m_DivideFilter = DivideFilterType::New();
  m_DivideFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  m_DivideFilter->SetInput1( m_PaddedInput );
  m_DivideFilter->SetInput2( m_IFFTFilter1->GetOutput() );
  m_DivideFilter->InPlaceOn();
  m_DivideFilter->SetCoordinateTolerance( NumericTraits< double >::max() );
  m_DivideFilter->SetDirectionTolerance( NumericTraits< double >::max() );
  progress->RegisterInternalFilter( m_DivideFilter,
                                    0.1f * iterationProgressWeight );

  m_FFTFilter = FFTFilterType::New();
  m_FFTFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  m_FFTFilter->SetInput( m_DivideFilter->GetOutput() );
  m_FFTFilter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( m_FFTFilter,
                                    0.2f * iterationProgressWeight );

  m_ConjugateAdaptor = ConjugateAdaptorType::New();
  m_ConjugateAdaptor->SetImage( this->m_TransferFunction );

  m_ComplexMultiplyFilter2 = ComplexConjugateMultiplyType::New();
  m_ComplexMultiplyFilter2->SetNumberOfThreads( this->GetNumberOfThreads() );
  m_ComplexMultiplyFilter2->SetInput1( m_FFTFilter->GetOutput() );
  m_ComplexMultiplyFilter2->SetInput2( m_ConjugateAdaptor );
  m_ComplexMultiplyFilter2->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( m_ComplexMultiplyFilter2,
                                    0.07f * iterationProgressWeight );

  m_IFFTFilter2 = IFFTFilterType::New();
  m_IFFTFilter2->SetNumberOfThreads( this->GetNumberOfThreads() );
  m_IFFTFilter2->SetActualXDimensionIsOdd( this->GetXDimensionIsOdd() );
  m_IFFTFilter2->SetInput( m_ComplexMultiplyFilter2->GetOutput() );
  m_IFFTFilter2->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( m_IFFTFilter2,
                                    0.2f * iterationProgressWeight );

  m_MultiplyFilter = MultiplyFilterType::New();
  m_MultiplyFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
  // Current estimate will be set as input 1 in Iteration()
  m_MultiplyFilter->SetInput2( m_IFFTFilter2->GetOutput() );
  m_MultiplyFilter->InPlaceOn();
  m_MultiplyFilter->ReleaseDataFlagOn();
  m_MultiplyFilter->SetCoordinateTolerance( NumericTraits< double >::max() );
  m_MultiplyFilter->SetDirectionTolerance( NumericTraits< double >::max() );
  progress->RegisterInternalFilter( m_MultiplyFilter,
                                    0.06f * iterationProgressWeight );

}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
RichardsonLucyDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::Iteration(ProgressAccumulator * progress, float iterationProgressWeight)
{
  // Set up minipipeline to compute the new estimate
  InternalComplexImagePointerType transformedEstimate;
  this->TransformPaddedInput( this->m_CurrentEstimate,
                              transformedEstimate, progress,
                              0.1f * iterationProgressWeight );

  // Set the inputs
  m_ComplexMultiplyFilter1->SetInput1( transformedEstimate );
  m_MultiplyFilter->SetInput1( this->m_CurrentEstimate );

  // Trigger the update
  m_MultiplyFilter->UpdateLargestPossibleRegion();

  // Store the current estimate
  this->m_CurrentEstimate = m_MultiplyFilter->GetOutput();
  this->m_CurrentEstimate->DisconnectPipeline();
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
RichardsonLucyDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::Finish(ProgressAccumulator * progress, float progressWeight)
{
  this->Superclass::Finish( progress, progressWeight );

  m_ComplexMultiplyFilter1 = ITK_NULLPTR;
  m_IFFTFilter1 = ITK_NULLPTR;
  m_DivideFilter = ITK_NULLPTR;
  m_FFTFilter = ITK_NULLPTR;
  m_ConjugateAdaptor = ITK_NULLPTR;
  m_ComplexMultiplyFilter2 = ITK_NULLPTR;
  m_IFFTFilter2 = ITK_NULLPTR;
  m_MultiplyFilter = ITK_NULLPTR;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
RichardsonLucyDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
