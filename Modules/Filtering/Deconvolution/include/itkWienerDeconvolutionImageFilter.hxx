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
#ifndef itkWienerDeconvolutionImageFilter_hxx
#define itkWienerDeconvolutionImageFilter_hxx

#include "itkWienerDeconvolutionImageFilter.h"

#include "itkBinaryFunctorImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
WienerDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::WienerDeconvolutionImageFilter()
{
  m_NoiseVariance = 0.0;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
WienerDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this
  // minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter( this );

  typename InputImageType::Pointer localInput = InputImageType::New();
  localInput->Graft( this->GetInput() );

  const KernelImageType* kernelImage = this->GetKernelImage();

  InternalComplexImagePointerType input = ITK_NULLPTR;
  InternalComplexImagePointerType kernel = ITK_NULLPTR;

  this->PrepareInputs( localInput, kernelImage, input, kernel, progress, 0.7 );

  typedef Functor::WienerDeconvolutionFunctor< InternalComplexType > FunctorType;
  typedef BinaryFunctorImageFilter< InternalComplexImageType,
                                    InternalComplexImageType,
                                    InternalComplexImageType,
                                    FunctorType > WienerFilterType;
  typename WienerFilterType::Pointer wienerFilter = WienerFilterType::New();
  wienerFilter->SetInput( 0, input );
  wienerFilter->SetInput( 1, kernel );

  typename WienerFilterType::FunctorType & wienerFunctor = wienerFilter->GetFunctor();
  wienerFunctor.SetNoisePowerSpectralDensityConstant( m_NoiseVariance );
  wienerFunctor.SetKernelZeroMagnitudeThreshold( this->GetKernelZeroMagnitudeThreshold() );
  wienerFilter->ReleaseDataFlagOn();
  progress->RegisterInternalFilter( wienerFilter, 0.1 );

  // Free up the memory for the prepared inputs
  input = ITK_NULLPTR;
  kernel = ITK_NULLPTR;

  this->ProduceOutput( wienerFilter->GetOutput(), progress, 0.2 );
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
WienerDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf( os, indent );

  os << indent << "NoiseVariance: " << m_NoiseVariance << std::endl;
}

}  // end namespace itk
#endif
