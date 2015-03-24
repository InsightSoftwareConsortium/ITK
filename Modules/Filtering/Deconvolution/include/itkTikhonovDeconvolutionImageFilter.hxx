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
#ifndef itkTikhonovDeconvolutionImageFilter_hxx
#define itkTikhonovDeconvolutionImageFilter_hxx

#include "itkTikhonovDeconvolutionImageFilter.h"

#include "itkBinaryFunctorImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
TikhonovDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
::TikhonovDeconvolutionImageFilter()
{
  m_RegularizationConstant = 0.0;
}

template< typename TInputImage, typename TKernelImage, typename TOutputImage, typename TInternalPrecision >
void
TikhonovDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision >
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

  typedef Functor::TikhonovDeconvolutionFunctor< InternalComplexType,
                         InternalComplexType,
                         InternalComplexType> FunctorType;
  typedef BinaryFunctorImageFilter< InternalComplexImageType,
                               InternalComplexImageType,
                               InternalComplexImageType, FunctorType > TikhonovFilterType;
  typename TikhonovFilterType::Pointer tikhonovFilter = TikhonovFilterType::New();
  tikhonovFilter->SetInput1( input );
  tikhonovFilter->SetInput2( kernel );
  tikhonovFilter->ReleaseDataFlagOn();

  typename TikhonovFilterType::FunctorType & tikhonovFunctor = tikhonovFilter->GetFunctor();
  tikhonovFunctor.SetRegularizationConstant( this->GetRegularizationConstant() );
  tikhonovFunctor.SetKernelZeroMagnitudeThreshold( this->GetKernelZeroMagnitudeThreshold() );
  progress->RegisterInternalFilter( tikhonovFilter, 0.1 );

  // Free up the memory for the prepared inputs
  input = ITK_NULLPTR;
  kernel = ITK_NULLPTR;

  this->ProduceOutput( tikhonovFilter->GetOutput(), progress, 0.2 );
}

template< typename TInputImage, typename TOutputImage, typename TKernelImage, typename TInternalPrecision >
void
TikhonovDeconvolutionImageFilter< TInputImage, TOutputImage, TKernelImage, TInternalPrecision >
::PrintSelf(std::ostream &os, Indent indent) const
{
  this->Superclass::PrintSelf( os, indent );

  os << indent << "RegularizationConstant: " << m_RegularizationConstant << std::endl;
}

}  // end namespace itk
#endif
