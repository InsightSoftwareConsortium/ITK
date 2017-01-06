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
#ifndef itkUnsharpMaskImageFilter_hxx
#define itkUnsharpMaskImageFilter_hxx

#include "itkUnsharpMaskImageFilter.h"
#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressAccumulator.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage, typename TInternalPrecision >
UnsharpMaskImageFilter< TInputImage, TOutputImage, TInternalPrecision >
::UnsharpMaskImageFilter()
  :m_Amount(0.5),
  m_Threshold(0),
  m_Clamp(NumericTraits<OutputPixelType>::IsInteger)
  // clamping is on for integral types, and off for floating types
  // this gives intuitive behavior for integral types
  // and skips min/max checks for floating types
{
  m_Sigmas.Fill(1.0);
}

template< typename TInputImage, typename TOutputImage, typename TInternalPrecision >
void
UnsharpMaskImageFilter< TInputImage, TOutputImage, TInternalPrecision >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // This filter needs all of the input
  InputImagePointer image = const_cast<InputImageType *>(this->GetInput());
  if ( image )
    {
    image->SetRequestedRegion(this->GetInput()->GetLargestPossibleRegion());
    }
}


template< typename TInputImage, typename TOutputImage, typename TInternalPrecision >
void
UnsharpMaskImageFilter< TInputImage, TOutputImage, TInternalPrecision >
::VerifyPreconditions()
{
  Superclass::VerifyPreconditions();
  if (m_Threshold < 0.0)
    {
    itkExceptionMacro(<< "Threshold must be non-negative!");
    }
}


template< typename TInputImage, typename TOutputImage, typename TInternalPrecision >
void
UnsharpMaskImageFilter< TInputImage, TOutputImage, TInternalPrecision >
::GenerateData()
{
  typename TInputImage::Pointer input = TInputImage::New();
  input->Graft(const_cast<TInputImage *>(this->GetInput()));
  typename GaussianType::Pointer gaussianF = GaussianType::New();
  gaussianF->SetInput(input);
  gaussianF->SetSigmaArray(m_Sigmas);
  gaussianF->SetNumberOfThreads(this->GetNumberOfThreads());

  typedef UnsharpMaskingFunctor< InputPixelType, TInternalPrecision, OutputPixelType > USMType;
  typedef BinaryFunctorImageFilter< TInputImage, typename GaussianType::OutputImageType,
    TOutputImage, USMType > BinaryFunctorType;
  typename BinaryFunctorType::Pointer functorF = BinaryFunctorType::New();
  functorF->SetInput1(this->GetInput());
  functorF->SetInput2(gaussianF->GetOutput());
  USMType usmT(m_Amount, m_Threshold, m_Clamp);
  functorF->SetFunctor(usmT);
  functorF->SetNumberOfThreads(this->GetNumberOfThreads());

  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  progress->RegisterInternalFilter(gaussianF, 0.7);
  progress->RegisterInternalFilter(functorF, 0.3);

  functorF->GraftOutput(this->GetOutput());
  functorF->Update();
  this->GraftOutput(functorF->GetOutput());
}


template< typename TInputImage, typename TOutputImage, typename TInternalPrecision >
void
UnsharpMaskImageFilter< TInputImage, TOutputImage, TInternalPrecision >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Sigmas: " << m_Sigmas << std::endl;
  os << indent << "Amount: " << m_Amount << std::endl;
  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Clamp: " << m_Clamp << std::endl;
}

} // end namespace itk

#endif
