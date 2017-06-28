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

#ifndef itkFFTPadPositiveIndexImageFilter_hxx
#define itkFFTPadPositiveIndexImageFilter_hxx

#include "itkFFTPadPositiveIndexImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkOffset.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template <class TInputImage, class TOutputImage>
FFTPadPositiveIndexImageFilter<TInputImage, TOutputImage>::FFTPadPositiveIndexImageFilter()
  : m_SizeGreatestPrimeFactor(2)
  , m_BoundaryCondition(ITK_NULLPTR)
{
  m_FFTPadFilter = FFTPadFilterType::New();
  m_FFTPadFilter->ReleaseDataFlagOn();
  m_BoundaryCondition = m_FFTPadFilter->GetBoundaryCondition();
  m_ChangeInfoFilter = ChangeInfoFilterType::New();
  m_ChangeInfoFilter->ChangeRegionOn();
}

template <class TInputImage, class TOutputImage>
void
FFTPadPositiveIndexImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // Get pointers to the input and output.
  InputImageType *        inputPtr = const_cast<InputImageType *>(this->GetInput());
  const OutputImageType * outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr != ITK_NULLPTR);
  itkAssertInDebugAndIgnoreInReleaseMacro(outputPtr);

  const typename InputImageType::RegionType &  inputLargestPossibleRegion = inputPtr->GetLargestPossibleRegion();
  const typename OutputImageType::RegionType & outputRequestedRegion = outputPtr->GetRequestedRegion();

  typename OutputImageType::RegionType            shiftedOutputRequestedRegion;
  typename OutputImageType::RegionType::IndexType shiftedRequestedIndex =
    outputRequestedRegion.GetIndex() - this->m_ChangeInfoFilter->GetOutputOffset();
  shiftedOutputRequestedRegion.SetIndex(shiftedRequestedIndex);
  shiftedOutputRequestedRegion.SetSize(outputRequestedRegion.GetSize());

  // Ask the boundary condition for the input requested region.
  if (!m_BoundaryCondition)
  {
    itkExceptionMacro(<< "Boundary condition is ITK_NULLPTR so no request region can be generated.");
  }
  typename InputImageType::RegionType inputRequestedRegion =
    m_BoundaryCondition->GetInputRequestedRegion(inputLargestPossibleRegion, shiftedOutputRequestedRegion);

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

template <class TInputImage, class TOutputImage>
void
FFTPadPositiveIndexImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  const InputImageType * inputPtr = this->GetInput();
  OutputImageType *      outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr);
  itkAssertInDebugAndIgnoreInReleaseMacro(outputPtr != ITK_NULLPTR);

  RegionType inputRegion = inputPtr->GetLargestPossibleRegion();
  SizeType   size;
  IndexType  index;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    SizeValueType padSize = 0;
    if (m_SizeGreatestPrimeFactor > 1)
    {
      while (Math::GreatestPrimeFactor(inputRegion.GetSize()[i] + padSize) > m_SizeGreatestPrimeFactor)
      {
        ++padSize;
      }
    }
    else if (m_SizeGreatestPrimeFactor == 1)
    {
      // make sure the total size is even
      padSize += (inputRegion.GetSize()[i] + padSize) % 2;
    }
    index[i] = inputRegion.GetIndex()[i];
    size[i] = inputRegion.GetSize()[i] + padSize;
  }
  RegionType region(index, size);
  outputPtr->SetLargestPossibleRegion(region);
}

template <class TInputImage, class TOutputImage>
void
FFTPadPositiveIndexImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(FFTPadFilter);
  itkPrintSelfObjectMacro(ChangeInfoFilter);

  os << indent << "SizeGreatestPrimeFactor: " << m_SizeGreatestPrimeFactor << std::endl;

  m_BoundaryCondition->Print(os, indent);
}

template <class TInputImage, class TOutputImage>
void
FFTPadPositiveIndexImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Get pointers to the input and output
  OutputImageType * output = this->GetOutput();
  m_FFTPadFilter->SetInput(this->GetInput());
  m_FFTPadFilter->SetBoundaryCondition(this->m_BoundaryCondition);
  m_FFTPadFilter->SetSizeGreatestPrimeFactor(this->GetSizeGreatestPrimeFactor());
  m_FFTPadFilter->Update();
  progress->RegisterInternalFilter(m_FFTPadFilter, 0.5);

  typename OutputImageType::IndexType outputIndex = output->GetLargestPossibleRegion().GetIndex();
  typename OutputImageType::IndexType negativeIndex =
    m_FFTPadFilter->GetOutput()->GetLargestPossibleRegion().GetIndex();

  m_ChangeInfoFilter->SetOutputOffset(outputIndex - negativeIndex);
  itkDebugMacro(<< "Offset difference: " << outputIndex - negativeIndex);
  m_ChangeInfoFilter->SetInput(m_FFTPadFilter->GetOutput());
  m_ChangeInfoFilter->GraftOutput(output);
  m_ChangeInfoFilter->Update();
  progress->RegisterInternalFilter(m_ChangeInfoFilter, 0.5);
  this->GraftOutput(m_ChangeInfoFilter->GetOutput());
}
} // end namespace itk
#endif
