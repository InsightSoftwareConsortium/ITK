/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkTernaryGeneratorImageFilter_hxx
#define itkTernaryGeneratorImageFilter_hxx

#include "itkTernaryGeneratorImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkTotalProgressReporter.h"

namespace itk
{
template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::TernaryGeneratorImageFilter()
{
  this->SetNumberOfRequiredInputs(3);
  this->InPlaceOff();
  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOff();
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput1(
  const TInputImage1 * image1)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput(0, const_cast<TInputImage1 *>(image1));
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput1(
  const DecoratedInput1ImagePixelType * input1)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(0, const_cast<DecoratedInput1ImagePixelType *>(input1));
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput1(
  const Input1ImagePixelType & input1)
{
  typename DecoratedInput1ImagePixelType::Pointer newInput = DecoratedInput1ImagePixelType::New();
  newInput->Set(input1);
  this->SetInput1(newInput);
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetConstant1(
  const Input1ImagePixelType & input1)
{
  this->SetInput1(input1);
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
const typename TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::
  Input1ImagePixelType &
  TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::GetConstant1() const
{
  const auto * input = dynamic_cast<const DecoratedInput1ImagePixelType *>(this->ProcessObject::GetInput(0));
  if (input == nullptr)
  {
    itkExceptionMacro(<< "Constant 1 is not set");
  }
  return input->Get();
}


template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput2(
  const TInputImage2 * image2)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput(1, const_cast<TInputImage2 *>(image2));
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput2(
  const DecoratedInput2ImagePixelType * input2)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(1, const_cast<DecoratedInput2ImagePixelType *>(input2));
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput2(
  const Input2ImagePixelType & input2)
{
  typename DecoratedInput2ImagePixelType::Pointer newInput = DecoratedInput2ImagePixelType::New();
  newInput->Set(input2);
  this->SetInput2(newInput);
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetConstant2(
  const Input2ImagePixelType & input2)
{
  this->SetInput2(input2);
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
const typename TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::
  Input2ImagePixelType &
  TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::GetConstant2() const
{
  const auto * input = dynamic_cast<const DecoratedInput2ImagePixelType *>(this->ProcessObject::GetInput(1));
  if (input == nullptr)
  {
    itkExceptionMacro(<< "Constant 2 is not set");
  }
  return input->Get();
}


template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput3(
  const TInputImage3 * image3)
{
  // The ProcessObject is not const-correct so the const_cast is required here
  this->SetNthInput(2, const_cast<TInputImage3 *>(image3));
}


template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput3(
  const DecoratedInput3ImagePixelType * input3)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(2, const_cast<DecoratedInput3ImagePixelType *>(input3));
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetInput3(
  const Input3ImagePixelType & input3)
{
  typename DecoratedInput3ImagePixelType::Pointer newInput = DecoratedInput3ImagePixelType::New();
  newInput->Set(input3);
  this->SetInput3(newInput);
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::SetConstant3(
  const Input3ImagePixelType & input3)
{
  this->SetInput3(input3);
}

template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
const typename TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::
  Input3ImagePixelType &
  TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::GetConstant3() const
{
  const auto * input = dynamic_cast<const DecoratedInput3ImagePixelType *>(this->ProcessObject::GetInput(2));
  if (input == nullptr)
  {
    itkExceptionMacro(<< "Constant 3 is not set");
  }
  return input->Get();
}

/**
 * BeforeThreadedGenerateData function. Validate inputs
 */
template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::GenerateOutputInformation()
{

  const DataObject * input = nullptr;
  const auto *       inputPtr1 = dynamic_cast<const TInputImage1 *>(ProcessObject::GetInput(0));
  const auto *       inputPtr2 = dynamic_cast<const TInputImage2 *>(ProcessObject::GetInput(1));
  const auto *       inputPtr3 = dynamic_cast<const TInputImage2 *>(ProcessObject::GetInput(2));

  if (this->GetNumberOfInputs() >= 3)
  {
    if (inputPtr1)
    {
      input = inputPtr1;
    }
    else if (inputPtr2)
    {
      input = inputPtr2;
    }
    else if (inputPtr3)
    {
      input = inputPtr3;
    }
    else
    {
      return;
    }

    for (unsigned int idx = 0; idx < this->GetNumberOfOutputs(); ++idx)
    {
      DataObject * output = this->GetOutput(idx);
      if (output)
      {
        output->CopyInformation(input);
      }
    }
  }
}

/**
 * DynamicThreadedGenerateData function. Performs the pixel-wise addition
 */
template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  m_DynamicThreadedGenerateDataFunction(outputRegionForThread);
}


template <typename TInputImage1, typename TInputImage2, typename TInputImage3, typename TOutputImage>
template <typename TFunctor>
void
TernaryGeneratorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage>::
  DynamicThreadedGenerateDataWithFunctor(const TFunctor & functor, const OutputImageRegionType & outputRegionForThread)
{

  // We use dynamic_cast since inputs are stored as DataObjects. The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second input.
  const auto *       inputPtr1 = dynamic_cast<const TInputImage1 *>(ProcessObject::GetInput(0));
  const auto *       inputPtr2 = dynamic_cast<const TInputImage2 *>(ProcessObject::GetInput(1));
  const auto *       inputPtr3 = dynamic_cast<const TInputImage3 *>(ProcessObject::GetInput(2));
  OutputImagePointer outputPtr = this->GetOutput(0);

  TotalProgressReporter progress(this, outputPtr->GetRequestedRegion().GetNumberOfPixels());

  std::unique_ptr<ImageScanlineConstIterator<TInputImage1>> inputIt1;
  std::unique_ptr<ImageScanlineConstIterator<TInputImage2>> inputIt2;
  std::unique_ptr<ImageScanlineConstIterator<TInputImage3>> inputIt3;
  ImageScanlineIterator<TOutputImage>                       outputIt(outputPtr, outputRegionForThread);

  if (inputPtr1 && inputPtr2 && inputPtr3)
  {
    inputIt1.reset(new ImageScanlineConstIterator<TInputImage1>(inputPtr1, outputRegionForThread));
    inputIt2.reset(new ImageScanlineConstIterator<TInputImage2>(inputPtr2, outputRegionForThread));
    inputIt3.reset(new ImageScanlineConstIterator<TInputImage3>(inputPtr3, outputRegionForThread));

    while (!outputIt.IsAtEnd())
    {
      while (!outputIt.IsAtEndOfLine())
      {
        outputIt.Set(functor(inputIt1->Get(), inputIt2->Get(), inputIt3->Get()));
        ++*inputIt1;
        ++*inputIt2;
        ++*inputIt3;
        ++outputIt;
      }
      inputIt1->NextLine();
      inputIt2->NextLine();
      inputIt3->NextLine();
      outputIt.NextLine();
      progress.Completed(outputRegionForThread.GetSize()[0]);
    }
  }
  else
  {
    if (inputPtr1)
    {
      inputIt1.reset(new ImageScanlineConstIterator<TInputImage1>(inputPtr1, outputRegionForThread));
    }
    if (inputPtr2)
    {
      inputIt2.reset(new ImageScanlineConstIterator<TInputImage2>(inputPtr2, outputRegionForThread));
    }
    if (inputPtr3)
    {
      inputIt3.reset(new ImageScanlineConstIterator<TInputImage3>(inputPtr3, outputRegionForThread));
    }


    const Input1ImagePixelType & input1Value = inputPtr1 ? Input1ImagePixelType() : this->GetConstant1();
    const Input2ImagePixelType & input2Value = inputPtr2 ? Input2ImagePixelType() : this->GetConstant2();
    const Input3ImagePixelType & input3Value = inputPtr3 ? Input3ImagePixelType() : this->GetConstant3();

    while (!outputIt.IsAtEnd())
    {
      while (!outputIt.IsAtEndOfLine())
      {
        outputIt.Set(functor(inputPtr1 ? inputIt1->Get() : input1Value,
                             inputPtr2 ? inputIt2->Get() : input2Value,
                             inputPtr3 ? inputIt3->Get() : input3Value));
        if (inputIt1)
        {
          ++*inputIt1;
        }
        if (inputIt2)
        {
          ++*inputIt2;
        }
        if (inputIt3)
        {
          ++*inputIt3;
        }
        ++outputIt;
      }
      if (inputIt1)
      {
        inputIt1->NextLine();
      }
      if (inputIt2)
      {
        inputIt2->NextLine();
      }
      if (inputIt3)
      {
        inputIt3->NextLine();
      }
      outputIt.NextLine();
      progress.Completed(outputRegionForThread.GetSize()[0]);
    }
  }
}
} // end namespace itk

#endif
