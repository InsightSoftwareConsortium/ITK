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
#ifndef itkBinaryGeneratorImageFilter_hxx
#define itkBinaryGeneratorImageFilter_hxx

#include "itkBinaryGeneratorImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkTotalProgressReporter.h"


namespace itk
{

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::BinaryGeneratorImageFilter()
{
  this->SetNumberOfRequiredInputs(2);
  this->InPlaceOff();
  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOff();
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::SetInput1(const TInputImage1 * image1)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(0, const_cast<TInputImage1 *>(image1));
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::SetInput1(
  const DecoratedInput1ImagePixelType * input1)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(0, const_cast<DecoratedInput1ImagePixelType *>(input1));
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::SetInput1(const Input1ImagePixelType & input1)
{
  typename DecoratedInput1ImagePixelType::Pointer newInput = DecoratedInput1ImagePixelType::New();
  newInput->Set(input1);
  this->SetInput1(newInput);
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::SetConstant1(const Input1ImagePixelType & input1)
{
  this->SetInput1(input1);
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
const typename BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::Input1ImagePixelType &
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::GetConstant1() const
{
  const auto * input = dynamic_cast<const DecoratedInput1ImagePixelType *>(this->ProcessObject::GetInput(0));
  if (input == nullptr)
  {
    itkExceptionMacro(<< "Constant 1 is not set");
  }
  return input->Get();
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::SetInput2(const TInputImage2 * image2)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(1, const_cast<TInputImage2 *>(image2));
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::SetInput2(
  const DecoratedInput2ImagePixelType * input2)
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(1, const_cast<DecoratedInput2ImagePixelType *>(input2));
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::SetInput2(const Input2ImagePixelType & input2)
{
  typename DecoratedInput2ImagePixelType::Pointer newInput = DecoratedInput2ImagePixelType::New();
  newInput->Set(input2);
  this->SetInput2(newInput);
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::SetConstant2(const Input2ImagePixelType & input2)
{
  this->SetInput2(input2);
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
const typename BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::Input2ImagePixelType &
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::GetConstant2() const
{
  const auto * input = dynamic_cast<const DecoratedInput2ImagePixelType *>(this->ProcessObject::GetInput(1));
  if (input == nullptr)
  {
    itkExceptionMacro(<< "Constant 2 is not set");
  }
  return input->Get();
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::GenerateOutputInformation()
{

  const DataObject * input = nullptr;
  const auto *       inputPtr1 = dynamic_cast<const TInputImage1 *>(ProcessObject::GetInput(0));
  const auto *       inputPtr2 = dynamic_cast<const TInputImage2 *>(ProcessObject::GetInput(1));

  if (this->GetNumberOfInputs() >= 2)
  {
    if (inputPtr1)
    {
      input = inputPtr1;
    }
    else if (inputPtr2)
    {
      input = inputPtr2;
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

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)

{
  m_DynamicThreadedGenerateDataFunction(outputRegionForThread);
}

template <typename TInputImage1, typename TInputImage2, typename TOutputImage>
template <typename TFunctor>
void
BinaryGeneratorImageFilter<TInputImage1, TInputImage2, TOutputImage>::DynamicThreadedGenerateDataWithFunctor(
  const TFunctor &              functor,
  const OutputImageRegionType & outputRegionForThread)
{
  // We use dynamic_cast since inputs are stored as DataObjects. The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second input.
  const auto *   inputPtr1 = dynamic_cast<const TInputImage1 *>(ProcessObject::GetInput(0));
  const auto *   inputPtr2 = dynamic_cast<const TInputImage2 *>(ProcessObject::GetInput(1));
  TOutputImage * outputPtr = this->GetOutput(0);

  TotalProgressReporter progress(this, outputPtr->GetRequestedRegion().GetNumberOfPixels());

  if (inputPtr1 && inputPtr2)
  {
    ImageScanlineConstIterator<TInputImage1> inputIt1(inputPtr1, outputRegionForThread);
    ImageScanlineConstIterator<TInputImage2> inputIt2(inputPtr2, outputRegionForThread);
    ImageScanlineIterator<TOutputImage>      outputIt(outputPtr, outputRegionForThread);

    while (!inputIt1.IsAtEnd())
    {
      while (!inputIt1.IsAtEndOfLine())
      {
        outputIt.Set(functor(inputIt1.Get(), inputIt2.Get()));
        ++inputIt2;
        ++inputIt1;
        ++outputIt;
      }

      inputIt1.NextLine();
      inputIt2.NextLine();
      outputIt.NextLine();
      progress.Completed(outputRegionForThread.GetSize()[0]);
    }
  }
  else if (inputPtr1)
  {
    ImageScanlineConstIterator<TInputImage1> inputIt1(inputPtr1, outputRegionForThread);
    ImageScanlineIterator<TOutputImage>      outputIt(outputPtr, outputRegionForThread);

    const Input2ImagePixelType & input2Value = this->GetConstant2();

    while (!inputIt1.IsAtEnd())
    {
      while (!inputIt1.IsAtEndOfLine())
      {
        outputIt.Set(functor(inputIt1.Get(), input2Value));
        ++inputIt1;
        ++outputIt;
      }
      inputIt1.NextLine();
      outputIt.NextLine();
      progress.Completed(outputRegionForThread.GetSize()[0]);
    }
  }
  else if (inputPtr2)
  {
    ImageScanlineConstIterator<TInputImage2> inputIt2(inputPtr2, outputRegionForThread);
    ImageScanlineIterator<TOutputImage>      outputIt(outputPtr, outputRegionForThread);

    const Input1ImagePixelType & input1Value = this->GetConstant1();

    while (!inputIt2.IsAtEnd())
    {
      while (!inputIt2.IsAtEndOfLine())
      {
        outputIt.Set(functor(input1Value, inputIt2.Get()));
        ++inputIt2;
        ++outputIt;
      }
      inputIt2.NextLine();
      outputIt.NextLine();
      progress.Completed(outputRegionForThread.GetSize()[0]);
    }
  }
  else
  {
    itkGenericExceptionMacro(<< "At most one of the inputs can be a constant.");
  }
}
} // end namespace itk

#endif
