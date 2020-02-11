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
#ifndef itkBinaryImageToLabelMapFilter_hxx
#define itkBinaryImageToLabelMapFilter_hxx

#include "itkBinaryImageToLabelMapFilter.h"
#include "itkNumericTraits.h"
#include "itkImageScanlineIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkConnectedComponentAlgorithm.h"
#include "itkProgressReporter.h"
#include "itkProgressTransformer.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
BinaryImageToLabelMapFilter<TInputImage, TOutputImage>::BinaryImageToLabelMapFilter()
  : ScanlineFilterCommon<TInputImage, TOutputImage>(this)
  , m_OutputBackgroundValue(NumericTraits<OutputPixelType>::NonpositiveMin())
{
  this->m_NumberOfObjects = 0;
  this->m_InputForegroundValue = NumericTraits<InputPixelType>::max();
}

template <typename TInputImage, typename TOutputImage>
void
BinaryImageToLabelMapFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if (!input)
  {
    return;
  }
  input->SetRequestedRegion(input->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputImage>
void
BinaryImageToLabelMapFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  TOutputImage * output = this->GetOutput();
  output->SetRequestedRegion(output->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputImage>
void
BinaryImageToLabelMapFilter<TInputImage, TOutputImage>::GenerateData()
{
  // Call a method that can be overriden by a subclass to allocate
  // memory for the filter's outputs
  this->AllocateOutputs();
  this->SetupLineOffsets(false);
  OutputImageType * output = this->GetOutput();
  output->SetBackgroundValue(this->m_OutputBackgroundValue);

  const typename OutputImageType::RegionType & requestedRegion = output->GetRequestedRegion();
  const typename OutputImageType::SizeType &   requestedSize = requestedRegion.GetSize();

  const SizeValueType pixelcount = requestedRegion.GetNumberOfPixels();
  const SizeValueType xsize = requestedSize[0];
  const SizeValueType linecount = pixelcount / xsize;
  this->m_LineMap.resize(linecount);
  this->m_NumberOfLabels.store(0);
  this->SetupLineOffsets(false);

  ProgressTransformer progress1(0.0f, 0.5f, this);

  MultiThreaderBase * multiThreader = this->GetMultiThreader();
  multiThreader->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  multiThreader->template ParallelizeImageRegionRestrictDirection<TOutputImage::ImageDimension>(
    0,
    requestedRegion,
    [this](const RegionType & lambdaRegion) { this->DynamicThreadedGenerateData(lambdaRegion); },
    progress1.GetProcessObject());

  // compute the total number of labels
  SizeValueType nbOfLabels = this->m_NumberOfLabels.load();

  // insert all the labels into the structure -- an extra loop but
  // saves complicating the ones that come later
  this->InitUnion(nbOfLabels);

  ProgressTransformer progress2(0.55f, 0.6f, this);
  multiThreader->ParallelizeArray(
    0,
    this->m_WorkUnitResults.size(),
    [this](SizeValueType index) { this->ComputeEquivalence(index, true); },
    progress2.GetProcessObject());

  ProgressTransformer progress3(0.6f, 0.75f, this);
  multiThreader->ParallelizeArray(
    0,
    this->m_WorkUnitResults.size(),
    [this](SizeValueType index) { this->ComputeEquivalence(index, false); },
    progress3.GetProcessObject());

  // AfterThreadedGenerateData
  typename TInputImage::ConstPointer input = this->GetInput();
  m_NumberOfObjects = this->CreateConsecutive(m_OutputBackgroundValue);
  ProgressReporter progress(this, 0, linecount, 25, 0.75f, 0.25f);
  // check for overflow exception here
  if (m_NumberOfObjects > static_cast<SizeValueType>(NumericTraits<OutputPixelType>::max()))
  {
    itkExceptionMacro(<< "Number of objects (" << m_NumberOfObjects << ") greater than maximum of output pixel type ("
                      << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(
                           NumericTraits<OutputPixelType>::max())
                      << ").");
  }

  for (SizeValueType thisIdx = 0; thisIdx < linecount; thisIdx++)
  {
    // now fill the labelled sections
    LineEncodingConstIterator       cIt = this->m_LineMap[thisIdx].begin();
    const LineEncodingConstIterator cEnd = this->m_LineMap[thisIdx].end();

    while (cIt != cEnd)
    {
      const InternalLabelType Ilab = this->LookupSet(cIt->label);
      const OutputPixelType   lab = this->m_Consecutive[Ilab];
      output->SetLine(cIt->where, cIt->length, lab);
      ++cIt;
    }
    progress.CompletedPixel();
  }

  // clear and make sure memory is freed
  std::deque<WorkUnitData>().swap(this->m_WorkUnitResults);
  OffsetVectorType().swap(this->m_LineOffsets);
  LineMapType().swap(this->m_LineMap);
}

template <typename TInputImage, typename TOutputImage>
void
BinaryImageToLabelMapFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const RegionType & outputRegionForThread)
{
  const TInputImage * input = this->GetInput();
  using InputLineIteratorType = ImageScanlineConstIterator<InputImageType>;
  InputLineIteratorType inLineIt(input, outputRegionForThread);

  WorkUnitData  workUnitData = this->CreateWorkUnitData(outputRegionForThread);
  SizeValueType lineId = workUnitData.firstLine;

  SizeValueType nbOfLabels = 0;
  for (inLineIt.GoToBegin(); !inLineIt.IsAtEnd(); inLineIt.NextLine())
  {
    LineEncodingType thisLine;
    while (!inLineIt.IsAtEndOfLine())
    {
      const InputPixelType pixelValue = inLineIt.Get();
      if (pixelValue == this->m_InputForegroundValue)
      {
        // We've hit the start of a run
        SizeValueType length = 0;
        IndexType     thisIndex;
        thisIndex = inLineIt.GetIndex();
        ++length;
        ++inLineIt;
        while (!inLineIt.IsAtEndOfLine() && inLineIt.Get() == this->m_InputForegroundValue)
        {
          ++length;
          ++inLineIt;
        }
        // create the run length object to go in the vector
        RunLength thisRun(length, thisIndex, 0); // will give a real label later
        thisLine.push_back(thisRun);
        ++nbOfLabels;
      }
      else
      {
        ++inLineIt;
      }
    }
    // equivalent to assignment because thisLine goes of out scope afterwards
    this->m_LineMap[lineId].swap(thisLine);
    ++lineId;
  }

  this->m_NumberOfLabels.fetch_add(nbOfLabels, std::memory_order_relaxed);
  std::lock_guard<std::mutex> mutexHolder(this->m_Mutex);
  this->m_WorkUnitResults.push_back(workUnitData);
}


template <typename TInputImage, typename TOutputImage>
void
BinaryImageToLabelMapFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "InputForegroundValue: "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(this->m_InputForegroundValue) << std::endl;
  os << indent << "OutputBackgroundValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(this->m_OutputBackgroundValue)
     << std::endl;
  os << indent << "Number of Objects: " << this->m_NumberOfObjects << std::endl;
}
} // end namespace itk

#endif
