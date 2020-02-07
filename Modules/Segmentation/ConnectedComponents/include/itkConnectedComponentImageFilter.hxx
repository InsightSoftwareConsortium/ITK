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
#ifndef itkConnectedComponentImageFilter_hxx
#define itkConnectedComponentImageFilter_hxx

#include "itkConnectedComponentImageFilter.h"

#include "itkImageScanlineIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkMaskImageFilter.h"
#include "itkConnectedComponentAlgorithm.h"
#include "itkProgressTransformer.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TMaskImage>
ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>::ConnectedComponentImageFilter()
  : ScanlineFilterCommon<TInputImage, TOutputImage>(this)
{
  // implicit
  // #0 "Primary" required

  //  #1 "MaskImage" optional
  Self::AddOptionalInputName("MaskImage", 1);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateInputRequestedRegion()
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

  MaskImagePointer mask = const_cast<MaskImageType *>(this->GetMaskImage());
  if (mask)
  {
    mask->SetRequestedRegion(input->GetLargestPossibleRegion());
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateData()
{
  this->AllocateOutputs();
  this->SetupLineOffsets(false);
  typename TInputImage::ConstPointer input = this->GetInput();
  typename TMaskImage::ConstPointer  mask = this->GetMaskImage();

  using MaskFilterType = MaskImageFilter<TInputImage, TMaskImage, TInputImage>;
  typename MaskFilterType::Pointer maskFilter = MaskFilterType::New();
  if (mask)
  {
    maskFilter->SetInput(input);
    maskFilter->SetMaskImage(mask);
    maskFilter->Update();
    m_Input = maskFilter->GetOutput();
  }
  else
  {
    m_Input = input;
  }

  const typename OutputImageType::RegionType & requestedRegion = this->GetOutput()->GetRequestedRegion();
  const typename OutputImageType::SizeType &   requestedSize = requestedRegion.GetSize();

  // set up the vars used in the threads
  const SizeValueType pixelcount = requestedRegion.GetNumberOfPixels();
  const SizeValueType xsize = requestedSize[0];
  const SizeValueType linecount = pixelcount / xsize;
  this->m_LineMap.resize(linecount);
  this->m_NumberOfLabels.store(0);

  ProgressTransformer progress1(0.0f, 0.5f, this);

  MultiThreaderBase * multiThreader = this->GetMultiThreader();
  multiThreader->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  multiThreader->template ParallelizeImageRegionRestrictDirection<TOutputImage::ImageDimension>(
    0,
    requestedRegion,
    [this](const RegionType & lambdaRegion) { this->DynamicThreadedGenerateData(lambdaRegion); },
    progress1.GetProcessObject());

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
  SizeValueType numberOfObjects = this->CreateConsecutive(m_BackgroundValue);
  itkAssertOrThrowMacro(numberOfObjects <= this->m_NumberOfLabels,
                        "Number of consecutive labels cannot be greater than the initial number of labels!");
  // check for overflow exception here
  if (numberOfObjects > static_cast<SizeValueType>(NumericTraits<OutputPixelType>::max()))
  {
    itkExceptionMacro(<< "Number of objects (" << numberOfObjects << ") greater than maximum of output pixel type ("
                      << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(
                           NumericTraits<OutputPixelType>::max())
                      << ").");
  }
  m_ObjectCount = numberOfObjects;

  ProgressTransformer progress4(0.75f, 1.0f, this);
  multiThreader->template ParallelizeImageRegionRestrictDirection<TOutputImage::ImageDimension>(
    0,
    requestedRegion,
    [this](const RegionType & lambdaRegion) { this->ThreadedWriteOutput(lambdaRegion); },
    progress4.GetProcessObject());

  // clear and make sure memory is freed
  std::deque<WorkUnitData>().swap(this->m_WorkUnitResults);
  OffsetVectorType().swap(this->m_LineOffsets);
  LineMapType().swap(this->m_LineMap);
  ConsecutiveVectorType().swap(this->m_Consecutive);
  UnionFindType().swap(this->m_UnionFind);
  m_Input = nullptr;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>::DynamicThreadedGenerateData(
  const RegionType & outputRegionForThread)
{
  using InputLineIteratorType = ImageScanlineConstIterator<InputImageType>;
  InputLineIteratorType inLineIt(m_Input, outputRegionForThread);

  WorkUnitData  workUnitData = this->CreateWorkUnitData(outputRegionForThread);
  SizeValueType lineId = workUnitData.firstLine;

  SizeValueType nbOfLabels = 0;
  for (inLineIt.GoToBegin(); !inLineIt.IsAtEnd(); inLineIt.NextLine())
  {
    LineEncodingType thisLine;
    while (!inLineIt.IsAtEndOfLine())
    {
      const InputPixelType PVal = inLineIt.Get();
      // std::cout << inLineIt.GetIndex() << std::endl;
      if (PVal != NumericTraits<InputPixelType>::ZeroValue(PVal))
      {
        // We've hit the start of a run
        const IndexType thisIndex = inLineIt.GetIndex();
        // std::cout << thisIndex << std::endl;
        SizeValueType length = 1;
        ++inLineIt;
        while (!inLineIt.IsAtEndOfLine() && inLineIt.Get() != NumericTraits<InputPixelType>::ZeroValue(PVal))
        {
          ++length;
          ++inLineIt;
        }
        // create the run length object to go in the vector
        RunLength thisRun = { length, thisIndex, 0 };
        thisLine.push_back(thisRun);
        nbOfLabels++;
      }
      else
      {
        ++inLineIt;
      }
    }
    this->m_LineMap[lineId] = thisLine;
    lineId++;
  }

  this->m_NumberOfLabels.fetch_add(nbOfLabels, std::memory_order_relaxed);
  std::lock_guard<std::mutex> mutexHolder(this->m_Mutex);
  this->m_WorkUnitResults.push_back(workUnitData);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>::ThreadedWriteOutput(
  const RegionType & outputRegionForThread)
{
  // A more complex version that is intended to minimize the number of
  // visits to the output image which should improve cache
  // performance on large images. We also want to optimize the
  // performance of the map by being able to iterate through it,
  // rather than do lots of look ups. Don't know whether that will
  // make much difference in practice.
  // Note - this is unnecessary if AllocateOutputs initalizes to zero

  OutputImageType *                    output = this->GetOutput();
  ImageRegionIterator<OutputImageType> oit(output, outputRegionForThread);
  ImageRegionIterator<OutputImageType> fstart = oit;
  ImageRegionIterator<OutputImageType> fend = oit;
  fend.GoToEnd();

  WorkUnitData workUnitData = this->CreateWorkUnitData(outputRegionForThread);

  for (SizeValueType thisIdx = workUnitData.firstLine; thisIdx <= workUnitData.lastLine; thisIdx++)
  {
    for (LineEncodingConstIterator cIt = this->m_LineMap[thisIdx].begin(); cIt != this->m_LineMap[thisIdx].end(); ++cIt)
    {
      const SizeValueType   Ilab = this->LookupSet(cIt->label);
      const OutputPixelType lab = this->m_Consecutive[Ilab];
      oit.SetIndex(cIt->where);
      // initialize the non labelled pixels
      for (; fstart != oit; ++fstart)
      {
        fstart.Set(m_BackgroundValue);
      }
      // now fill the labelled sections
      for (SizeValueType i = 0; i < (SizeValueType)cIt->length; ++i, ++oit)
      {
        oit.Set(lab);
      }
      fstart = oit;
    }
  }

  // fill the rest of the output region with background value
  for (; fstart != fend; ++fstart)
  {
    fstart.Set(m_BackgroundValue);
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
ConnectedComponentImageFilter<TInputImage, TOutputImage, TMaskImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ObjectCount: " << m_ObjectCount << std::endl;
}
} // end namespace itk

#endif
