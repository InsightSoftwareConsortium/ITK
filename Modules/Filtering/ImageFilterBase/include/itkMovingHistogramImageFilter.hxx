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
#ifndef itkMovingHistogramImageFilter_hxx
#define itkMovingHistogramImageFilter_hxx

#include "itkMovingHistogramImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOffset.h"
#include "itkTotalProgressReporter.h"
#include "itkNumericTraits.h"
#include "itkImageRegionIterator.h"
#include "itkImageLinearConstIteratorWithIndex.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TKernel, typename THistogram>
MovingHistogramImageFilter<TInputImage, TOutputImage, TKernel, THistogram>::MovingHistogramImageFilter()
{
  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOn();
}

// a modified version that uses line iterators and only moves the
// histogram in one direction. Hopefully it will be a bit simpler and
// faster due to improved memory access and a tighter loop.
template <typename TInputImage, typename TOutputImage, typename TKernel, typename THistogram>
void
MovingHistogramImageFilter<TInputImage, TOutputImage, TKernel, THistogram>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  HistogramType histogram;
  this->ConfigureHistogram(histogram);

  OutputImageType *      outputImage = this->GetOutput();
  const InputImageType * inputImage = this->GetInput();
  RegionType             inputRegion = inputImage->GetRequestedRegion();

  TotalProgressReporter progress(this, outputImage->GetRequestedRegion().GetNumberOfPixels());

  // initialize the histogram
  for (auto listIt = this->m_KernelOffsets.begin(); listIt != this->m_KernelOffsets.end(); listIt++)
  {
    IndexType idx = outputRegionForThread.GetIndex() + (*listIt);
    if (inputRegion.IsInside(idx))
    {
      histogram.AddPixel(inputImage->GetPixel(idx));
    }
    else
    {
      histogram.AddBoundary();
    }
  }

  // now move the histogram
  FixedArray<short, ImageDimension> direction;
  direction.Fill(1);
  int        axis = ImageDimension - 1;
  OffsetType offset;
  offset.Fill(0);
  RegionType stRegion;
  stRegion.SetSize(this->m_Kernel.GetSize());
  stRegion.PadByRadius(1); // must pad the region by one because of the translation

  OffsetType   centerOffset;
  unsigned int i;
  for (i = 0; i < ImageDimension; i++)
  {
    centerOffset[i] = stRegion.GetSize()[i] / 2;
  }

  int BestDirection = this->m_Axes[axis];
  int LineLength = inputRegion.GetSize()[BestDirection];

  // init the offset and get the lists for the best axis
  offset[BestDirection] = direction[BestDirection];
  // it's very important for performances to get a pointer and not a copy
  const OffsetListType * addedList = &this->m_AddedOffsets[offset];
  const OffsetListType * removedList = &this->m_RemovedOffsets[offset];

  using InputLineIteratorType = ImageLinearConstIteratorWithIndex<InputImageType>;
  InputLineIteratorType InLineIt(inputImage, outputRegionForThread);
  InLineIt.SetDirection(BestDirection);

  InLineIt.GoToBegin();
  IndexType LineStart;
  // PrevLineStart = InLineIt.GetIndex();
  InLineIt.GoToBegin();

  using HistVecType = typename std::vector<HistogramType>;
  HistVecType HistVec(ImageDimension);
  using IndexVecType = typename std::vector<IndexType>;
  IndexVecType PrevLineStartVec(ImageDimension);

  // Steps is used to keep track of the order in which the line
  // iterator passes over the various dimensions.
  auto * Steps = new int[ImageDimension];

  for (i = 0; i < ImageDimension; i++)
  {
    HistVec[i] = histogram;
    PrevLineStartVec[i] = InLineIt.GetIndex();
    Steps[i] = 0;
  }

  while (!InLineIt.IsAtEnd())
  {
    HistogramType & histRef = HistVec[BestDirection];
    IndexType       PrevLineStart = InLineIt.GetIndex();
    for (InLineIt.GoToBeginOfLine(); !InLineIt.IsAtEndOfLine(); ++InLineIt)
    {
      // Update the historgram
      IndexType currentIdx = InLineIt.GetIndex();
      outputImage->SetPixel(currentIdx,
                            static_cast<OutputPixelType>(histRef.GetValue(inputImage->GetPixel(currentIdx))));
      stRegion.SetIndex(currentIdx - centerOffset);
      PushHistogram(histRef, addedList, removedList, inputRegion, stRegion, inputImage, currentIdx);
    }
    Steps[BestDirection] += LineLength;
    InLineIt.NextLine();
    if (InLineIt.IsAtEnd())
    {
      break;
    }
    LineStart = InLineIt.GetIndex();
    // This section updates the histogram for the next line
    // Since we aren't zig zagging we need to figure out which
    // histogram to update and the direction in which to push
    // it. Then we need to copy that histogram to the relevant
    // places
    OffsetType LineOffset, Changes;
    // Figure out which stored histogram to move and in
    // which direction
    int LineDirection = 0;
    // This function deals with changing planes etc
    this->GetDirAndOffset(LineStart, PrevLineStart, LineOffset, Changes, LineDirection);
    ++(Steps[LineDirection]);
    IndexType              PrevLineStartHist = LineStart - LineOffset;
    const OffsetListType * addedListLine = &this->m_AddedOffsets[LineOffset];
    const OffsetListType * removedListLine = &this->m_RemovedOffsets[LineOffset];
    HistogramType &        tmpHist = HistVec[LineDirection];
    stRegion.SetIndex(PrevLineStart - centerOffset);
    // Now move the histogram
    PushHistogram(tmpHist, addedListLine, removedListLine, inputRegion, stRegion, inputImage, PrevLineStartHist);

    // PrevLineStartVec[LineDirection] = LineStart;
    // copy the updated histogram and line start entries to the
    // relevant directions. When updating direction 2, for example,
    // new copies of directions 0 and 1 should be made.
    for (i = 0; i < ImageDimension; i++)
    {
      if (Steps[i] > Steps[LineDirection])
      {
        // PrevLineStartVec[i] = LineStart;
        HistVec[i] = HistVec[LineDirection];
      }
    }
    progress.Completed(outputRegionForThread.GetSize()[0]);
  }
  delete[] Steps;
}

template <typename TInputImage, typename TOutputImage, typename TKernel, typename THistogram>
void
MovingHistogramImageFilter<TInputImage, TOutputImage, TKernel, THistogram>::PushHistogram(
  HistogramType &        histogram,
  const OffsetListType * addedList,
  const OffsetListType * removedList,
  const RegionType &     inputRegion,
  const RegionType &     kernRegion,
  const InputImageType * inputImage,
  const IndexType        currentIdx)
{
  if (inputRegion.IsInside(kernRegion))
  {
    // update the histogram
    for (auto addedIt = addedList->begin(); addedIt != addedList->end(); addedIt++)
    {
      histogram.AddPixel(inputImage->GetPixel(currentIdx + (*addedIt)));
    }
    for (auto removedIt = removedList->begin(); removedIt != removedList->end(); removedIt++)
    {
      histogram.RemovePixel(inputImage->GetPixel(currentIdx + (*removedIt)));
    }
  }
  else
  {
    // update the histogram
    for (auto addedIt = addedList->begin(); addedIt != addedList->end(); addedIt++)
    {
      IndexType idx = currentIdx + (*addedIt);
      if (inputRegion.IsInside(idx))
      {
        histogram.AddPixel(inputImage->GetPixel(idx));
      }
      else
      {
        histogram.AddBoundary();
      }
    }
    for (auto removedIt = removedList->begin(); removedIt != removedList->end(); removedIt++)
    {
      IndexType idx = currentIdx + (*removedIt);
      if (inputRegion.IsInside(idx))
      {
        histogram.RemovePixel(inputImage->GetPixel(idx));
      }
      else
      {
        histogram.RemoveBoundary();
      }
    }
  }
}

} // end namespace itk
#endif
