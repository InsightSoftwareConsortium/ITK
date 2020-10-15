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
#ifndef itkBinaryContourImageFilter_hxx
#define itkBinaryContourImageFilter_hxx

#include "itkBinaryContourImageFilter.h"

#include "itkImageScanlineIterator.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkMaskImageFilter.h"
#include "itkConnectedComponentAlgorithm.h"
#include "itkProgressTransformer.h"
#include "itkMath.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
BinaryContourImageFilter<TInputImage, TOutputImage>::BinaryContourImageFilter()
  : ScanlineFilterCommon<TInputImage, TOutputImage>(this)
  , m_ForegroundValue(NumericTraits<OutputImagePixelType>::max())
  , m_BackgroundValue(NumericTraits<OutputImagePixelType>::NonpositiveMin())
{
  this->SetInPlace(false);
  this->DynamicMultiThreadingOn();
}


template <typename TInputImage, typename TOutputImage>
void
BinaryContourImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if (!input)
  {
    return;
  }
  input->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage, typename TOutputImage>
void
BinaryContourImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  OutputImagePointer output = this->GetOutput();
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage, typename TOutputImage>
void
BinaryContourImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  this->UpdateProgress(0.0f);
  this->AllocateOutputs();
  this->BeforeThreadedGenerateData();
  this->SetupLineOffsets(true);

  ProgressTransformer progress1(0.05f, 0.5f, this);

  RegionType reqRegion = this->GetOutput()->GetRequestedRegion();

  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  // parallelize in a way which does not split the region along X axis
  constexpr unsigned int restrictedDirection = 0;
  this->GetMultiThreader()->template ParallelizeImageRegionRestrictDirection<ImageDimension>(
    restrictedDirection,
    reqRegion,
    [this](const RegionType & outputRegionForThread) { this->DynamicThreadedGenerateData(outputRegionForThread); },
    progress1.GetProcessObject());

  ProgressTransformer progress2(0.5f, 0.99f, this);

  // avoid splitting the region along X
  this->GetMultiThreader()->template ParallelizeImageRegionRestrictDirection<ImageDimension>(
    restrictedDirection,
    reqRegion,
    [this](const RegionType & outputRegionForThread) { this->ThreadedIntegrateData(outputRegionForThread); },
    progress2.GetProcessObject());

  this->AfterThreadedGenerateData();
  this->UpdateProgress(1.0f);
}

template <typename TInputImage, typename TOutputImage>
void
BinaryContourImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  OutputImagePointer     output = this->GetOutput();
  InputImageConstPointer input = this->GetInput();

  const RegionType &  reqRegion = output->GetRequestedRegion();
  const SizeValueType pixelcount = reqRegion.GetNumberOfPixels();
  const SizeValueType xsize = reqRegion.GetSize()[0];
  const SizeValueType linecount = (xsize > 0 ? pixelcount / xsize : 0);

  m_ForegroundLineMap.clear();
  m_ForegroundLineMap.resize(linecount);

  m_BackgroundLineMap.clear();
  m_BackgroundLineMap.resize(linecount);
}

template <typename TInputImage, typename TOutputImage>
void
BinaryContourImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const RegionType & outputRegionForThread)
{
  OutputImageType *      output = this->GetOutput();
  const InputImageType * input = this->GetInput();

  using InputLineIteratorType = ImageScanlineConstIterator<InputImageType>;
  InputLineIteratorType inLineIt(input, outputRegionForThread);

  using OutputLineIteratorType = ImageScanlineIterator<OutputImageType>;
  OutputLineIteratorType outLineIt(output, outputRegionForThread);

  outLineIt.GoToBegin();
  for (inLineIt.GoToBegin(); !inLineIt.IsAtEnd(); inLineIt.NextLine(), outLineIt.NextLine())
  {
    SizeValueType    lineId = this->IndexToLinearIndex(inLineIt.GetIndex());
    LineEncodingType fgLine;
    LineEncodingType bgLine;

    while (!inLineIt.IsAtEndOfLine())
    {
      InputImagePixelType PVal = inLineIt.Get();

      if (Math::AlmostEquals(PVal, m_ForegroundValue))
      {
        // We've hit the start of a run
        SizeValueType length = 0;
        IndexType     thisIndex = inLineIt.GetIndex();

        outLineIt.Set(m_BackgroundValue);

        ++length;
        ++inLineIt;
        ++outLineIt;

        while (!inLineIt.IsAtEndOfLine() && Math::AlmostEquals(inLineIt.Get(), m_ForegroundValue))
        {
          outLineIt.Set(m_BackgroundValue);
          ++length;
          ++inLineIt;
          ++outLineIt;
        }
        // create the run length object to go in the vector
        fgLine.push_back(RunLength(length, thisIndex));
      }
      else
      {
        // We've hit the start of a run
        SizeValueType length = 0;
        IndexType     thisIndex = inLineIt.GetIndex();

        outLineIt.Set(PVal);
        ++length;
        ++inLineIt;
        ++outLineIt;
        while (!inLineIt.IsAtEndOfLine() && Math::NotAlmostEquals(inLineIt.Get(), m_ForegroundValue))
        {
          outLineIt.Set(inLineIt.Get());
          ++length;
          ++inLineIt;
          ++outLineIt;
        }
        // create the run length object to go in the vector
        bgLine.push_back(RunLength(length, thisIndex));
      }
    }

    m_ForegroundLineMap[lineId] = fgLine;
    m_BackgroundLineMap[lineId] = bgLine;
    lineId++;
  }
}

template <typename TInputImage, typename TOutputImage>
void
BinaryContourImageFilter<TInputImage, TOutputImage>::ThreadedIntegrateData(const RegionType & outputRegionForThread)
{
  OutputImagePointer output = this->GetOutput();

  using OutputLineIteratorType = ImageScanlineIterator<OutputImageType>;
  OutputLineIteratorType outLineIt(output, outputRegionForThread);

  OffsetValueType linecount = m_ForegroundLineMap.size();

  for (outLineIt.GoToBegin(); !outLineIt.IsAtEnd(); outLineIt.NextLine())
  {
    SizeValueType thisIdx = this->IndexToLinearIndex(outLineIt.GetIndex());
    if (!m_ForegroundLineMap[thisIdx].empty())
    {
      for (OffsetVectorConstIterator I = this->m_LineOffsets.begin(); I != this->m_LineOffsets.end(); ++I)
      {
        OffsetValueType neighIdx = thisIdx + (*I);

        // check if the neighbor is in the map
        if (neighIdx >= 0 && neighIdx < OffsetValueType(linecount) && !m_BackgroundLineMap[neighIdx].empty())
        {
          // Now check whether they are really neighbors
          bool areNeighbors =
            this->CheckNeighbors(m_ForegroundLineMap[thisIdx][0].where, m_BackgroundLineMap[neighIdx][0].where);
          if (areNeighbors)
          {
            this->CompareLines(m_ForegroundLineMap[thisIdx],
                               m_BackgroundLineMap[neighIdx],
                               true,
                               false,
                               m_BackgroundValue,
                               [this, output](const LineEncodingConstIterator & foregroundRun,
                                              const LineEncodingConstIterator &,
                                              OffsetValueType oStart,
                                              OffsetValueType oLast) {
                                 itkAssertInDebugAndIgnoreInReleaseMacro(oStart <= oLast);
                                 OutputIndexType idx = foregroundRun->where;
                                 for (OffsetValueType x = oStart; x <= oLast; ++x)
                                 {
                                   idx[0] = x;
                                   output->SetPixel(idx, this->m_ForegroundValue);
                                 }
                               });
          }
        }
      }
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
BinaryContourImageFilter<TInputImage, TOutputImage>::AfterThreadedGenerateData()
{
  m_ForegroundLineMap.clear();
  m_BackgroundLineMap.clear();
}

template <typename TInputImage, typename TOutputImage>
void
BinaryContourImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: " << this->m_FullyConnected << std::endl;
  os << indent
     << "BackgroundValue: " << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue)
     << std::endl;
  os << indent
     << "ForegroundValue: " << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_ForegroundValue)
     << std::endl;
}
} // end namespace itk

#endif
