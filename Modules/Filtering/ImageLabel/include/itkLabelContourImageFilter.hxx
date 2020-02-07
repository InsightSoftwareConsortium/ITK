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
#ifndef itkLabelContourImageFilter_hxx
#define itkLabelContourImageFilter_hxx

#include "itkLabelContourImageFilter.h"

#include "itkImageScanlineIterator.h"
#include "itkConnectedComponentAlgorithm.h"
#include "itkProgressTransformer.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
LabelContourImageFilter<TInputImage, TOutputImage>::LabelContourImageFilter()
  : ScanlineFilterCommon<TInputImage, TOutputImage>(this)
  , m_BackgroundValue(NumericTraits<OutputImagePixelType>::NonpositiveMin())
{
  this->SetInPlace(false);
  this->DynamicMultiThreadingOn();
}

// -----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());

  if (!input)
  {
    return;
  }
  input->SetRequestedRegion(input->GetLargestPossibleRegion());
}

// -----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  this->UpdateProgress(0.0f);
  this->AllocateOutputs();
  this->SetupLineOffsets(true);
  this->BeforeThreadedGenerateData();

  ProgressTransformer progress1(0.01f, 0.5f, this);

  OutputRegionType reqRegion = this->GetOutput()->GetRequestedRegion();

  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  this->GetMultiThreader()->template ParallelizeImageRegionRestrictDirection<ImageDimension>(
    0, // do not split along X axis
    reqRegion,
    [this](const OutputRegionType & r) { this->DynamicThreadedGenerateData(r); },
    progress1.GetProcessObject());

  ProgressTransformer progress2(0.5f, 0.99f, this);
  this->GetMultiThreader()->template ParallelizeImageRegionRestrictDirection<ImageDimension>(
    0, // do not split along X axis
    reqRegion,
    [this](const OutputRegionType & r) { this->ThreadedIntegrateData(r); },
    progress2.GetProcessObject());

  this->AfterThreadedGenerateData();
  this->UpdateProgress(1.0f);
}

// -----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  OutputImageType * output = this->GetOutput();

  SizeValueType pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType xsize = output->GetRequestedRegion().GetSize()[0];
  SizeValueType linecount = pixelcount / xsize;

  m_LineMap.clear();
  m_LineMap.resize(linecount);
}


// -----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputRegionType & outputRegionForThread)
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
    LineEncodingType thisLine;
    while (!inLineIt.IsAtEndOfLine())
    {
      InputPixelType PVal = inLineIt.Get();

      SizeValueType  length = 0;
      InputIndexType thisIndex = inLineIt.GetIndex();
      outLineIt.Set(m_BackgroundValue);
      ++length;
      ++inLineIt;
      ++outLineIt;
      while (!inLineIt.IsAtEndOfLine() && inLineIt.Get() == PVal)
      {
        outLineIt.Set(m_BackgroundValue);
        ++length;
        ++inLineIt;
        ++outLineIt;
      }
      // create the run length object to go in the vector
      RunLength thisRun = { length, thisIndex, static_cast<InternalLabelType>(PVal) };

      thisLine.push_back(thisRun);
    }
    m_LineMap[lineId] = thisLine;
  }
}

template <typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>::ThreadedIntegrateData(
  const OutputRegionType & outputRegionForThread)
{
  OutputImageType * output = this->GetOutput();

  using OutputLineIteratorType = ImageScanlineIterator<OutputImageType>;
  OutputLineIteratorType outLineIt(output, outputRegionForThread);

  SizeValueType   pixelcount = output->GetRequestedRegion().GetNumberOfPixels();
  SizeValueType   xsize = output->GetRequestedRegion().GetSize()[0];
  OffsetValueType linecount = pixelcount / xsize;
  itkAssertInDebugAndIgnoreInReleaseMacro(SizeValueType(linecount) == m_LineMap.size());

  for (outLineIt.GoToBegin(); !outLineIt.IsAtEnd(); outLineIt.NextLine())
  {
    SizeValueType thisIdx = this->IndexToLinearIndex(outLineIt.GetIndex());
    if (!m_LineMap[thisIdx].empty())
    {
      for (OffsetVectorConstIterator I = this->m_LineOffsets.begin(); I != this->m_LineOffsets.end(); ++I)
      {
        OffsetValueType neighIdx = thisIdx + (*I);

        // check if the neighbor is in the map
        if (neighIdx >= 0 && neighIdx < linecount)
        {
          if (!m_LineMap[neighIdx].empty())
          {
            // Now check whether they are really neighbors
            bool areNeighbors = this->CheckNeighbors(m_LineMap[thisIdx][0].where, m_LineMap[neighIdx][0].where);
            if (areNeighbors)
            {
              this->CompareLines(m_LineMap[thisIdx],
                                 m_LineMap[neighIdx],
                                 true,
                                 true,
                                 m_BackgroundValue,
                                 [output](const LineEncodingConstIterator & currentRun,
                                          const LineEncodingConstIterator &,
                                          OffsetValueType oStart,
                                          OffsetValueType oLast) {
                                   itkAssertInDebugAndIgnoreInReleaseMacro(oStart <= oLast);
                                   OutputIndexType idx = currentRun->where;
                                   for (OffsetValueType x = oStart; x <= oLast; ++x)
                                   {
                                     idx[0] = x;
                                     output->SetPixel(idx, currentRun->label);
                                   }
                                 });
            }
          }
        }
      }
    }
  }
}

// -----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>::AfterThreadedGenerateData()
{
  m_LineMap.clear();
}

// -----------------------------------------------------------------------------
template <typename TInputImage, typename TOutputImage>
void
LabelContourImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: " << this->m_FullyConnected << std::endl;
  os << indent
     << "BackgroundValue: " << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue)
     << std::endl;
}

} // end namespace itk

#endif
