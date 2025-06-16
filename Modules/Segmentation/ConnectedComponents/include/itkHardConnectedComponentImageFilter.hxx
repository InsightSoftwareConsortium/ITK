/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkHardConnectedComponentImageFilter_hxx
#define itkHardConnectedComponentImageFilter_hxx
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkMath.h"
#include "itkMakeUniqueForOverwrite.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
HardConnectedComponentImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  using LabelType = unsigned short;

  const auto equivalenceTable = make_unique_for_overwrite<LabelType[]>(NumericTraits<LabelType>::max());

  LabelType maxLabel = 0;

  TOutputImage *      output = this->GetOutput();
  const TInputImage * input = this->GetInput();

  SizeType         size = input->GetLargestPossibleRegion().GetSize();
  const RegionType region(size);
  output->SetRegions(region);
  output->Allocate();

  ImageRegionConstIterator<TInputImage> it(input, input->GetRequestedRegion());
  ImageRegionIterator<TOutputImage>     ot(output, output->GetRequestedRegion());

  ProgressReporter progress(this, 0, output->GetRequestedRegion().GetNumberOfPixels());
  it.GoToBegin();
  ot.GoToBegin();
  for (; !it.IsAtEnd(); ++it, ++ot)
  {
    if (Math::NotExactlyEquals(it.Get(), typename ImageRegionConstIterator<TInputImage>::PixelType{}))
    {
      ot.Set(NumericTraits<typename TOutputImage::PixelType>::max());
    }
    else
    {
      ot.Set(typename TOutputImage::PixelType{});
    }
  }
  equivalenceTable[0] = 0;
  ot.GoToBegin();
  for (; !ot.IsAtEnd(); ++ot)
  {
    if (ot.Get())
    {
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        IndexType currentIndex = ot.GetIndex();
        currentIndex[i] = currentIndex[i] - 1;
        LabelType label = 0;
        if (currentIndex[i] >= 0)
        {
          label = static_cast<LabelType>(output->GetPixel(currentIndex));
        }
        if (label)
        {
          if (ot.Get() == NumericTraits<OutputPixelType>::max())
          {
            ot.Set(label);
          }
          else if ((ot.Get() != label) &&
                   (equivalenceTable[static_cast<LabelType>(ot.Get())] != equivalenceTable[label]))
          {
            if (equivalenceTable[static_cast<LabelType>(ot.Get())] > equivalenceTable[label])
            {
              int q = equivalenceTable[static_cast<LabelType>(ot.Get())];
              for (int p = q; p <= maxLabel; ++p)
              {
                if (equivalenceTable[p] == q)
                {
                  equivalenceTable[p] = equivalenceTable[label];
                }
              }
            }
            else
            {
              int q = equivalenceTable[label];
              for (int p = q; p <= maxLabel; ++p)
              {
                if (equivalenceTable[p] == q)
                {
                  equivalenceTable[p] = equivalenceTable[static_cast<LabelType>(ot.Get())];
                }
              }
            }
          }
        }
      }
      if (ot.Get() == NumericTraits<OutputPixelType>::max())
      {
        ++maxLabel;
        equivalenceTable[maxLabel] = maxLabel;
        ot.Set(maxLabel);
        if (maxLabel == NumericTraits<LabelType>::max())
        {
          return;
        }
      }
    }
    progress.CompletedPixel();
  }

  for (int p = 1; p <= maxLabel; ++p)
  {
    int m = p;
    for (; (m <= maxLabel) && (equivalenceTable[m] != p); ++m)
    {
    }
    if (m > maxLabel)
    {
      for (m = p; (m <= maxLabel) && (equivalenceTable[m] < p); ++m)
      {
      }
      if (m <= maxLabel)
      {
        for (unsigned int i = m; i <= maxLabel; ++i)
        {
          if (equivalenceTable[i] == m)
          {
            equivalenceTable[i] = p;
          }
        }
      }
    }
  }

  const auto flags = make_unique_for_overwrite<unsigned char[]>(NumericTraits<LabelType>::max());
  memset(flags.get(), 0, maxLabel + 1);
  for (auto iter = m_Seeds.begin(); iter != m_Seeds.end(); ++iter)
  {
    const IndexType currentIndex = *iter;
    int             m = equivalenceTable[static_cast<LabelType>(output->GetPixel(currentIndex))];
    for (unsigned int i = m; i <= maxLabel; ++i)
    {
      if (equivalenceTable[i] == m)
      {
        flags[i] = 1;
      }
    }
  }

  ot.GoToBegin();
  if (m_Seeds.empty())
  {
    for (; !ot.IsAtEnd(); ++ot)
    {
      ot.Set(equivalenceTable[static_cast<LabelType>(ot.Get())]);
    }
  }
  else
  {
    for (; !ot.IsAtEnd(); ++ot)
    {
      ot.Set(flags[static_cast<LabelType>(ot.Get())]);
    }
  }
}

} // end namespace itk

#endif
