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
#ifndef itkRelabelComponentImageFilter_hxx
#define itkRelabelComponentImageFilter_hxx

#include "itkRelabelComponentImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"
#include "itkProgressTransformer.h"
#include "itkImageScanlineIterator.h"
#include <map>
#include <utility>
#include "itkTotalProgressReporter.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
RelabelComponentImageFilter<TInputImage, TOutputImage>::RelabelComponentImageFilter()
{
  this->InPlaceOff();
  this->ThreaderUpdateProgressOff();
}

template <typename TInputImage, typename TOutputImage>
void
RelabelComponentImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetInput());
  if (input)
  {
    input->SetRequestedRegion(input->GetLargestPossibleRegion());
  }
}


template <typename TInputImage, typename TOutputImage>
void
RelabelComponentImageFilter<TInputImage, TOutputImage>::ParallelComputeLabels(const RegionType & inputRegionForThread)
{
  RelabelComponentObjectType initialSize;
  initialSize.m_SizeInPixels = 0;

  // walk the input
  ImageScanlineConstIterator<InputImageType> it(this->GetInput(), inputRegionForThread);

  auto                  inputRequestedRegion = this->GetInput()->GetRequestedRegion();
  TotalProgressReporter report(this, inputRequestedRegion.GetNumberOfPixels(), 100, 0.5f);

  MapType localSizeMap;

  auto mapIt = localSizeMap.end();
  while (!it.IsAtEnd())
  {
    while (!it.IsAtEndOfLine())
    {
      // Get the input pixel value
      const auto inputValue = it.Get();

      // if the input pixel is not the background
      if (inputValue != NumericTraits<LabelType>::ZeroValue())
      {
        // label is not currently in the map
        mapIt = localSizeMap.insert(mapIt, { inputValue, initialSize });

        // label is already in the map, update the values
        ++(mapIt->second.m_SizeInPixels);
      }

      // increment the iterator
      ++it;
    }
    report.Completed(inputRequestedRegion.GetSize(0));
    it.NextLine();
  }

  // Merge localStatistics and m_LabelStatistics concurrently safe in a
  // local copy, this thread may do multiple merges.
  while (true)
  {
    std::unique_lock<std::mutex> lock(m_Mutex);

    if (m_SizeMap.empty())
    {
      swap(m_SizeMap, localSizeMap);
      break;
    }
    else
    {
      // copy the output map to thread local storage
      MapType toMerge;
      swap(m_SizeMap, toMerge);

      // allow other threads to merge data
      lock.unlock();

      // Merge toMerge into localSizeMap, locally
      for (auto & sizePair : toMerge)
      {
        localSizeMap[sizePair.first] += sizePair.second;
      }
    }
  } // release lock
}


template <typename TInputImage, typename TOutputImage>
void
RelabelComponentImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  using LabelComponentPairType = std::pair<LabelType, RelabelComponentObjectType>;

  // Get the input and the output
  const TInputImage * input = this->GetInput();
  TOutputImage *      output = this->GetOutput();

  // Calculate the size of pixel
  float physicalPixelSize = 1.0;
  for (unsigned int i = 0; i < TInputImage::ImageDimension; ++i)
  {
    physicalPixelSize *= input->GetSpacing()[i];
  }

  // Walk the entire input image and compute used labels and the number of each label.
  this->GetMultiThreader()->template ParallelizeImageRegion<ImageDimension>(
    input->GetRequestedRegion(),
    [this](const RegionType & inputRegion) { this->ParallelComputeLabels(inputRegion); },
    nullptr);


  // Construct an array of the label, component information pair to sort
  auto sizeVector = std::vector<LabelComponentPairType>(m_SizeMap.begin(), m_SizeMap.end());

  // free memory by swapping to a default constructed object.
  MapType().swap(m_SizeMap);

  // Sort the objects by size by default, unless m_SortByObjectSize
  // is set to false.
  if (m_SortByObjectSize)
  {
    std::sort(sizeVector.begin(),
              sizeVector.end(),
              [](const LabelComponentPairType & a, const LabelComponentPairType & b) -> bool {
                return a.second.m_SizeInPixels > b.second.m_SizeInPixels ||
                       (!(a.second.m_SizeInPixels < b.second.m_SizeInPixels) && a.first < b.first);
              });
  }


  // A map from the input pixel labels to the output labels
  using RelabelMapType = std::map<LabelType, OutputPixelType>;
  RelabelMapType relabelMap;

  // create a lookup table to map the input label to the output label.
  // cache the object sizes for later access by the user
  m_NumberOfObjects = sizeVector.size();
  m_OriginalNumberOfObjects = sizeVector.size();
  m_SizeOfObjectsInPixels.clear();
  m_SizeOfObjectsInPixels.resize(m_NumberOfObjects);
  SizeValueType   NumberOfObjectsRemoved = 0;
  OutputPixelType outputLabel = 0;
  for (const auto & sizeVectorPair : sizeVector)
  {
    // skip objects that are too small ( but don't increment the output label )
    if (m_MinimumObjectSize > 0 && sizeVectorPair.second.m_SizeInPixels < m_MinimumObjectSize)
    {
      // map small objects to the background
      ++NumberOfObjectsRemoved;
      relabelMap.insert({ sizeVectorPair.first, NumericTraits<OutputPixelType>::ZeroValue() });
    }
    else
    {
      if (outputLabel == NumericTraits<OutputPixelType>::max())
      {
        itkExceptionMacro("Output voxel range exceeded for relabeling.  Too many objects of sufficient size found!");
      }
      // map for input labels to output labels (Note we use i+1 in the
      // map since index 0 is the background)
      relabelMap.insert({ sizeVectorPair.first, outputLabel + 1 });

      // cache object sizes for later access by the user
      m_SizeOfObjectsInPixels[outputLabel] = sizeVectorPair.second.m_SizeInPixels;
      ++outputLabel;
    }
  }

  // update number of objects and resize vectors if we have removed small
  // objects
  m_NumberOfObjects -= NumberOfObjectsRemoved;
  if (NumberOfObjectsRemoved > 0)
  {
    m_SizeOfObjectsInPixels.resize(m_NumberOfObjects);
  }

  // compute the object sizes in physical space too
  m_SizeOfObjectsInPhysicalUnits.resize(m_NumberOfObjects);
  std::transform(m_SizeOfObjectsInPixels.begin(),
                 m_SizeOfObjectsInPixels.end(),
                 m_SizeOfObjectsInPhysicalUnits.begin(),
                 [physicalPixelSize](ObjectSizeType sizeInPixels) { return sizeInPixels * physicalPixelSize; });


  // After the objects stats are computed add in the background label so the relabelMap can be directly applied.
  relabelMap.insert({ NumericTraits<LabelType>::ZeroValue(), NumericTraits<OutputPixelType>::ZeroValue() });

  // Second pass: walk just the output requested region and relabel
  // the necessary pixels.
  //

  // Allocate the output
  this->AllocateOutputs();

  // In parallel apply the relabling map
  this->GetMultiThreader()->template ParallelizeImageRegion<ImageDimension>(
    output->GetRequestedRegion(),
    [this, &relabelMap](const RegionType & outputRegionForThread) {
      auto                  outputRequestedRegion = this->GetOutput()->GetRequestedRegion();
      TotalProgressReporter report(this, outputRequestedRegion.GetNumberOfPixels(), 100, 0.5f);

      ImageScanlineIterator<OutputImageType>     oit(this->GetOutput(), outputRegionForThread);
      ImageScanlineConstIterator<InputImageType> it(this->GetInput(), outputRegionForThread);

      auto mapIt = relabelMap.cbegin();

      while (!oit.IsAtEnd())
      {
        while (!oit.IsAtEndOfLine())
        {
          const auto && inputValue = it.Get();

          if (mapIt->first != inputValue)
          {
            mapIt = relabelMap.find(inputValue);
          }

          // no new labels should be encountered in the input
          assert(mapIt != relabelMap.cend());

          oit.Set(mapIt->second);

          ++oit;
          ++it;
        }
        report.Completed(outputRequestedRegion.GetSize(0));
        oit.NextLine();
        it.NextLine();
      }
    },
    nullptr);
}

template <typename TInputImage, typename TOutputImage>
void
RelabelComponentImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfObjects: " << m_NumberOfObjects << std::endl;
  os << indent << "OriginalNumberOfObjects: " << m_OriginalNumberOfObjects << std::endl;
  os << indent << "NumberOfObjectsToPrint: " << m_NumberOfObjectsToPrint << std::endl;
  os << indent << "MinimumObjectSizes: " << m_MinimumObjectSize << std::endl;
  os << indent << "SortByObjectSize: " << m_SortByObjectSize << std::endl;

  typename ObjectSizeInPixelsContainerType::const_iterator it;
  ObjectSizeInPhysicalUnitsContainerType::const_iterator   fit;
  SizeValueType                                            i;

  // limit the number of objects to print
  SizeValueType numPrint = std::min<SizeValueType>(m_NumberOfObjectsToPrint, m_SizeOfObjectsInPixels.size());

  for (i = 0, it = m_SizeOfObjectsInPixels.begin(), fit = m_SizeOfObjectsInPhysicalUnits.begin(); i < numPrint;
       ++it, ++fit, ++i)
  {
    os << indent << "Object #" << i + 1 << ": " << *it << " pixels, " << *fit << " physical units" << std::endl;
  }
  if (numPrint < m_SizeOfObjectsInPixels.size())
  {
    os << indent << "..." << std::endl;
  }
}
} // end namespace itk

#endif
