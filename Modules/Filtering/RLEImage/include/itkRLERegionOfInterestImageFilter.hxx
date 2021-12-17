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
#ifndef itkRLERegionOfInterestImageFilter_hxx
#define itkRLERegionOfInterestImageFilter_hxx


#include "itkImage.h"
#include "itkImageAlgorithm.h"
#include "itkObjectFactory.h"
#include "itkRegionOfInterestImageFilter.h"
#include <typeinfo>

namespace itk
{
template <typename RLEImageTypeIn, typename RLEImageTypeOut>
void
copyImagePortion(ImageRegionConstIterator<typename RLEImageTypeIn::BufferType> iIt,
                 ImageRegionIterator<typename RLEImageTypeOut::BufferType>     oIt,
                 IndexValueType                                                start0,
                 IndexValueType                                                end0)
{
  while (!oIt.IsAtEnd())
  {
    // determine begin and end iterator and copy range
    typename RLEImageTypeOut::RLLine & oLine = oIt.Value();
    oLine.clear();
    const typename RLEImageTypeIn::RLLine & iLine = iIt.Value();
    typename RLEImageTypeIn::RLCounterType  t = 0;
    SizeValueType                           x = 0;
    // find start
    for (; x < iLine.size(); x++)
    {
      t += iLine[x].first;
      if (t > start0)
      {
        break;
      }
    }
    assert(x < iLine.size());

    SizeValueType begin = x;
    if (t >= end0) // both begin and end are in this segment
    {
      oLine.push_back(
        typename RLEImageTypeOut::RLSegment(typename RLEImageTypeOut::RLCounterType(end0 - start0), iLine[x].second));
      ++iIt;
      ++oIt;
      continue; // next line
    }
    else if (t - start0 < iLine[x].first) // not the first pixel in segment
    {
      oLine.push_back(
        typename RLEImageTypeOut::RLSegment(typename RLEImageTypeOut::RLCounterType(t - start0), iLine[x].second));
      begin++; // start copying from next segment
    }
    for (x++; x < iLine.size(); x++)
    {
      t += iLine[x].first;
      if (t >= end0)
      {
        break;
      }
    }
    if (t == end0)
    {
      oLine.insert(oLine.end(), iLine.begin() + begin, iLine.begin() + x + 1);
    }
    else // we need to take special care of the last segment
    {
      oLine.insert(oLine.end(), iLine.begin() + begin, iLine.begin() + x);
      oLine.push_back(typename RLEImageTypeOut::RLSegment(
        typename RLEImageTypeOut::RLCounterType(end0 + iLine[x].first - t), iLine[x].second));
    }

    ++iIt;
    ++oIt;
  }
} // copyImagePortion

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                            RLEImage<TPixel, VImageDimension, CounterType>>::PrintSelf(std::ostream & os,
                                                                                       Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "RegionOfInterest: " << m_RegionOfInterest << std::endl;
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                            RLEImage<TPixel, VImageDimension, CounterType>>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointer to the input
  typename Superclass::InputImagePointer inputPtr = const_cast<RLEImageType *>(this->GetInput());

  if (inputPtr)
  {
    // request the region of interest
    inputPtr->SetRequestedRegion(m_RegionOfInterest);
  }
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                            RLEImage<TPixel, VImageDimension, CounterType>>::EnlargeOutputRequestedRegion(DataObject *
                                                                                                            output)
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // generate everything in the region of interest
  output->SetRequestedRegionToLargestPossibleRegion();
}


template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                            RLEImage<TPixel, VImageDimension, CounterType>>::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // get pointers to the input and output
  typename Superclass::OutputImagePointer     outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();

  if (!outputPtr || !inputPtr)
  {
    return;
  }

  // Set the output image size to the same value as the region of interest.
  RegionType region;
  IndexType  start;
  start.Fill(0);

  region.SetSize(m_RegionOfInterest.GetSize());
  region.SetIndex(start);

  // Copy Information without modification.
  outputPtr->CopyInformation(inputPtr);

  // Adjust output region
  outputPtr->SetLargestPossibleRegion(region);

  // Correct origin of the extracted region.
  IndexType                                       roiStart(m_RegionOfInterest.GetIndex());
  typename Superclass::OutputImageType::PointType outputOrigin;
  inputPtr->TransformIndexToPhysicalPoint(roiStart, outputOrigin);
  outputPtr->SetOrigin(outputOrigin);
} // >::GenerateOutputInformation


template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<
  RLEImage<TPixel, VImageDimension, CounterType>,
  RLEImage<TPixel, VImageDimension, CounterType>>::DynamicThreadedGenerateData(const RegionType & outputRegionForThread)
{
  // Get the input and output pointers
  const RLEImageType * in = this->GetInput();
  RLEImageType *       out = this->GetOutput();

  // Concurrent writing to RLLine is not supported,
  // so we ignore all the output regions which do not start
  // at the beginning of a line.
  // But the regions which do start at the beginning of a line
  // must process the whole line
  RegionType reqRegion = out->GetRequestedRegion();
  if (reqRegion.GetIndex(0) != outputRegionForThread.GetIndex(0))
  {
    return; // another thread will process this
  }
  RegionType outRegion = outputRegionForThread;
  outRegion.SetSize(0, reqRegion.GetSize(0));

  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;

  inputRegionForThread.SetSize(outRegion.GetSize());

  IndexType start, end;
  IndexType roiStart(m_RegionOfInterest.GetIndex());
  IndexType threadStart(outRegion.GetIndex());
  for (unsigned int i = 0; i < VImageDimension; i++)
  {
    start[i] = roiStart[i] + threadStart[i];
    end[i] = roiStart[i] + threadStart[i] + outRegion.GetSize(i);
  }
  inputRegionForThread.SetIndex(start);

  bool copyLines = (in->GetLargestPossibleRegion().GetSize(0) == outRegion.GetSize(0));
  typename ImageType::BufferType::RegionType               oReg = outRegion.Slice(0);
  typename ImageType::BufferType::RegionType               iReg = inputRegionForThread.Slice(0);
  ImageRegionConstIterator<typename ImageType::BufferType> iIt(in->GetBuffer(), iReg);
  ImageRegionIterator<typename ImageType::BufferType>      oIt(out->GetBuffer(), oReg);

  if (copyLines)
  {
    while (!oIt.IsAtEnd())
    {
      oIt.Set(iIt.Get());

      ++iIt;
      ++oIt;
    }
  }
  else
  {
    copyImagePortion<ImageType, ImageType>(iIt, oIt, start[0], end[0]);
  }
} // DynamicThreadedGenerateData

template <typename TPixelIn,
          typename TPixelOut,
          unsigned int VImageDimension,
          typename CounterTypeIn,
          typename CounterTypeOut>
void
RegionOfInterestImageFilter<RLEImage<TPixelIn, VImageDimension, CounterTypeIn>,
                            RLEImage<TPixelOut, VImageDimension, CounterTypeOut>>::PrintSelf(std::ostream & os,
                                                                                             Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "RegionOfInterest: " << m_RegionOfInterest << std::endl;
}

template <typename TPixelIn,
          typename TPixelOut,
          unsigned int VImageDimension,
          typename CounterTypeIn,
          typename CounterTypeOut>
void
RegionOfInterestImageFilter<RLEImage<TPixelIn, VImageDimension, CounterTypeIn>,
                            RLEImage<TPixelOut, VImageDimension, CounterTypeOut>>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointer to the input
  typename Superclass::InputImagePointer inputPtr = const_cast<RLEImageTypeIn *>(this->GetInput());

  if (inputPtr)
  {
    // request the region of interest
    inputPtr->SetRequestedRegion(m_RegionOfInterest);
  }
}

template <typename TPixelIn,
          typename TPixelOut,
          unsigned int VImageDimension,
          typename CounterTypeIn,
          typename CounterTypeOut>
void
RegionOfInterestImageFilter<
  RLEImage<TPixelIn, VImageDimension, CounterTypeIn>,
  RLEImage<TPixelOut, VImageDimension, CounterTypeOut>>::EnlargeOutputRequestedRegion(DataObject * output)
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // generate everything in the region of interest
  output->SetRequestedRegionToLargestPossibleRegion();
}


template <typename TPixelIn,
          typename TPixelOut,
          unsigned int VImageDimension,
          typename CounterTypeIn,
          typename CounterTypeOut>
void
RegionOfInterestImageFilter<RLEImage<TPixelIn, VImageDimension, CounterTypeIn>,
                            RLEImage<TPixelOut, VImageDimension, CounterTypeOut>>::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // get pointers to the input and output
  typename Superclass::OutputImagePointer     outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();

  if (!outputPtr || !inputPtr)
  {
    return;
  }

  // Set the output image size to the same value as the region of interest.
  RegionType region;
  IndexType  start;
  start.Fill(0);

  region.SetSize(m_RegionOfInterest.GetSize());
  region.SetIndex(start);

  // Copy Information without modification.
  outputPtr->CopyInformation(inputPtr);

  // Adjust output region
  outputPtr->SetLargestPossibleRegion(region);

  // Correct origin of the extracted region.
  IndexType                                       roiStart(m_RegionOfInterest.GetIndex());
  typename Superclass::OutputImageType::PointType outputOrigin;
  inputPtr->TransformIndexToPhysicalPoint(roiStart, outputOrigin);
  outputPtr->SetOrigin(outputOrigin);
} // >::GenerateOutputInformation


template <typename TPixelIn,
          typename TPixelOut,
          unsigned int VImageDimension,
          typename CounterTypeIn,
          typename CounterTypeOut>
void
RegionOfInterestImageFilter<RLEImage<TPixelIn, VImageDimension, CounterTypeIn>,
                            RLEImage<TPixelOut, VImageDimension, CounterTypeOut>>::
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread)
{
  // Get the input and output pointers
  const RLEImageTypeIn * in = this->GetInput();
  RLEImageTypeOut *      out = this->GetOutput();

  // Concurrent writing to RLLine is not supported,
  // so we ignore all the output regions which do not start
  // at the beginning of a line.
  // But the regions which do start at the beginning of a line
  // must process the whole line
  RegionType reqRegion = out->GetRequestedRegion();
  if (reqRegion.GetIndex(0) != outputRegionForThread.GetIndex(0))
  {
    return; // another thread will process this
  }
  RegionType outRegion = outputRegionForThread;
  outRegion.SetSize(0, reqRegion.GetSize(0));

  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;

  inputRegionForThread.SetSize(outRegion.GetSize());

  IndexType start, end;
  IndexType roiStart(m_RegionOfInterest.GetIndex());
  IndexType threadStart(outRegion.GetIndex());
  for (unsigned int i = 0; i < VImageDimension; i++)
  {
    start[i] = roiStart[i] + threadStart[i];
    end[i] = roiStart[i] + threadStart[i] + outRegion.GetSize(i);
  }
  inputRegionForThread.SetIndex(start);

  typename RLEImageTypeIn::BufferType::RegionType               iReg = inputRegionForThread.Slice(0);
  typename RLEImageTypeOut::BufferType::RegionType              oReg = outRegion.Slice(0);
  ImageRegionConstIterator<typename RLEImageTypeIn::BufferType> iIt(in->GetBuffer(), iReg);
  ImageRegionIterator<typename RLEImageTypeOut::BufferType>     oIt(out->GetBuffer(), oReg);

  copyImagePortion<RLEImageTypeIn, RLEImageTypeOut>(iIt, oIt, start[0], end[0]);
} // DynamicThreadedGenerateData

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<Image<TPixel, VImageDimension>, RLEImage<TPixel, VImageDimension, CounterType>>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "RegionOfInterest: " << m_RegionOfInterest << std::endl;
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<Image<TPixel, VImageDimension>,
                            RLEImage<TPixel, VImageDimension, CounterType>>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointer to the input
  typename Superclass::InputImagePointer inputPtr = const_cast<ImageType *>(this->GetInput());

  if (inputPtr)
  {
    // request the region of interest
    inputPtr->SetRequestedRegion(m_RegionOfInterest);
  }
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<Image<TPixel, VImageDimension>, RLEImage<TPixel, VImageDimension, CounterType>>::
  EnlargeOutputRequestedRegion(DataObject * output)
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // generate everything in the region of interest
  output->SetRequestedRegionToLargestPossibleRegion();
}


template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<Image<TPixel, VImageDimension>,
                            RLEImage<TPixel, VImageDimension, CounterType>>::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // get pointers to the input and output
  typename Superclass::OutputImagePointer     outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();

  if (!outputPtr || !inputPtr)
  {
    return;
  }

  // Set the output image size to the same value as the region of interest.
  RegionType region;
  IndexType  start;
  start.Fill(0);

  region.SetSize(m_RegionOfInterest.GetSize());
  region.SetIndex(start);

  // Copy Information without modification.
  outputPtr->CopyInformation(inputPtr);

  // Adjust output region
  outputPtr->SetLargestPossibleRegion(region);

  // Correct origin of the extracted region.
  IndexType                                       roiStart(m_RegionOfInterest.GetIndex());
  typename Superclass::OutputImageType::PointType outputOrigin;
  inputPtr->TransformIndexToPhysicalPoint(roiStart, outputOrigin);
  outputPtr->SetOrigin(outputOrigin);
} // >::GenerateOutputInformation


template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<Image<TPixel, VImageDimension>, RLEImage<TPixel, VImageDimension, CounterType>>::
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread)
{
  // Get the input and output pointers
  const ImageType * in = this->GetInput();
  RLEImageType *    out = this->GetOutput();

  // Concurrent writing to RLLine is not supported,
  // so we ignore all the output regions which do not start
  // at the beginning of a line.
  // But the regions which do start at the beginning of a line
  // must process the whole line
  RegionType reqRegion = out->GetRequestedRegion();
  if (reqRegion.GetIndex(0) != outputRegionForThread.GetIndex(0))
  {
    return; // another thread will process this
  }
  RegionType outRegion = outputRegionForThread;
  outRegion.SetSize(0, reqRegion.GetSize(0));

  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;

  inputRegionForThread.SetSize(outRegion.GetSize());

  IndexType start, end;
  IndexType roiStart(m_RegionOfInterest.GetIndex());
  IndexType threadStart(outRegion.GetIndex());
  for (unsigned int i = 0; i < VImageDimension; i++)
  {
    start[i] = roiStart[i] + threadStart[i];
    end[i] = roiStart[i] + threadStart[i] + outRegion.GetSize(i);
  }
  inputRegionForThread.SetIndex(start);

  typename RLEImageType::BufferType::RegionType          oReg = outRegion.Slice(0);
  ImageRegionConstIterator<ImageType>                    iIt(in, inputRegionForThread);
  ImageRegionIterator<typename RLEImageType::BufferType> oIt(out->GetBuffer(), oReg);
  SizeValueType                                          size0 = outRegion.GetSize(0);
  typename RLEImageType::RLLine                          temp;
  temp.reserve(size0); // pessimistically preallocate buffer, otherwise reallocations can occur

  while (!oIt.IsAtEnd())
  {
    SizeValueType x = 0;
    temp.clear();
    while (x < size0)
    {
      typename RLEImageType::RLSegment s(0, iIt.Value());
      while (x < size0 && iIt.Value() == s.second)
      {
        x++;
        s.first++;
        ++(iIt);
      }

      temp.push_back(s);
    }

    oIt.Value() = temp;
    ++oIt;
  }
} // DynamicThreadedGenerateData

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>, Image<TPixel, VImageDimension>>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "RegionOfInterest: " << m_RegionOfInterest << std::endl;
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                            Image<TPixel, VImageDimension>>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointer to the input
  typename Superclass::InputImagePointer inputPtr = const_cast<RLEImageType *>(this->GetInput());

  if (inputPtr)
  {
    // request the region of interest
    inputPtr->SetRequestedRegion(m_RegionOfInterest);
  }
}

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                            Image<TPixel, VImageDimension>>::EnlargeOutputRequestedRegion(DataObject * output)
{
  // call the superclass' implementation of this method
  Superclass::EnlargeOutputRequestedRegion(output);

  // generate everything in the region of interest
  output->SetRequestedRegionToLargestPossibleRegion();
}


template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>,
                            Image<TPixel, VImageDimension>>::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions

  // get pointers to the input and output
  typename Superclass::OutputImagePointer     outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();

  if (!outputPtr || !inputPtr)
  {
    return;
  }

  // Set the output image size to the same value as the region of interest.
  RegionType region;
  IndexType  start;
  start.Fill(0);

  region.SetSize(m_RegionOfInterest.GetSize());
  region.SetIndex(start);

  // Copy Information without modification.
  outputPtr->CopyInformation(inputPtr);

  // Adjust output region
  outputPtr->SetLargestPossibleRegion(region);

  // Correct origin of the extracted region.
  IndexType                                       roiStart(m_RegionOfInterest.GetIndex());
  typename Superclass::OutputImageType::PointType outputOrigin;
  inputPtr->TransformIndexToPhysicalPoint(roiStart, outputOrigin);
  outputPtr->SetOrigin(outputOrigin);
} // >::GenerateOutputInformation

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>, Image<TPixel, VImageDimension>>::
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread)
{
  // Get the input and output pointers
  const RLEImageType * in = this->GetInput();
  ImageType *          out = this->GetOutput();

  // Define the portion of the input to walk for this thread
  InputImageRegionType inputRegionForThread;

  inputRegionForThread.SetSize(outputRegionForThread.GetSize());

  IndexType start, end;
  IndexType roiStart(m_RegionOfInterest.GetIndex());
  IndexType threadStart(outputRegionForThread.GetIndex());
  for (unsigned int i = 0; i < VImageDimension; i++)
  {
    start[i] = roiStart[i] + threadStart[i];
    end[i] = roiStart[i] + threadStart[i] + outputRegionForThread.GetSize(i);
  }
  inputRegionForThread.SetIndex(start);

  typename RLEImageType::BufferType::RegionType               iReg = inputRegionForThread.Slice(0);
  ImageRegionConstIterator<typename RLEImageType::BufferType> iIt(in->GetBuffer(), iReg);
  ImageRegionIterator<ImageType>                              oIt(out, outputRegionForThread);

  while (!iIt.IsAtEnd())
  {
    const typename RLEImageType::RLLine & iLine = iIt.Value();
    CounterType                           t = 0;
    SizeValueType                         x = 0;
    // find start
    for (; x < iLine.size(); x++)
    {
      t += iLine[x].first;
      if (t > start[0])
      {
        break;
      }
    }
    assert(x < iLine.size());

    if (t >= end[0]) // both begin and end are in this segment
    {
      for (IndexValueType i = start[0]; i < end[0]; i++)
      {
        oIt.Set(iLine[x].second);
        ++oIt;
      }
      ++iIt;
      continue; // next line
    }
    // else handle the beginning segment
    for (IndexValueType i = start[0]; i < t; i++)
    {
      oIt.Set(iLine[x].second);
      ++oIt;
    }
    // now handle middle segments
    for (x++; x < iLine.size(); x++)
    {
      t += iLine[x].first;
      if (t >= end[0])
      {
        break;
      }
      for (CounterType i = 0; i < iLine[x].first; i++)
      {
        oIt.Set(iLine[x].second);
        ++oIt;
      }
    }
    // handle the last segment
    for (IndexValueType i = 0; i < end[0] + iLine[x].first - t; i++)
    {
      oIt.Set(iLine[x].second);
      ++oIt;
    }
    ++iIt;
  }
} // DynamicThreadedGenerateData
} // end namespace itk

#endif // itkRLERegionOfInterestImageFilter_hxx
