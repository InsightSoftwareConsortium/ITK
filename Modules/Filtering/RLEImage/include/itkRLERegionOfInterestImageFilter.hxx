/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkRLERegionOfInterestImageFilter.h"

#include "itkRegionOfInterestImageFilter.h"
#include "itkImageAlgorithm.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"
#include "itkImage.h"

namespace itk
{
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

/**
 * RegionOfInterestImageFilter can produce an image which is a different size
 * than its input image.  As such, RegionOfInterestImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton()
 */
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
}

/**
 * RegionOfInterestImageFilter can be implemented as a multithreaded filter.
 * Therefore, this implementation provides a ThreadedGenerateData()
 * routine which is called for each processing thread. The output
 * image data is allocated automatically by the superclass prior to
 * calling ThreadedGenerateData().  ThreadedGenerateData can only
 * write to the portion of the output image specified by the
 * parameter "outputRegionForThread"
 *
 * \sa ImageToImageFilter::ThreadedGenerateData(),
 *     ImageToImageFilter::GenerateData()
 */
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<
  RLEImage<TPixel, VImageDimension, CounterType>,
  RLEImage<TPixel, VImageDimension, CounterType>>::ThreadedGenerateData(const RegionType & outputRegionForThread,
                                                                        ThreadIdType       threadId)
{
  // Get the input and output pointers
  const RLEImageType * in = this->GetInput();
  RLEImageType *       out = this->GetOutput();

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

  bool copyLines = (in->GetLargestPossibleRegion().GetSize(0) == outputRegionForThread.GetSize(0));
  typename ImageType::BufferType::RegionType               oReg = ImageType::truncateRegion(outputRegionForThread),
                                                           iReg = ImageType::truncateRegion(inputRegionForThread);
  ImageRegionConstIterator<typename ImageType::BufferType> iIt(in->GetBuffer(), iReg);
  ImageRegionIterator<typename ImageType::BufferType>      oIt(out->GetBuffer(), oReg);

  while (!oIt.IsAtEnd())
  {
    if (copyLines)
      oIt.Set(iIt.Get());
    else // determine begin and end iterator and copy range
    {
      typename RLEImageType::RLLine & oLine = oIt.Value();
      oLine.clear();
      const typename RLEImageType::RLLine & iLine = iIt.Value();
      CounterType                           t = 0;
      SizeValueType                         x = 0;
      // find start
      for (; x < iLine.size(); x++)
      {
        t += iLine[x].first;
        if (t > start[0])
          break;
      }
      assert(x < iLine.size());

      SizeValueType begin = x;
      if (t >= end[0]) // both begin and end are in this segment
      {
        oLine.push_back(typename RLEImageType::RLSegment(end[0] - start[0], iLine[x].second));
        ++iIt;
        ++oIt;
        continue; // next line
      }
      else if (t - start[0] < iLine[x].first) // not the first pixel in segment
      {
        oLine.push_back(typename RLEImageType::RLSegment(t - start[0], iLine[x].second));
        begin++; // start copying from next segment
      }

      // if (t < end[0])
      for (x++; x < iLine.size(); x++)
      {
        t += iLine[x].first;
        if (t >= end[0])
          break;
      }
      if (t == end[0])
        oLine.insert(oLine.end(), iLine.begin() + begin, iLine.begin() + x + 1);
      else // we need to take special care of the last segment
      {
        oLine.insert(oLine.end(), iLine.begin() + begin, iLine.begin() + x);
        oLine.push_back(typename RLEImageType::RLSegment(end[0] + iLine[x].first - t, iLine[x].second));
      }
    }
    ++iIt;
    ++oIt;
  }
}


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

/**
 * RegionOfInterestImageFilter can produce an image which is a different size
 * than its input image.  As such, RegionOfInterestImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton()
 */
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
}

/**
 * RegionOfInterestImageFilter can be implemented as a multithreaded filter.
 * Therefore, this implementation provides a ThreadedGenerateData()
 * routine which is called for each processing thread. The output
 * image data is allocated automatically by the superclass prior to
 * calling ThreadedGenerateData().  ThreadedGenerateData can only
 * write to the portion of the output image specified by the
 * parameter "outputRegionForThread"
 *
 * \sa ImageToImageFilter::ThreadedGenerateData(),
 *     ImageToImageFilter::GenerateData()
 */
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<Image<TPixel, VImageDimension>, RLEImage<TPixel, VImageDimension, CounterType>>::
  ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType threadId)
{
  // Get the input and output pointers
  const ImageType * in = this->GetInput();
  RLEImageType *    out = this->GetOutput();

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

  typename RLEImageType::BufferType::RegionType          oReg = RLEImageType::truncateRegion(outputRegionForThread);
  ImageRegionConstIterator<ImageType>                    iIt(in, inputRegionForThread);
  ImageRegionIterator<typename RLEImageType::BufferType> oIt(out->GetBuffer(), oReg);
  SizeValueType                                          size0 = outputRegionForThread.GetSize(0);
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
}


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

/**
 * RegionOfInterestImageFilter can produce an image which is a different size
 * than its input image.  As such, RegionOfInterestImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton()
 */
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
}

/**
 * RegionOfInterestImageFilter can be implemented as a multithreaded filter.
 * Therefore, this implementation provides a ThreadedGenerateData()
 * routine which is called for each processing thread. The output
 * image data is allocated automatically by the superclass prior to
 * calling ThreadedGenerateData().  ThreadedGenerateData can only
 * write to the portion of the output image specified by the
 * parameter "outputRegionForThread"
 *
 * \sa ImageToImageFilter::ThreadedGenerateData(),
 *     ImageToImageFilter::GenerateData()
 */
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
void
RegionOfInterestImageFilter<RLEImage<TPixel, VImageDimension, CounterType>, Image<TPixel, VImageDimension>>::
  ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType threadId)
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

  typename RLEImageType::BufferType::RegionType               iReg = RLEImageType::truncateRegion(inputRegionForThread);
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
        break;
    }
    assert(x < iLine.size());

    SizeValueType begin = x;
    if (t >= end[0]) // both begin and end are in this segment
    {
      for (SizeValueType i = start[0]; i < end[0]; i++)
      {
        oIt.Set(iLine[x].second);
        ++oIt;
      }
      ++iIt;
      continue; // next line
    }
    // else handle the beginning segment
    for (SizeValueType i = start[0]; i < t; i++)
    {
      oIt.Set(iLine[x].second);
      ++oIt;
    }
    // now handle middle segments
    for (x++; x < iLine.size(); x++)
    {
      t += iLine[x].first;
      if (t >= end[0])
        break;
      for (SizeValueType i = 0; i < iLine[x].first; i++)
      {
        oIt.Set(iLine[x].second);
        ++oIt;
      }
    }
    // handle the last segment
    for (SizeValueType i = 0; i < end[0] + iLine[x].first - t; i++)
    {
      oIt.Set(iLine[x].second);
      ++oIt;
    }
    ++iIt;
  }
}
} // end namespace itk

#endif // itkRLERegionOfInterestImageFilter_hxx
