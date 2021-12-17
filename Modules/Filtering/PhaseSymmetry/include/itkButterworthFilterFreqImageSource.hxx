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

#ifndef itkButterworthFilterFreqImageSource_hxx
#define itkButterworthFilterFreqImageSource_hxx

#include "itkImageRegionIteratorWithIndex.h"


namespace itk
{

template <typename TOutputImage>
ButterworthFilterFreqImageSource<TOutputImage>::ButterworthFilterFreqImageSource()

  = default;


template <typename TOutputImage>
ButterworthFilterFreqImageSource<TOutputImage>::~ButterworthFilterFreqImageSource() = default;


template <typename TOutputImage>
void
ButterworthFilterFreqImageSource<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Cutoff: " << this->GetCutoff() << std::endl;
  os << indent << "Order:  " << this->GetOrder() << std::endl;
}


template <typename TOutputImage>
void
ButterworthFilterFreqImageSource<TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  OutputImageType * outputPtr = this->GetOutput();
  const SizeType    size = this->GetSize();

  typename OutputImageType::PointType centerPoint;
  for (unsigned int ii = 0; ii < ImageDimension; ++ii)
  {
    centerPoint[ii] = double(size[ii]) / 2.0;
  }

  using OutputIteratorType = ImageRegionIteratorWithIndex<OutputImageType>;
  OutputIteratorType outIt(outputPtr, outputRegionForThread);
  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    const typename TOutputImage::IndexType index = outIt.GetIndex();
    // std::cout << "index: " << index << std::endl;

    double radius = 0.0;
    for (unsigned int ii = 0; ii < ImageDimension; ++ii)
    {
      const double dist = (centerPoint[ii] - double(index[ii])) / double(size[ii]);
      // %todo: is this correct for odd numbers?
      // const SizeValueType halfLength = size[ii] / 2;
      // const double dist = (index[ii] % halfLength) / double(halfLength);
      radius += dist * dist;
    }
    radius = std::sqrt(radius);
    // std::cout << "radius: " << radius << std::endl;

    double value = 0.0;
    value = radius / m_Cutoff;
    value = std::pow(value, 2 * m_Order);
    value = 1. / (1. + value);

    outIt.Set(static_cast<typename TOutputImage::PixelType>(value));
  }
}

} // end namespace itk

#endif
