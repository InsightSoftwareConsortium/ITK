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
#ifndef itkLogGaborFreqImageSource_hxx
#define itkLogGaborFreqImageSource_hxx

#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

template <typename TOutputImage>
LogGaborFreqImageSource<TOutputImage>::LogGaborFreqImageSource()

{
  // Gaussian parameters, defined so that the gaussian
  // is centered in the default image
  m_Wavelengths.Fill(2.0);
}


template <typename TOutputImage>
LogGaborFreqImageSource<TOutputImage>::~LogGaborFreqImageSource() = default;


template <typename TOutputImage>
void
LogGaborFreqImageSource<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Sigma: " << m_Sigma << std::endl;
  os << indent << "Wavelengths: " << m_Wavelengths << std::endl;
}


template <typename TOutputImage>
void
LogGaborFreqImageSource<TOutputImage>::DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread)
{
  OutputImageType * outputPtr = this->GetOutput();
  const SizeType    size = this->GetSize();

  PointType centerPoint;
  for (unsigned int ii = 0; ii < ImageDimension; ++ii)
  {
    centerPoint[ii] = double(size[ii]) / 2.0;
  }

  using OutputIteratorType = ImageRegionIteratorWithIndex<OutputImageType>;
  OutputIteratorType outIt(outputPtr, outputRegionForThread);

  double sigma = std::log(m_Sigma);
  sigma *= sigma;
  sigma *= 2;

  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    const typename OutputImageType::IndexType index = outIt.GetIndex();

    double radius = 0.0;
    for (unsigned int ii = 0; ii < ImageDimension; ++ii)
    {
      const double dist = (centerPoint[ii] - double(index[ii])) / double(size[ii]);
      radius += dist * dist * m_Wavelengths[ii] * m_Wavelengths[ii];
    }
    if (radius == 0.0)
    {
      outIt.Set(static_cast<typename TOutputImage::PixelType>(0.0));
      continue;
    }
    radius = std::sqrt(radius);

    radius = std::log(radius);
    radius *= radius;

    double logGaborValue = std::exp(-radius / sigma);
    outIt.Set(static_cast<typename TOutputImage::PixelType>(logGaborValue));
  }
}

} // end namespace itk

#endif
