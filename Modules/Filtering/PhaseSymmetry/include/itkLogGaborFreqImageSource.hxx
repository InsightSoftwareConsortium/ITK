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
#ifndef __itkLogGaborFreqImageSource_hxx
#define __itkLogGaborFreqImageSource_hxx

#include "itkLogGaborFreqImageSource.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

template <typename TOutputImage>
LogGaborFreqImageSource<TOutputImage>::LogGaborFreqImageSource()
  : m_Sigma(1.0)
{
  // Gaussian parameters, defined so that the gaussian
  // is centered in the default image
  m_Wavelengths.Fill(2.0);
}


template <typename TOutputImage>
LogGaborFreqImageSource<TOutputImage>::~LogGaborFreqImageSource()
{}


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
LogGaborFreqImageSource<TOutputImage>::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                                                            ThreadIdType                  itkNotUsed(threadId))
{
  OutputImageType * outputPtr = this->GetOutput();
  const SizeType    size = this->GetSize();

  PointType centerPoint;
  for (unsigned int ii = 0; ii < ImageDimension; ++ii)
  {
    centerPoint[ii] = double(size[ii]) / 2.0;
  }

  typedef ImageRegionIteratorWithIndex<OutputImageType> OutputIteratorType;
  OutputIteratorType                                    outIt(outputPtr, outputRegionForThread);

  double sigma = vcl_log(m_Sigma);
  sigma *= sigma;
  sigma *= 2;

  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    const typename OutputImageType::IndexType index = outIt.GetIndex();
    // std::cout << "index: " << index << std::endl;

    double radius = 0.0;
    for (unsigned int ii = 0; ii < ImageDimension; ++ii)
    {
      const double dist = (centerPoint[ii] - double(index[ii])) / double(size[ii]);
      // %todo: is this correct for odd numbers?
      // const SizeValueType halfLength = size[ii] / 2;
      // const double dist = (index[ii] % halfLength) / double(halfLength);
      radius += dist * dist * m_Wavelengths[ii] * m_Wavelengths[ii];
    }
    radius = std::sqrt(radius);
    // std::cout << "radius: " << radius << std::endl;

    radius = vcl_log(radius);
    radius *= radius;

    double logGaborValue = vcl_exp(-radius / sigma);
    outIt.Set(static_cast<typename TOutputImage::PixelType>(logGaborValue));
  }
}

} // end namespace itk

#endif
