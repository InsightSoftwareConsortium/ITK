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
{
  // Gaussian parameters, defined so that the gaussian
  // is centered in the default image
  m_Sigma = 1.0;
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
}


template <typename TOutputImage>
void
LogGaborFreqImageSource<TOutputImage>::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                                                            ThreadIdType                  itkNotUsed(threadId))
{
  // The a pointer to the output image
  typename TOutputImage::Pointer outputPtr = this->GetOutput();

  typedef ImageRegionIteratorWithIndex<TOutputImage> OutputIteratorType;
  OutputIteratorType                                 outIt = OutputIteratorType(outputPtr, outputRegionForThread);

  int ndims = TOutputImage::ImageDimension;

  DoubleArrayType centerPoint;
  for (int i = 0; i < ndims; i++)
  {
    centerPoint[i] = double(this->GetSize()[i]) / 2.0;
  }

  double radius = 0;
  double sigma = 0;
  double logGaborValue = 0;

  DoubleArrayType                  dist;
  typename TOutputImage::IndexType index;
  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    index = outIt.GetIndex();
    radius = 0;
    sigma = 0;
    logGaborValue = 0;

    for (int i = 0; i < TOutputImage::ImageDimension; i++)
    {
      dist[i] = (double(index[i]) - centerPoint[i]) / double(this->GetSize()[i]);
      radius = radius + dist[i] * dist[i] * m_Wavelengths[i] * m_Wavelengths[i];
    }
    radius = sqrt(radius);

    radius = vcl_log(radius) * vcl_log(radius);
    sigma = 2 * vcl_log(m_Sigma) * vcl_log(m_Sigma);
    logGaborValue = vcl_exp(-radius / sigma);

    // Set the pixel value to the function value
    outIt.Set(static_cast<typename TOutputImage::PixelType>(logGaborValue));
  }
}

} // end namespace itk

#endif
