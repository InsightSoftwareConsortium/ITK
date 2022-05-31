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
#ifndef itkSteerableFilterFreqImageSource_hxx
#define itkSteerableFilterFreqImageSource_hxx

#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include "itkObjectFactory.h"
#include <algorithm>

namespace itk
{

template <typename TOutputImage>
SteerableFilterFreqImageSource<TOutputImage>::SteerableFilterFreqImageSource()
{
  // Initial image is 64 wide in each direction.
  for (unsigned int i = 0; i < TOutputImage::GetImageDimension(); i++)
  {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
  }
  m_Direction.SetIdentity();

  // this->ReleaseDataBeforeUpdateFlagOn();
}


template <typename TOutputImage>
SteerableFilterFreqImageSource<TOutputImage>::~SteerableFilterFreqImageSource() = default;


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::GenerateOutputInformation()
{
  TOutputImage *                   output;
  typename TOutputImage::IndexType index = { { 0 } };
  typename TOutputImage::SizeType  size = { { 0 } };
  size.SetSize(m_Size);

  output = this->GetOutput(0);

  typename TOutputImage::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize(size);
  largestPossibleRegion.SetIndex(index);
  output->SetLargestPossibleRegion(largestPossibleRegion);

  output->SetSpacing(m_Spacing);
  output->SetOrigin(m_Origin);
  output->SetDirection(m_Direction);
}


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  // The a pointer to the output image

  typename TOutputImage::Pointer outputPtr = this->GetOutput();
  // outputPtr->SetBufferedRegion( outputRegionForThread );
  // outputPtr->Allocate();


  using OutputIterator = ImageRegionIteratorWithIndex<TOutputImage>;
  OutputIterator outIt = OutputIterator(outputPtr, outputRegionForThread);

  int ndims = TOutputImage::ImageDimension;

  double angularSigma;
  angularSigma = (m_AngularBandwidth / 2) / 1.1774;


  double          dangle;
  double          orientationRadius = 0;
  DoubleArrayType dist;
  DoubleArrayType centerPoint;

  double radius = 0;
  double angularGaussianValue = 0;
  double dotProduct = 0;

  for (int i = 0; i < ndims; i++)
  {
    orientationRadius = orientationRadius + m_Orientation[i] * m_Orientation[i];
    centerPoint[i] = double(m_Size[i]) / 2.0;
  }
  orientationRadius = sqrt(orientationRadius);

  typename TOutputImage::IndexType index;
  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    index = outIt.GetIndex();
    radius = 0;
    dotProduct = 0;

    for (unsigned int i = 0; i < TOutputImage::ImageDimension; i++)
    {
      dist[i] = (double(index[i]) - centerPoint[i]) / double(m_Size[i]);
      dotProduct = dotProduct + m_Orientation[i] * dist[i];
      radius = radius + (dist[i] * dist[i]);
    }
    radius = sqrt(radius);
    dotProduct = dotProduct / (radius * orientationRadius);
    dangle = acos(dotProduct);


    angularGaussianValue = exp(-((dangle * dangle) / (2 * angularSigma * angularSigma)));
    if (radius == 0)
    {
      angularGaussianValue = 1.0;
    }
    // Set the pixel value to the function value
    outIt.Set((typename TOutputImage::PixelType)angularGaussianValue);
  }
}


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::SetSpacing(const float * spacing)
{
  unsigned int i;
  for (i = 0; i < TOutputImage::ImageDimension; i++)
  {
    if ((double)spacing[i] != m_Spacing[i])
    {
      break;
    }
  }
  if (i < TOutputImage::ImageDimension)
  {
    for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
      m_Spacing[i] = spacing[i];
    }
    this->Modified();
  }
}


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::SetSpacing(const double * spacing)
{
  unsigned int i;
  for (i = 0; i < TOutputImage::ImageDimension; i++)
  {
    if (spacing[i] != m_Spacing[i])
    {
      break;
    }
  }
  if (i < TOutputImage::ImageDimension)
  {
    for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
      m_Spacing[i] = spacing[i];
    }
    this->Modified();
  }
}


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::SetOrigin(const float * origin)
{
  unsigned int i;
  for (i = 0; i < TOutputImage::ImageDimension; i++)
  {
    if ((double)origin[i] != m_Origin[i])
    {
      break;
    }
  }
  if (i < TOutputImage::ImageDimension)
  {
    for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
      m_Origin[i] = origin[i];
    }
    this->Modified();
  }
}


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::SetOrigin(const double * origin)
{
  unsigned int i;
  for (i = 0; i < TOutputImage::ImageDimension; i++)
  {
    if (origin[i] != m_Origin[i])
    {
      break;
    }
  }
  if (i < TOutputImage::ImageDimension)
  {
    for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
      m_Origin[i] = origin[i];
    }
    this->Modified();
  }
}


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::SetSize(const SizeValueType * size)
{
  unsigned int i;
  for (i = 0; i < TOutputImage::ImageDimension; i++)
  {
    if (size[i] != m_Size[i])
    {
      break;
    }
  }
  if (i < TOutputImage::ImageDimension)
  {
    for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
      m_Size[i] = size[i];
    }
    this->Modified();
  }
}


template <typename TOutputImage>
void
SteerableFilterFreqImageSource<TOutputImage>::SetSize(const SizeType size)
{
  unsigned int i;
  for (i = 0; i < TOutputImage::ImageDimension; i++)
  {
    if (size[i] != m_Size[i])
    {
      break;
    }
  }
  if (i < TOutputImage::ImageDimension)
  {
    for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
      m_Size[i] = size[i];
    }
    this->Modified();
  }
}

} // end namespace itk

#endif
