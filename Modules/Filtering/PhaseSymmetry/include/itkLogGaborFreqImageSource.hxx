
#ifndef __itkLogGaborFreqImageSource_hxx
#define __itkLogGaborFreqImageSource_hxx

#include "itkLogGaborFreqImageSource.h"
#include "itkImageRegionIteratorWithIndex.h"


namespace itk
{

template <class TOutputImage>
LogGaborFreqImageSource<TOutputImage>::LogGaborFreqImageSource()
{
  // Initial image is 64 wide in each direction.
  for (unsigned int i = 0; i < TOutputImage::GetImageDimension(); i++)
  {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
  }
  m_Direction.SetIdentity();
  // Gaussian parameters, defined so that the gaussian
  // is centered in the default image
  m_Sigma = 1.0;
  m_Wavelengths.Fill(2.0);
}


template <class TOutputImage>
LogGaborFreqImageSource<TOutputImage>::~LogGaborFreqImageSource()
{}


template <class TOutputImage>
void
LogGaborFreqImageSource<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


template <typename TOutputImage>
void
LogGaborFreqImageSource<TOutputImage>::GenerateOutputInformation()
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
LogGaborFreqImageSource<TOutputImage>::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                                                            ThreadIdType                  tid)
{
  // The a pointer to the output image
  typename TOutputImage::Pointer outputPtr = this->GetOutput();
  // outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  // outputPtr->Allocate();

  typedef ImageRegionIteratorWithIndex<TOutputImage> OutputIteratorType;
  OutputIteratorType                                 outIt = OutputIteratorType(outputPtr, outputRegionForThread);

  int ndims = TOutputImage::ImageDimension;

  DoubleArrayType centerPoint;
  for (int i = 0; i < ndims; i++)
  {
    centerPoint[i] = double(m_Size[i]) / 2.0;
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
      dist[i] = (double(index[i]) - centerPoint[i]) / double(m_Size[i]);
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

template <typename TOutputImage>
void
LogGaborFreqImageSource<TOutputImage>::SetSpacing(const float * spacing)
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
LogGaborFreqImageSource<TOutputImage>::SetSpacing(const double * spacing)
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
LogGaborFreqImageSource<TOutputImage>::SetOrigin(const float * origin)
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
LogGaborFreqImageSource<TOutputImage>::SetOrigin(const double * origin)
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
LogGaborFreqImageSource<TOutputImage>::SetSize(const SizeValueType * size)
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
LogGaborFreqImageSource<TOutputImage>::SetSize(const SizeType size)
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
