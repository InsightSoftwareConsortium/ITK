
#ifndef __itkButterworthFilterFreqImageSource_hxx
#define __itkButterworthFilterFreqImageSource_hxx

#include "itkButterworthFilterFreqImageSource.h"
#include "itkImageRegionIteratorWithIndex.h"


namespace itk
{

template <class TOutputImage>
ButterworthFilterFreqImageSource<TOutputImage>::ButterworthFilterFreqImageSource()
{
  // Initial image is 64 wide in each direction.
  for (unsigned int i = 0; i < TOutputImage::GetImageDimension(); i++)
  {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
  }
  m_Direction.SetIdentity();
}


template <class TOutputImage>
ButterworthFilterFreqImageSource<TOutputImage>::~ButterworthFilterFreqImageSource()
{}


template <class TOutputImage>
void
ButterworthFilterFreqImageSource<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


template <typename TOutputImage>
void
ButterworthFilterFreqImageSource<TOutputImage>::GenerateOutputInformation()
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
ButterworthFilterFreqImageSource<TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  itkNotUsed(threadId))
{
  TOutputImage *                                     outputPtr = this->GetOutput();
  typedef ImageRegionIteratorWithIndex<TOutputImage> OutputIterator;
  OutputIterator                                     outIt = OutputIterator(outputPtr, outputRegionForThread);

  int ndims = TOutputImage::ImageDimension;

  double Value = 0;

  DoubleArrayType centerPoint;
  for (int i = 0; i < ndims; i++)
  {
    centerPoint[i] = double(m_Size[i]) / 2.0;
  }

  double                           radius = 0;
  DoubleArrayType                  dist;
  typename TOutputImage::IndexType index;
  for (outIt.GoToBegin(); !outIt.IsAtEnd(); ++outIt)
  {
    index = outIt.GetIndex();

    radius = 0;
    Value = 0;

    for (int i = 0; i < TOutputImage::ImageDimension; i++)
    {
      dist[i] = (centerPoint[i] - double(index[i])) / double(m_Size[i]);
      radius = radius + dist[i] * dist[i];
    }
    radius = sqrt(radius);
    Value = radius / m_Cutoff;
    Value = pow(Value, 2 * m_Order);
    Value = 1 / (1 + Value);

    // Set the pixel value to the function value
    outIt.Set((typename TOutputImage::PixelType)Value);
  }
}


template <typename TOutputImage>
void
ButterworthFilterFreqImageSource<TOutputImage>::SetSpacing(const float * spacing)
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
ButterworthFilterFreqImageSource<TOutputImage>::SetSpacing(const double * spacing)
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
ButterworthFilterFreqImageSource<TOutputImage>::SetOrigin(const float * origin)
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
ButterworthFilterFreqImageSource<TOutputImage>::SetOrigin(const double * origin)
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
ButterworthFilterFreqImageSource<TOutputImage>::SetSize(const SizeValueType * size)
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
ButterworthFilterFreqImageSource<TOutputImage>::SetSize(const SizeType size)
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
