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
#ifndef __itkSignedMaurerDistanceMapImageFilter_txx
#define __itkSignedMaurerDistanceMapImageFilter_txx

#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkProgressReporter.h"
#include "itkProgressAccumulator.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_math.h"

//Simple functor to invert an image for Outside Danielsson distance map
namespace itk
{
namespace Functor
{
template< class InputPixelType >
class InvertBinaryIntensityFunctor
{
public:
  InputPixelType operator()(InputPixelType input) const
  {
    if ( input )
      {
      return NumericTraits< InputPixelType >::Zero;
      }
    else
      {
      return NumericTraits< InputPixelType >::One;
      }
  }
};
}
}

namespace itk
{
template< class TInputImage, class TOutputImage >
SignedMaurerDistanceMapImageFilter< TInputImage, TOutputImage >
::SignedMaurerDistanceMapImageFilter():m_BackgroundValue(0),
  m_InsideIsPositive(false),
  m_UseImageSpacing(false),
  m_SquaredDistance(true)
{}

template< class TInputImage, class TOutputImage >
SignedMaurerDistanceMapImageFilter< TInputImage, TOutputImage >
::~SignedMaurerDistanceMapImageFilter()
{}

template< class TInputImage, class TOutputImage >
int
SignedMaurerDistanceMapImageFilter< TInputImage, TOutputImage >
::SplitRequestedRegion(int i, int num, OutputImageRegionType & splitRegion)
{
  // Get the output pointer
  OutputImageType *outputPtr = this->GetOutput();

  const typename TOutputImage::SizeType & requestedRegionSize =
    outputPtr->GetRequestedRegion().GetSize();

  int splitAxis;
  typename TOutputImage::IndexType splitIndex;
  typename TOutputImage::SizeType splitSize;

  // Initialize the splitRegion to the output requested region
  splitRegion = outputPtr->GetRequestedRegion();
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  // split on the outermost dimension available
  // and avoid the current dimension
  splitAxis = outputPtr->GetImageDimension() - 1;
  while ( requestedRegionSize[splitAxis] == 1 || splitAxis == (int)m_CurrentDimension )
    {
    --splitAxis;
    if ( splitAxis < 0 )
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return 1;
      }
    }

  // determine the actual number of pieces that will be generated
  typename TOutputImage::SizeType::SizeValueType range = requestedRegionSize[splitAxis];
  int valuesPerThread = (int)vcl_ceil(range / (double)num);
  int maxThreadIdUsed = (int)vcl_ceil(range / (double)valuesPerThread) - 1;

  // Split the region
  if ( i < maxThreadIdUsed )
    {
    splitIndex[splitAxis] += i * valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
    }
  if ( i == maxThreadIdUsed )
    {
    splitIndex[splitAxis] += i * valuesPerThread;
    // last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i * valuesPerThread;
    }

  // set the split region ivars
  splitRegion.SetIndex(splitIndex);
  splitRegion.SetSize(splitSize);

  itkDebugMacro("  Split Piece: " << splitRegion);

  return maxThreadIdUsed + 1;
}

template< class TInputImage, class TOutputImage >
void
SignedMaurerDistanceMapImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // prepare the data
  this->AllocateOutputs();
  this->m_Spacing = this->GetOutput()->GetSpacing();

  // store the binary image in an image with a pixel type as small as possible
  // instead of keeping the native input pixel type to avoid using too much
  // memory.
  typedef Image< unsigned char, InputImageDimension > BinaryImageType;

  typedef BinaryThresholdImageFilter< InputImageType,
                                      BinaryImageType >  BinaryFilterType;

  ProgressAccumulator::Pointer progressAcc = ProgressAccumulator::New();
  progressAcc->SetMiniPipelineFilter(this);

  // compute the boundary of the binary object.
  // To do that, we erode the binary object. The eroded pixels are the ones
  // on the boundary. We mark them with the value 2
  typename BinaryFilterType::Pointer binaryFilter = BinaryFilterType::New();

  binaryFilter->SetLowerThreshold(this->m_BackgroundValue);
  binaryFilter->SetUpperThreshold(this->m_BackgroundValue);
  binaryFilter->SetInsideValue(0);
  binaryFilter->SetOutsideValue(1);
  binaryFilter->SetInput( this->GetInput() );
  binaryFilter->SetNumberOfThreads( this->GetNumberOfThreads() );
//   progressAcc->RegisterInternalFilter( binaryFilter, 0.1f );
  binaryFilter->Update();

  // Dilate the inverted image by 1 pixel to give it the same boundary
  // as the univerted this->GetInput().
  // This part is not threaded yet, and should be replaced by a contour
  // detector like in http://voxel.jouy.inra.fr/darcs/contrib-itk/watershed/
  // itkSignedMaurerDistanceMapImageFilter.txx

  typedef BinaryBallStructuringElement<
    unsigned char,
    InputImageDimension  > StructuringElementType;

  typedef BinaryErodeImageFilter<
    BinaryImageType,
    BinaryImageType,
    StructuringElementType >     ErodeType;

  typename ErodeType::Pointer erode = ErodeType::New();

  StructuringElementType structuringElement;
  structuringElement.SetRadius(1);
  structuringElement.CreateStructuringElement();
  erode->SetKernel(structuringElement);
  erode->SetForegroundValue(1);
  erode->SetBackgroundValue(2);
  erode->SetInput( binaryFilter->GetOutput() );
  progressAcc->RegisterInternalFilter(erode, 0.33f);
  erode->Update();

  typedef ImageRegionConstIterator< BinaryImageType > InputIterator;

  InputIterator inIterator( erode->GetOutput(),
                            erode->GetOutput()->GetRequestedRegion() );

  typedef ImageRegionIterator< OutputImageType > OutputIterator;

  OutputIterator outIterator( this->GetOutput(),
                              this->GetOutput()->GetRequestedRegion() );

  for (  inIterator.GoToBegin(), outIterator.GoToBegin();
         !inIterator.IsAtEnd();
         ++inIterator, ++outIterator )
    {
    if ( inIterator.Get() == 2 )
      {
      outIterator.Set(NumericTraits< OutputPixelType >::Zero);
      }
    else
      {
      outIterator.Set( NumericTraits< OutputPixelType >::max() );
      }
    }

  // Set up the multithreaded processing
  typename ImageSource< TOutputImage >::ThreadStruct str;
  str.Filter = this;

  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->ThreaderCallback, &str);

  // multithread the execution
  for ( unsigned int d = 0; d < ImageDimension; d++ )
    {
    m_CurrentDimension = d;
    this->GetMultiThreader()->SingleMethodExecute();
    }
}

template< class TInputImage, class TOutputImage >
void
SignedMaurerDistanceMapImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId)
{
  vnl_vector< unsigned int > k(InputImageDimension - 1);

  typedef typename InputImageType::RegionType InputRegionType;

  InputRegionType region = outputRegionForThread;
  InputSizeType   size   = region.GetSize();
  typename InputImageType::RegionType::IndexType startIndex;
  startIndex = outputRegionForThread.GetIndex();

  // compute the number of rows first, so we can setup a progress reporter
  typename std::vector< unsigned int > NumberOfRows;

  for ( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    NumberOfRows.push_back(1);
    for ( unsigned int d = 0; d < InputImageDimension; d++ )
      {
      if ( d != i )
        {
        NumberOfRows[i] *= size[d];
        }
      }
    }

  // set the progress reporter. Use a pointer to be able to destroy it before
  // the creation of progress2
  // so it won't set wrong progress at the end of ThreadedGenerateData()
  float progressPerDimension = 0.67f / ImageDimension;
  if ( !this->m_SquaredDistance )
    {
    progressPerDimension = 0.67f / ( ImageDimension + 1 );
    }
  ProgressReporter *progress = new ProgressReporter(this,
                                                    threadId,
                                                    NumberOfRows[m_CurrentDimension],
                                                    30,
                                                    0.33f + m_CurrentDimension * progressPerDimension,
                                                    progressPerDimension);

  OutputIndexType idx;
  idx.Fill(0);

  k[0] = 1;
  unsigned int count = 1;

  for ( unsigned int d = m_CurrentDimension + 2; d < m_CurrentDimension + InputImageDimension; d++ )
    {
    k[count] = k[count - 1] * size[d % InputImageDimension];
    count++;
    }
  k.flip();

  unsigned int index;
  for ( unsigned int n = 0; n < NumberOfRows[m_CurrentDimension]; n++ )
    {
    index = n;
    count = 0;
    for ( unsigned int d = m_CurrentDimension + 1; d < m_CurrentDimension + InputImageDimension; d++ )
      {
      idx[d % InputImageDimension] =
        static_cast< unsigned int >(
          static_cast< double >( index )
          / static_cast< double >( k[count] ) )
        + startIndex[d % InputImageDimension];

      index %= k[count];
      count++;
      }
    this->Voronoi(m_CurrentDimension, idx);
    progress->CompletedPixel();
    }
  delete progress;

  if ( m_CurrentDimension == ImageDimension - 1 && !this->m_SquaredDistance )
    {
    typedef ImageRegionIterator< OutputImageType >      OutputIterator;
    typedef ImageRegionConstIterator< InputImageType  > InputIterator;

    typename OutputImageType::RegionType outputRegion = outputRegionForThread;

    OutputIterator Ot(this->GetOutput(), outputRegion);
    InputIterator  It(this->GetInput(),  outputRegion);

    Ot.GoToBegin();
    It.GoToBegin();

    ProgressReporter progress2(this, threadId,
                               outputRegionForThread.GetNumberOfPixels(), 30, 0.33f + ImageDimension
                               * progressPerDimension, progressPerDimension);
    while ( !Ot.IsAtEnd() )
      {
      // cast to a real type is required on some platforms
      // TODO: use "typename NumericTraits<OutputPixelType>::RealType" instead
      // double. cableswig currently fail to build it with msvc 7.1
      const OutputPixelType outputValue =
        static_cast< OutputPixelType >(
          vcl_sqrt( static_cast< double >( vnl_math_abs( Ot.Get() ) ) ) );

      if ( It.Get() != this->m_BackgroundValue )
        {
        if ( this->GetInsideIsPositive() )
          {
          Ot.Set(outputValue);
          }
        else
          {
          Ot.Set(-outputValue);
          }
        }
      else
        {
        if ( this->GetInsideIsPositive() )
          {
          Ot.Set(-outputValue);
          }
        else
          {
          Ot.Set(outputValue);
          }
        }

      ++Ot;
      ++It;
      progress2.CompletedPixel();
      }
    }
}

template< class TInputImage, class TOutputImage >
void
SignedMaurerDistanceMapImageFilter< TInputImage, TOutputImage >
::Voronoi(unsigned int d, OutputIndexType idx)
{
  typename OutputImageType::Pointer output( this->GetOutput() );
  unsigned int                      nd = output->GetRequestedRegion().GetSize()[d];

  vnl_vector< OutputPixelType > g(nd);  g = 0;
  vnl_vector< OutputPixelType > h(nd);  h = 0;

  typename InputImageType::RegionType::IndexType startIndex;
  startIndex = this->GetInput()->GetRequestedRegion().GetIndex();

  OutputPixelType di;

  int l = -1;

  for ( unsigned int i = 0; i < nd; i++ )
    {
    idx[d] = i + startIndex[d];

    di = output->GetPixel(idx);

    OutputPixelType iw;

    if ( this->GetUseImageSpacing() )
      {
      iw = static_cast< OutputPixelType >( i * this->m_Spacing[d] );
      }
    else
      {
      iw  = static_cast< OutputPixelType >( i );
      }

    if ( di != NumericTraits< OutputPixelType >::max() )
      {
      if ( l < 1 )
        {
        l++;
        g(l) = di;
        h(l) = iw;
        }
      else
        {
        while ( ( l >= 1 )
                && this->Remove(g(l - 1), g(l), di, h(l - 1), h(l), iw) )
          {
          l--;
          }
        l++;
        g(l) = di;
        h(l) = iw;
        }
      }
    }

  if ( l == -1 )
    {
    return;
    }

  int ns = l;

  l = 0;

  for ( unsigned int i = 0; i < nd; i++ )
    {
    OutputPixelType iw;

    if ( this->GetUseImageSpacing() )
      {
      iw = static_cast< OutputPixelType >( i * this->m_Spacing[d] );
      }
    else
      {
      iw = static_cast< OutputPixelType >( i );
      }

    OutputPixelType d1 = vnl_math_abs( g(l) ) + ( h(l) - iw ) * ( h(l) - iw );

    while ( l < ns )
      {
      // be sure to compute d2 *only* if l < ns
      OutputPixelType d2 = vnl_math_abs( g(l + 1) ) + ( h(l + 1) - iw ) * ( h(l + 1) - iw );
      // then compare d1 and d2
      if ( d1 <= d2 )
        {
        break;
        }
      l++;
      d1 = d2;
      }
    idx[d] = i + startIndex[d];

    if ( this->GetInput()->GetPixel(idx) != this->m_BackgroundValue )
      {
      if ( this->m_InsideIsPositive )
        {
        output->SetPixel(idx,  d1);
        }
      else
        {
        output->SetPixel(idx, -d1);
        }
      }
    else
      {
      if ( this->m_InsideIsPositive )
        {
        output->SetPixel(idx, -d1);
        }
      else
        {
        output->SetPixel(idx,  d1);
        }
      }
    }
}

template< class TInputImage, class TOutputImage >
bool
SignedMaurerDistanceMapImageFilter< TInputImage, TOutputImage >
::Remove(OutputPixelType d1, OutputPixelType d2, OutputPixelType df,
         OutputPixelType x1, OutputPixelType x2, OutputPixelType xf)
{
  OutputPixelType a = x2 - x1;
  OutputPixelType b = xf - x2;
  OutputPixelType c = xf - x1;

  return ( (   c * vnl_math_abs(d2) - b * vnl_math_abs(d1)
               - a * vnl_math_abs(df) - a * b * c ) > 0 );
}

/**
 * Standard "PrintSelf" method
 */
template< class TInputImage, class TOutputImage >
void
SignedMaurerDistanceMapImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Background Value: "
     << this->m_BackgroundValue << std::endl;
  os << indent << "Spacing: "
     << this->m_Spacing << std::endl;
  os << indent << "Inside is positive: "
     << this->m_InsideIsPositive << std::endl;
  os << indent << "Use image spacing: "
     << this->m_UseImageSpacing << std::endl;
  os << indent << "Squared distance: "
     << this->m_SquaredDistance << std::endl;
}
} // end namespace itk

#endif
