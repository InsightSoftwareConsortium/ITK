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
#ifndef __itkIsoContourDistanceImageFilter_txx
#define __itkIsoContourDistanceImageFilter_txx

#include "itkIsoContourDistanceImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkNumericTraits.h"
#include "itkIndex.h"

namespace itk
{
/**
 * Default constructor.
 */
template< class TInputImage, class TOutputImage >
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::IsoContourDistanceImageFilter()
{
  m_LevelSetValue = NumericTraits< InputPixelType >::Zero;

  m_FarValue = 10 * NumericTraits< PixelType >::One;

  m_NarrowBanding = false;
  m_NarrowBand = NULL;

  m_Barrier = Barrier::New();
}

/**
 * Set the input narrowband container.
 */
template< class TInputImage, class TOutputImage >
void
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::SetNarrowBand(
  NarrowBandType *ptr)
{
  if ( m_NarrowBand != ptr )
    {
    m_NarrowBand = ptr;
    this->Modified();
    }
}

/**
 * PrintSelf method.
 */
template< class TInputImage, class TOutputImage >
void
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Narrowbanding: " << m_NarrowBanding << std::endl;
  os << indent << "LevelSetValue: " << m_LevelSetValue << std::endl;
  os << indent << "FarValue: " << m_FarValue << std::endl;
  os << std::endl;
}

/**
 * GenerateInputRequestedRegion method.
 */
template< class TInputImage, class TOutputImage >
void
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // use the default implementation.
  this->Superclass::GenerateInputRequestedRegion();
}

/**
 * EnlargeOutputRequestedRegion method.
 */
template< class TInputImage, class TOutputImage >
void
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(
  DataObject *output)
{
  // this filter requires the all of the output image to be in
  // the buffer
  TOutputImage *imgData;

  imgData = dynamic_cast< TOutputImage * >( output );
  if ( imgData )
    {
    imgData->SetRequestedRegionToLargestPossibleRegion();
    }
  else
    {
    // pointer could not be cast to TLevelSet *
    itkWarningMacro( << "itk::IsoContourDistanceImageFilter"
                     << "::EnlargeOutputRequestedRegion cannot cast "
                     << typeid( output ).name() << " to "
                     << typeid( TOutputImage * ).name() );
    }
}

/**
 * Before ThreadedGenerateData:
 *  Split the band if we use narrowband mode
 */
template< class TInputImage, class TOutputImage >
void
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  // Instead of using GetNumberOfThreads, we need to split the image into the
  // number of regions that will actually be returned by
  // itkImageSource::SplitRequestedRegion.  Sometimes this number is less than
  // the number of threads requested.
  typename TOutputImage::RegionType dummy;
  unsigned int actualThreads = this->SplitRequestedRegion(
    0, this->GetNumberOfThreads(),
    dummy);

  // Initialize the barrier for the thread synchronization in
  // the narrowband case.
  this->m_Barrier->Initialize(actualThreads);

  if ( m_NarrowBanding )
    {
    // Split the narrow band into sections, one section for each thread
    this->m_NarrowBandRegion = this->m_NarrowBand->SplitBand(actualThreads);
    }
}

//----------------------------------------------------------------------------
// The execute method created by the subclass.
template< class TInputImage, class TOutputImage >
void
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  typedef typename InputImageType::ConstPointer ImageConstPointer;
  typedef typename OutputImageType::Pointer     OutputPointer;

  ImageConstPointer inputPtr = this->GetInput();
  OutputPointer     outputPtr = this->GetOutput();

  typedef ImageRegionConstIterator< InputImageType > ConstIteratorType;
  typedef ImageRegionIterator< OutputImageType >     IteratorType;
  ConstIteratorType inIt (inputPtr,
                          outputRegionForThread);
  IteratorType outIt (outputPtr,
                      outputRegionForThread);

  //Initialize output image. Thi needs to be done regardless of the
  // NarrowBanding or Full implementation
  while ( !inIt.IsAtEnd() )
    {
    if ( inIt.Get() > m_LevelSetValue )
      {
      outIt.Set(+m_FarValue);
      }
    else if ( inIt.Get() < m_LevelSetValue )
      {
      outIt.Set(-(double)m_FarValue);
      }
    else
      {
      outIt.Set(NumericTraits< PixelType >::Zero);
      }
    ++inIt;
    ++outIt;
    }

  // Wait for all threads to be done initializing output
  this->m_Barrier->Wait();

  //Iterate over split region or split band as convinient.
  if ( m_NarrowBanding == false )
    {
    this->ThreadedGenerateDataFull(outputRegionForThread, threadId);
    }
  else
    {
    this->ThreadedGenerateDataBand(outputRegionForThread, threadId);
    }
}

// The execute method created by the subclass.
template< class TInputImage, class TOutputImage >
void
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateDataFull( const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType itkNotUsed(threadId) )
{
  typedef typename InputImageType::ConstPointer ImageConstPointer;
  typedef typename OutputImageType::Pointer     OutputPointer;

  ImageConstPointer inputPtr = this->GetInput();
  OutputPointer     outputPtr = this->GetOutput();

  unsigned int n, ng;

  InputSizeType radius_in;
  SizeType      radius_out;
  for ( n = 0; n < ImageDimension; n++ )
    {
    radius_in[n] = 2;
    radius_out[n] = 1;
    }

  //Define Neighborhood iterator
  ConstNeighborhoodIterator< InputImageType > inNeigIt(radius_in, inputPtr,
                                                       outputRegionForThread);
  NeighborhoodIterator< OutputImageType > outNeigIt(radius_out, outputPtr,
                                                    outputRegionForThread);
  PixelType val, val0, val1, val0_new, val1_new, diff;
  PixelType norm;
  bool      sign, neigh_sign;

  PixelType grad0[ImageDimension];
  PixelType grad1[ImageDimension];
  PixelType grad[ImageDimension];

  PixelType alpha0 = 0.5;  //Interpolation factor
  PixelType alpha1 = 0.5;  //Interpolation factor
  const typename InputImageType::SpacingType & vs = inputPtr->GetSpacing();
  double vs_2[ImageDimension];

  for ( n = 0; n < ImageDimension; n++ )
    {
    vs_2[n] = 2 * vs[n];
    }

  //Get Stride information to move across dimension
  OffsetValueType stride[ImageDimension];
  unsigned int center;

  for ( n = 0; n < ImageDimension; n++ )
    {
    stride[n] = inNeigIt.GetStride(n);
    }
  center = inNeigIt.Size() / 2;

  for ( inNeigIt.GoToBegin(); !inNeigIt.IsAtEnd(); ++inNeigIt, ++outNeigIt )
    {
    val0 = inNeigIt.GetPixel(center) - static_cast< PixelType >( m_LevelSetValue );
    sign = ( val0 > 0 );

    //Compute gradient at val0
    for ( ng = 0; ng < ImageDimension; ng++ )
      {
      grad0[ng] = static_cast< PixelType >( inNeigIt.GetNext(ng, 1) )
                  - static_cast< PixelType >( inNeigIt.GetPrevious(ng, 1) );
      }

    for ( n = 0; n < ImageDimension; n++ )
      {
      val1 =  static_cast< PixelType >( inNeigIt.GetPixel(center + stride[n]) )
             - static_cast< PixelType >( m_LevelSetValue );

      neigh_sign = ( val1 > 0 );

      if ( sign != neigh_sign )
        {
        for ( ng = 0; ng < ImageDimension; ng++ )
          {
          grad1[ng] = static_cast< PixelType >( inNeigIt.GetPixel(center + stride[n] + stride[ng]) )
                      - static_cast< PixelType >( inNeigIt.GetPixel(center + stride[n] - stride[ng]) );
          }
        if ( sign )
          {
          diff = val0 - val1;
          }
        else
          {
          diff = val1 - val0;
          }
        if ( diff < NumericTraits< PixelType >::min() )
          {
          //do something: printf, or thorw exception. ??
          continue;
          }
        //Interpolate values
        norm = NumericTraits< PixelType >::Zero;
        for ( ng = 0; ng < ImageDimension; ng++ )
          {
          grad[ng] = ( grad0[ng] * alpha0 + grad1[ng] * alpha1 ) / vs_2[ng];
          norm += grad[ng] * grad[ng];
          }
        norm = vcl_sqrt( (float)norm );

        if ( norm > NumericTraits< PixelType >::min() )
          {
          val = vcl_fabs( (float)grad[n] ) * vs[n] / norm / diff;

          val0_new = val0 * val;
          val1_new = val1 * val;

          if ( vcl_fabs( (float)val0_new ) < vcl_fabs( (float)outNeigIt.GetNext(n, 0) ) )
            {
            outNeigIt.SetNext( n, 0, static_cast< PixelType >( val0_new ) );
            }
          if ( vcl_fabs( (float)val1_new ) < vcl_fabs( (float)outNeigIt.GetNext(n, 1) ) )
            {
            outNeigIt.SetNext( n, 1, static_cast< PixelType >( val1_new ) );
            }
          }
        else
          {
          itkExceptionMacro(<< "Gradient norm is lower than pixel precision");
          }
        } // end if (sign != sign_neigh)
      }   //end for n
    }
}

// The execute method created by the subclass.
template< class TInputImage, class TOutputImage >
void
IsoContourDistanceImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateDataBand(const OutputImageRegionType & itkNotUsed(outputRegionForThread),
                           ThreadIdType threadId)
{
  typename InputImageType::ConstPointer inputPtr = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  //Tasks:
  //1. Initialize whole output image (done in ThreadedGenerateData)
  //2. Wait for threads (done in ThreadedGenerateData)
  //3. Computation over the narrowband
  ConstBandIterator bandIt  = m_NarrowBandRegion[threadId].Begin;
  ConstBandIterator bandEnd = m_NarrowBandRegion[threadId].End;
  typedef ImageRegionConstIterator< InputImageType > ConstIteratorType;
  typedef ImageRegionIterator< OutputImageType >     IteratorType;

  unsigned int n, ng;

  InputSizeType radius_in;
  SizeType      radius_out;
  for ( n = 0; n < ImageDimension; n++ )
    {
    //radius_in[n]= 2*NumericTraits<InputSizeType>::One();
    radius_in[n] = 2;
    radius_out[n] = 1;
    //radius_out[n]= NumericTraits<SizeType>::One();
    }

  //Create neighborhood iterator
  ConstNeighborhoodIterator< InputImageType > inNeigIt( radius_in, inputPtr,
                                                        inputPtr->GetRequestedRegion() );
  NeighborhoodIterator< OutputImageType > outNeigIt( radius_out, outputPtr,
                                                     outputPtr->GetRequestedRegion() );
  PixelType val, val0, val1, val0_new, val1_new, diff;
  PixelType norm;
  bool      sign, neigh_sign;

  PixelType grad0[ImageDimension];
  PixelType grad1[ImageDimension];
  PixelType grad[ImageDimension];

  PixelType alpha0 = 0.5;  //Interpolation factor
  PixelType alpha1 = 0.5;  //Interpolation factor
  const typename InputImageType::SpacingType & vs = inputPtr->GetSpacing();
  double vs_2[ImageDimension];

  for ( n = 0; n < ImageDimension; n++ )
    {
    vs_2[n] = 2 * vs[n];
    }
  //Get Stride information to move across dimension
  OffsetValueType stride[ImageDimension];
  unsigned int center;

  for ( n = 0; n < ImageDimension; n++ )
    {
    stride[n] = inNeigIt.GetStride(n);
    }
  center = inNeigIt.Size() / 2;

  for (; bandIt != bandEnd; bandIt++ )
    {
    inNeigIt.SetLocation(bandIt->m_Index);
    outNeigIt.SetLocation(bandIt->m_Index);

    val0 = inNeigIt.GetPixel(center) - m_LevelSetValue;
    sign = ( val0 > 0 );

    //Compute gradient at val0
    for ( ng = 0; ng < ImageDimension; ng++ )
      {
      grad0[ng] = inNeigIt.GetNext(ng, 1) - inNeigIt.GetPrevious(ng, 1);
      }
    //Compute gradient at val0
    for ( ng = 0; ng < ImageDimension; ng++ )
      {
      grad0[ng] = inNeigIt.GetNext(ng, 1) - inNeigIt.GetPrevious(ng, 1);
      }

    for ( n = 0; n < ImageDimension; n++ )
      {
      val1 = inNeigIt.GetPixel(center + stride[n]) - m_LevelSetValue;

      neigh_sign = ( val1 > 0 );

      if ( sign != neigh_sign )
        {
        for ( ng = 0; ng < ImageDimension; ng++ )
          {
          grad1[ng] = inNeigIt.GetPixel(center + stride[n] + stride[ng])
                      - inNeigIt.GetPixel(center + stride[n] - stride[ng]);
          }
        if ( sign )
          {
          diff = val0 - val1;
          }
        else
          {
          diff = val1 - val0;
          }
        if ( diff < NumericTraits< PixelType >::min() )
          {
          //do something: printf, or thorw exception.??
          continue;
          }
        //Interpolate values
        norm = NumericTraits< PixelType >::Zero;
        for ( ng = 0; ng < ImageDimension; ng++ )
          {
          grad[ng] = ( grad0[ng] * alpha0 + grad1[ng] * alpha1 ) / vs_2[ng];
          norm += grad[ng] * grad[ng];
          }
        norm = vcl_sqrt( (float)norm );

        if ( norm > NumericTraits< PixelType >::min() )
          {
          val = vcl_fabs( (float)grad[n] ) * vs[n] / norm / diff;

          val0_new = val0 * val;
          val1_new = val1 * val;

          if ( vcl_fabs( (float)val0_new ) < vcl_fabs( (float)outNeigIt.GetNext(n, 0) ) )
            {
            outNeigIt.SetNext( n, 0, static_cast< PixelType >( val0_new ) );
            }
          if ( vcl_fabs( (float)val1_new ) < vcl_fabs( (float)outNeigIt.GetNext(n, 1) ) )
            {
            outNeigIt.SetNext( n, 1, static_cast< PixelType >( val1_new ) );
            }
          }
        else
          {
          itkExceptionMacro(<< "Gradient norm is lower than pixel precision");
          }
        } // end if (sign != sign_neigh)
      }   //end for n
    }     //Band iteratior
}
} // namespace itk

#endif
