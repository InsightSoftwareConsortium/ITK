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
#ifndef itkVnlComplexToComplexFFTImageFilter_hxx
#define itkVnlComplexToComplexFFTImageFilter_hxx

#include "itkVnlComplexToComplexFFTImageFilter.h"
#include "itkProgressReporter.h"
#include "itkVnlFFTCommon.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageAlgorithm.h"


namespace itk
{

template< typename TImage >
VnlComplexToComplexFFTImageFilter< TImage >
::VnlComplexToComplexFFTImageFilter()
{
}


template <typename TImage>
void
VnlComplexToComplexFFTImageFilter< TImage >
::BeforeThreadedGenerateData()
{
  const ImageType * input = this->GetInput();
  ImageType * output = this->GetOutput();

  const typename ImageType::RegionType bufferedRegion = input->GetBufferedRegion();
  const typename ImageType::SizeType & imageSize = bufferedRegion.GetSize();

  for( unsigned int ii = 0; ii < ImageDimension; ++ii )
    {
    if ( !VnlFFTCommon::IsDimensionSizeLegal( imageSize[ii] ) )
      {
      itkExceptionMacro(<< "Cannot compute FFT of image with size "
                        << imageSize << ". VnlComplexToComplexFFTImageFilter operates "
                        << "only on images whose size in each dimension is a multiple of "
                        << "2, 3, or 5." );
      }
    }

  // Copy the input to the output, and we will work in place on the output.
  ImageAlgorithm::Copy< ImageType, ImageType >( input, output, bufferedRegion, bufferedRegion );

  typedef std::complex< typename PixelType::value_type > VclPixelType;
  VclPixelType * outputBuffer = static_cast< VclPixelType * >( output->GetBufferPointer() );

  // call the proper transform, based on compile type template parameter
  VnlFFTCommon::VnlFFTTransform< Image< typename PixelType::value_type , ImageDimension > > vnlfft( imageSize );
  if ( this->GetTransformDirection() == Superclass::INVERSE )
    {
    vnlfft.transform( outputBuffer, 1 );
    }
  else
    {
    vnlfft.transform( outputBuffer, -1 );
    }
}


template <typename TImage>
void
VnlComplexToComplexFFTImageFilter< TImage >
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType itkNotUsed(threadId) )
{
  //
  // Normalize the output if backward transform
  //
  if ( this->GetTransformDirection() == Superclass::INVERSE )
    {
    typedef ImageRegionIterator< OutputImageType >   IteratorType;
    SizeValueType totalOutputSize = this->GetOutput()->GetRequestedRegion().GetNumberOfPixels();
    IteratorType it(this->GetOutput(), outputRegionForThread);
    while( !it.IsAtEnd() )
      {
      PixelType val = it.Value();
      val /= totalOutputSize;
      it.Set(val);
      ++it;
      }
    }
}

} // end namespace itk

#endif // _itkVnlComplexToComplexFFTImageFilter_hxx
