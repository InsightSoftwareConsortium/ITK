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
#ifndef itkRealFFTTest_h
#define itkRealFFTTest_h

/* This test is built for filters specialized for real-to-complex
 * forward and complex-to-real inverse Fast Fourier Transforms using
 * VNL and FFTW FFT libraries. */
#include "itkConfigure.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
#include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#endif

#include "itksys/SystemTools.hxx"
#include "vnl/vnl_sample.h"
#include <cmath>

/* test_fft is the test function and it is templated over the pixel, Image
 * dimensions and the FFT library to be used. */
template< typename TPixel, unsigned int VImageDimensions,
          typename R2CType, typename C2RType >
int
test_fft(unsigned int *SizeOfDimensions)
{
  typedef itk::Image< TPixel, VImageDimensions >                  RealImageType;
  typedef itk::Image< std::complex< TPixel >, VImageDimensions >  ComplexImageType;
  unsigned int counter = 0;
  typename RealImageType::SizeType  imageSize;
  typename RealImageType::IndexType imageIndex;

  // We are testing the FFT for 1D, 2D, and 3D images. An array
  // (SizeOfDimensions) containing the sizes of each dimension is
  // passed as an argument to this function. Based on the template
  // argument VImageDimensions, we create a 1D, 2D, or 3D image by
  // selecting the sizes of image dimensions from this array.
  for (unsigned int i = 0; i < VImageDimensions; i++)
    {
    imageSize.SetElement( i, SizeOfDimensions[i] );
    imageIndex.SetElement( i, -2*static_cast<int>(i) - 5 ); // Test for handling non-zero
                                          // image indices correctly
    }

  typename RealImageType::RegionType region;
  region.SetSize( imageSize );
  region.SetIndex( imageIndex );

  // Create the Real Image.
  typename RealImageType::Pointer realImage = RealImageType::New();
  realImage->SetLargestPossibleRegion( region );
  realImage->SetBufferedRegion( region );
  realImage->SetRequestedRegion( region );
  realImage->Allocate();
  vnl_sample_reseed( static_cast< int >( 123456 ) );

  // We use 2 region iterators for this test: the original image
  // iterator and another iterator for the resultant image after
  // performing FFT and IFFT.
  itk::ImageRegionIterator< RealImageType > originalImageIterator( realImage, region );

  // Allocate random pixel values to the image by iterating through it
  // and print out the image data.
  try
    {
    std::cerr << "---- Original image ----" << std::endl;
    while( !originalImageIterator.IsAtEnd() )
      {
      TPixel val = vnl_sample_uniform( 0.0, 16384.0 );
      if ( (counter + 1 ) % SizeOfDimensions[0] == 0 )
        {
        std::cout << val << std::endl;
        }
      else
        {
        std::cout << val << " ";
        }
      counter++;
      originalImageIterator.Set( val );
      ++originalImageIterator;
      }
    std::cout << std::endl << std::endl;
    }
  catch ( itk::ExceptionObject & ex )
    {
    ex.Print( std::cerr );
    return -1;
    }

  // Real to complex pointer. This computes the forward FFT.
  typename R2CType::Pointer R2C = R2CType::New();

  // Complex to Real pointer. This computes the HalfHermitianToRealInverse FFT.
  typename C2RType::Pointer C2R = C2RType::New();

  // Set the real image created as the input to the forward FFT
  // filter.
  R2C->SetInput( realImage );
  R2C->Print( std::cout );

  try
    {
    R2C->Update();
    }
  catch ( itk::ExceptionObject & ex )
    {
    ex.Print( std::cerr );
    return -1;
    }

  // Get the size and the pointer to the complex image.
  typename ComplexImageType::Pointer complexImage = R2C->GetOutput();
  std::complex< TPixel > *fftbuf = complexImage->GetBufferPointer();
  const typename ComplexImageType::SizeType & complexImageSize =
    complexImage->GetLargestPossibleRegion().GetSize();

  unsigned int sizes[4] = { 1,1,1,1 };
  for( unsigned int i = 0; i < VImageDimensions; i++)
    {
    sizes[i] = complexImageSize[i];
    }

  /* Print out the the frequency domain data obtained after performing
   * the forward transform. */
  std::cout << "Frequency domain data after forward transform:" << std::endl;
  for( unsigned int i = 0; i < sizes[2]; i++)
    {
    unsigned int zStride = i * sizes[1] * sizes[0];
    for (unsigned int j = 0; j < sizes[1]; j++)
      {
      unsigned int yStride = j * sizes[0];
      for (unsigned int k = 0; k < sizes[0]; k++)
        {
        std::cout << fftbuf[zStride+yStride+k] << " ";
        }
      std::cout << std::endl;
      }
    }

  std::cout << std::endl << std::endl;

  // Perform the HalfHermitianToRealInverse FFT to get back the Real Image. C2R is the
  // complex conjugate to real image filter and we give the resulting
  // complex image as input to this filter. This is the HalfHermitianToRealInverse FFT of
  // the image.
  C2R->SetInput( complexImage );

  // Inform the filter that there's an odd # of pixels in the x
  // dimension.
  C2R->SetActualXDimensionIsOddInput( R2C->GetActualXDimensionIsOddOutput() );
  C2R->Print( std::cout );
  C2R->Update();
  std::cerr << "C2R region: " << C2R->GetOutput()->GetLargestPossibleRegion() << std::endl;
  typename RealImageType::Pointer imageAfterHalfHermitianToRealInverseFFT = C2R->GetOutput();

  // The HalfHermitianToRealInverse FFT image iterator is the resultant iterator after we
  // perform the FFT and HalfHermitianToRealInverse FFT on the Original Image. */
  itk::ImageRegionIterator< RealImageType > inverseFFTImageIterator( imageAfterHalfHermitianToRealInverseFFT,
                                                                     region );
  counter = 0;
  inverseFFTImageIterator.GoToBegin();

  // Print the Image data obtained by performing the HalfHermitianToRealInverse FFT.
  std::cerr << "---- HalfHermitianToRealInverse FFT image ----" << std::endl;
  while ( !inverseFFTImageIterator.IsAtEnd() )
    {
    TPixel val = inverseFFTImageIterator.Value();
    if ( (counter + 1 ) % SizeOfDimensions[0] == 0 )
      {
      std::cerr << val << std::endl;
      }
    else
      {
      std::cerr << val << " ";
      }
    counter++;
    ++inverseFFTImageIterator;
    }

  std::cerr << std::endl << std::endl;

  // Subtract the Original image Pixel Values from the resultant image
  // values and test whether they are greater than 0.01 for the test
  // to pass.
  originalImageIterator.GoToBegin();
  inverseFFTImageIterator.GoToBegin();
  while ( !originalImageIterator.IsAtEnd() )
    {
    TPixel val = originalImageIterator.Value();
    TPixel val2 = inverseFFTImageIterator.Value();
    TPixel diff = itk::Math::abs( val - val2 );
    if ( itk::Math::NotAlmostEquals(val, itk::NumericTraits<TPixel>::ZeroValue()) )
      {
      diff /= itk::Math::abs( val );
      }
    if ( diff > 0.01 )
      {
      std::cerr << "Diff found in test_fft: " << val << " " << val2 << " diff " << diff
                << std::endl;
      return -1;
      }
    ++originalImageIterator;
    ++inverseFFTImageIterator;
    }
  std::cout << std::endl << std::endl;

  return 0;
}


/* test_fft_rtc is the test function to compare two implementations
 * (Direct FFT only).  It is templated over the pixel, Image
 * dimensions and the FFT libraries to be used. */
template< typename TPixel, unsigned int VImageDimensions,
          typename R2CAType, typename R2CBType >
int
test_fft_rtc(unsigned int *SizeOfDimensions)
{
  typedef itk::Image< TPixel, VImageDimensions >                  RealImageType;
  typedef itk::Image< std::complex< TPixel >, VImageDimensions >  ComplexImageType;
  unsigned int counter = 0;
  typename RealImageType::SizeType  imageSize;
  typename RealImageType::IndexType imageIndex;

  // We are testing the FFT for 1D, 2D, and 3D images. An array
  // (SizeOfDimensions) containing the sizes of each dimension is
  // passed as an argument to this function. Based on the template
  // argument VImageDimensions, we create a 1D, 2D, or 3D image by
  // selecting the sizes of image dimensions from this array.
  for (unsigned int i = 0; i < VImageDimensions; i++)
    {
    imageSize.SetElement( i, SizeOfDimensions[i] );
    imageIndex.SetElement( i, 0 );
    }

  typename RealImageType::RegionType region;
  region.SetSize( imageSize );
  region.SetIndex( imageIndex );
  typename RealImageType::Pointer realImage = RealImageType::New();

  // Create the Real Image.
  realImage->SetLargestPossibleRegion( region );
  realImage->SetBufferedRegion( region );
  realImage->SetRequestedRegion( region );
  realImage->Allocate();
  vnl_sample_reseed( static_cast<int >( itksys::SystemTools::GetTime() / 10000.0 ) );

  // We use 2 region iterators for this test the original image
  // iterator and another iterator for the resultant image after
  // performing FFT and IFFT.
  itk::ImageRegionIterator< RealImageType > originalImageIterator( realImage, region );

  // Allocate random pixel values to the image by iterating through it
  // and Print out the image data.
  try
    {
    while( !originalImageIterator.IsAtEnd() )
      {
      TPixel val = vnl_sample_uniform( 0.0, 16384.0 );
      if ( (counter + 1 ) % SizeOfDimensions[0] == 0 )
        {
        std::cout << val << std::endl;
        }
      else
        {
        std::cout << val << " ";
        }
      counter++;
      originalImageIterator.Set( val );
      ++originalImageIterator;
      }
    std::cout << std::endl << std::endl;
    }
  catch( itk::ExceptionObject & ex )
    {
    ex.Print( std::cerr );
    return -1;
    }

  // Real to complex pointers. This computes the forward FFT.
  typename R2CAType::Pointer R2Ca = R2CAType::New();

  // Real to complex pointers. This computes the forward FFT.
  typename R2CBType::Pointer R2Cb = R2CBType::New();

  // Set the real image created as the input to the forward FFT
  // filter.
  R2Ca->SetInput( realImage );
  R2Ca->Update();

  R2Cb->SetInput( realImage );
  R2Cb->Update();

  // Get the size and the pointer to the complex image.
  typename ComplexImageType::Pointer complexImageA = R2Ca->GetOutput();
  std::complex< TPixel > *fftbufA = complexImageA->GetBufferPointer();
  const typename ComplexImageType::SizeType & complexImageSizeA =
    complexImageA->GetLargestPossibleRegion().GetSize();

  typename ComplexImageType::Pointer complexImageB = R2Cb->GetOutput();
  std::complex< TPixel > *fftbufB = complexImageB->GetBufferPointer();
  const typename ComplexImageType::SizeType & complexImageSizeB =
    complexImageB->GetLargestPossibleRegion().GetSize();


  unsigned int sizesA[4] = { 1,1,1,1 };
  unsigned int sizesB[4] = { 1,1,1,1 };
  for(unsigned int i = 0; i < VImageDimensions; i++)
    {
    // The size may be different if one implementation returns a
    // full matrix but not the other.
    sizesA[i] = complexImageSizeA[i];
    sizesB[i] = complexImageSizeB[i];
    }

  // Print out the the frequency domain data obtained after performing
  // the forward transform.
  std::cout << "Frequency domain data after forward transform:" << std::endl;
  for (unsigned int i = 0; i < sizesA[2]; i++)
    {
    unsigned int zStride = i * sizesA[1] * sizesA[0];
    for (unsigned int j = 0; j < sizesA[1]; j++)
      {
      unsigned int yStride = j * sizesA[0];
      for (unsigned int k = 0; k < sizesA[0]; k++)
        {
        std::cout << fftbufA[zStride+yStride+k] << " ";
        }
      std::cout << std::endl;
      }
    }
  std::cout << std::endl << std::endl;

  for (unsigned int i = 0; i < sizesB[2]; i++)
    {
    unsigned int zStride = i * sizesB[1] * sizesB[0];
    for (unsigned int j = 0; j < sizesB[1]; j++)
      {
      unsigned int yStride = j * sizesB[0];
      for (unsigned int k = 0; k < sizesB[0]; k++)
        {
        std::cout << fftbufB[zStride+yStride+k] << " ";
        }
      std::cout << std::endl;
      }
    }
  std::cout << std::endl << std::endl;


  // Subtract the pixel values from the two images. If one pixel
  // difference is greater than 0.01, the test is considered to have
  // failed.
  for (unsigned int i = 0; i < std::min( sizesA[2], sizesB[2] ); i++)
    {
    unsigned int zStrideA = i * sizesA[1] * sizesA[0];
    unsigned int zStrideB = i * sizesB[1] * sizesB[0];
    for (unsigned int j = 0; j < std::min( sizesA[1], sizesB[1] ); j++)
      {
      unsigned int yStrideA = j * sizesA[0];
      unsigned int yStrideB = j * sizesB[0];
      for (unsigned int k = 0; k < std::min( sizesA[0], sizesB[0] ); k++)
        {
        double val = std::abs(fftbufA[zStrideA+yStrideA+k]);
        double diff = std::abs(fftbufA[zStrideA+yStrideA+k] - fftbufB[zStrideB+yStrideB+k]);
        if ( itk::Math::NotAlmostEquals(val, 0.0) )
          {
          diff /= itk::Math::abs( val );
          }
        if ( diff > 0.01 )
          {
          std::cerr << "Diff found in test_fft_r2c: " << fftbufA[zStrideA+yStrideA+k]
                    << " " << fftbufB[zStrideB+yStrideB+k] << " diff " << diff << std::endl;
          return -1;
          }
        }
      }
    }

  std::cout << std::endl << std::endl;

  return 0;
}
#endif
