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
#ifndef __itkFFTTest_h
#define __itkFFTTest_h
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
  /*This test is build for testing forward and Inverse Fast Fourier Transforms
     using vnl , fftw and scsl fft libraries*/
#include "itkConfigure.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include "itkVnlFFTComplexConjugateToRealImageFilter.h"
#if defined(USE_FFTWF) || defined(USE_FFTWD)
#include "itkFFTWComplexConjugateToRealImageFilter.h"
#include "itkFFTWRealToComplexConjugateImageFilter.h"
#endif
#include "itksys/SystemTools.hxx"
#include "vnl/vnl_sample.h"
#include <math.h>

// test_fft is the test function and it is templated over the pixel, Image
// dimensions and the  FFT library to be used.
template <class TPixel,unsigned int VImageDimensions,
          class R2CType,class C2RType>
int test_fft(unsigned int *SizeOfDimensions)
{
  typedef itk::Image< TPixel , VImageDimensions >                  RealImageType;
  typedef itk::Image< std::complex<TPixel> , VImageDimensions >    ComplexImageType;
  unsigned int counter = 0;
  typename RealImageType::SizeType  imageSize;
  typename RealImageType::IndexType imageIndex;

  // We are testing the fft for 1d ,2d and 3d images. An array
  // (SizeOfDimensions) containing the sizes of each dimension is passed as an
  // argument to this function.Based on the template argument VImageDimensions, we
  // create a 1d 2d or 3d image by selecting the sizes of image dimensions from
  // this array .
  for(unsigned int i = 0; i < VImageDimensions; i++)
    {
    imageSize.SetElement(i,SizeOfDimensions[i]);
    imageIndex.SetElement(i,0);
    }

  typename RealImageType::RegionType region;
  region.SetSize(imageSize);
  region.SetIndex(imageIndex);
  typename RealImageType::Pointer realimage = RealImageType::New();
  /* Create the Real Image.*/
  realimage->SetLargestPossibleRegion(region);
  realimage->SetBufferedRegion(region);
  realimage->SetRequestedRegion(region);
  realimage->Allocate();
  vnl_sample_reseed(static_cast<int>(itksys::SystemTools::GetTime()/10000.0));
  /*We use 2 region iterators for this test the original image iterator and another iterator for
   the resultant image after performing FFT and IFFT */
  itk::ImageRegionIterator<RealImageType> OriginalImageIterator(realimage,region);

  /*Allocate random pixel values to the image by  iterating through the it and Print out the image data.*/
  try
    {
    while(!OriginalImageIterator.IsAtEnd())
      {
      TPixel val = vnl_sample_uniform(0.0, 16384.0);
      //TPixel val = static_cast<TPixel>(counter);
      if((counter + 1 ) % SizeOfDimensions[0] == 0)
        {
        std::cerr << val << std::endl;
        }
      else
        {
        std::cerr << val << " ";
        }
      counter++;
      OriginalImageIterator.Set(val);
      ++OriginalImageIterator;
      }
    std::cerr << std::endl << std::endl;
    }
  catch(itk::ExceptionObject & ex)
    {
    ex.Print(std::cerr);
    return -1;
    }
  /*Real to complex pointer. This computes the forward FFT*/
  typename R2CType::Pointer R2C = R2CType::New();
  /* complex to Real pointer. This computes the Inverse FFT*/
  typename C2RType::Pointer C2R = C2RType::New();
  /*Set the real image created as the input to the forwar FFT filter*/
  R2C->SetInput(realimage);
  R2C->Print( std::cout );
  R2C->Update();
  /*Get the size and the pointer to the complex image.*/
  typename ComplexImageType::Pointer complexImage = R2C->GetOutput();
  std::complex<TPixel> *fftbuf = complexImage->GetBufferPointer();
  const typename ComplexImageType::SizeType &complexImageSize =
    complexImage->GetLargestPossibleRegion().GetSize();
  unsigned int _Sizes[3] = { 1,1,1 };
  for(unsigned int i = 0; i < VImageDimensions; i++)
    {
    _Sizes[i] = complexImageSize[i];
    }
  /*Print out the  the frequency domain data obtained after performing the forward transform */
  for(unsigned int i = 0; i < _Sizes[2]; i++)
    {
    unsigned int zStride = i * _Sizes[1] * _Sizes[0];
    for(unsigned int j = 0; j < _Sizes[1]; j++)
      {
      unsigned int yStride = j * _Sizes[0];
      for(unsigned int k = 0; k < _Sizes[0]; k++)
        {
        std::cerr << fftbuf[zStride+yStride+k] << " ";
        }
      std::cerr << std::endl;
      }
    }

  std::cerr << std::endl << std::endl;

  // Perform the Inverse FFT to get back the Real Image.C@R is the complex
  // conjugate to real image filter and we give the obtained complex image as
  // input to this filter. This is the Inverse FFT of the image.
  C2R->SetInput(complexImage);

  //
  // newer method to inform filter that there's an odd # of pixels in the x dimension.
  const bool dimensionIsOdd = SizeOfDimensions[0] & 1;
  C2R->SetActualXDimensionIsOdd( dimensionIsOdd );
  C2R->Print( std::cout );
  C2R->Update();
  typename RealImageType::Pointer imageafterInverseFFT = C2R->GetOutput();
   /*The Inverse FFT image iterator is the resultant iterator after we
     perform the FFT and Inverse FFT on the Original Image*/
  itk::ImageRegionIterator<RealImageType> InverseFFTImageIterator(imageafterInverseFFT,region);
  counter = 0;
  InverseFFTImageIterator = InverseFFTImageIterator.Begin();
  /*Print the Image data obtained by performing the Inverse FFT. */
  while(!InverseFFTImageIterator.IsAtEnd())
    {
    TPixel val = InverseFFTImageIterator.Value();
    if(counter == imageSize[0])
      {
      std::cerr << val << std::endl;
      counter = 0;
      }
    else
      std::cerr << val << " ";
    counter++;
    ++InverseFFTImageIterator;
    }
  std::cerr << std::endl << std::endl;
  /*Subtract the Original image Pixel Values from the resultant image
   values and test whether they are greater than 0.01 for the test to pass*/
  OriginalImageIterator = OriginalImageIterator.Begin();
  InverseFFTImageIterator = InverseFFTImageIterator.Begin();
  while(!OriginalImageIterator.IsAtEnd())
    {
    TPixel val = OriginalImageIterator.Value();
    TPixel val2 = InverseFFTImageIterator.Value();
    TPixel diff = vnl_math_abs(val-val2);
    if(val != 0)
      {
      diff /= vnl_math_abs(val);
      }
    if(diff > 0.01)
      {
      std::cerr << "Diff found " << val << " " << val2 << " diff " << diff << std::endl;
      return -1;
      }
    ++OriginalImageIterator;
    ++InverseFFTImageIterator;
    }
  std::cerr << std::endl << std::endl;
  return 0;
}


/*test_fft_rtc is the test function to compare two implementations (Direct FFT only).
  It is templated over the pixel, Image dimensions and the  FFT libraries to be used.*/
template <class TPixel,unsigned int VImageDimensions,
          class R2CAType,class R2CBType>
int
test_fft_rtc(unsigned int *SizeOfDimensions)
{
  typedef itk::Image< TPixel , VImageDimensions >                RealImageType;
  typedef itk::Image< std::complex<TPixel> , VImageDimensions >  ComplexImageType;
  unsigned int counter = 0;
  typename RealImageType::SizeType    imageSize;
  typename RealImageType::IndexType   imageIndex;

  // We are testing the fft for 1d ,2d and 3d images. An array
  // (SizeOfDimensions) containing the sizes of each dimension is passed as an
  // argument to this function.Based on the template argument VImageDimensions,
  // we create a 1d 2d or 3d image by selecting the sizes of image dimensions
  // from this array .
  for(unsigned int i = 0; i < VImageDimensions; i++)
    {
    imageSize.SetElement(i,SizeOfDimensions[i]);
    imageIndex.SetElement(i,0);
    }

  typename RealImageType::RegionType region;
  region.SetSize(imageSize);
  region.SetIndex(imageIndex);
  typename RealImageType::Pointer realimage = RealImageType::New();
  /* Create the Real Image.*/
  realimage->SetLargestPossibleRegion(region);
  realimage->SetBufferedRegion(region);
  realimage->SetRequestedRegion(region);
  realimage->Allocate();
  vnl_sample_reseed(static_cast<int>(itksys::SystemTools::GetTime()/10000.0));
  /*We use 2 region iterators for this test the original image iterator and another iterator for
   the resultant image after performing FFT and IFFT */
  itk::ImageRegionIterator<RealImageType> OriginalImageIterator(realimage,region);

  /*Allocate random pixel values to the image by  iterating through the it and Print out the image data.*/
  try
    {
    while(!OriginalImageIterator.IsAtEnd())
      {
      TPixel val = vnl_sample_uniform(0.0, 16384.0);
      //TPixel val = static_cast<TPixel>(counter);
      if((counter + 1 ) % SizeOfDimensions[0] == 0)
        {
        std::cerr << val << std::endl;
        }
      else
        {
        std::cerr << val << " ";
        }
      counter++;
      OriginalImageIterator.Set(val);
      ++OriginalImageIterator;
      }
    std::cerr << std::endl << std::endl;
    }
  catch(itk::ExceptionObject & ex)
    {
    ex.Print(std::cerr);
    return -1;
    }

  /*Real to complex pointers. This computes the forward FFT*/
  typename R2CAType::Pointer R2Ca = R2CAType::New();

  /*Real to complex pointers. This computes the forward FFT*/
  typename R2CBType::Pointer R2Cb = R2CBType::New();

  /*Set the real image created as the input to the forwar FFT filter*/
  R2Ca->SetInput(realimage);
  R2Ca->Update();

  R2Cb->SetInput(realimage);
  R2Cb->Update();

  /*Get the size and the pointer to the complex image.*/
  typename ComplexImageType::Pointer complexImageA = R2Ca->GetOutput();
  std::complex<TPixel> *fftbufA = complexImageA->GetBufferPointer();
  const typename ComplexImageType::SizeType &complexImageSizeA =
    complexImageA->GetLargestPossibleRegion().GetSize();

  typename ComplexImageType::Pointer complexImageB = R2Cb->GetOutput();
  std::complex<TPixel> *fftbufB = complexImageB->GetBufferPointer();
  const typename ComplexImageType::SizeType &complexImageSizeB =
    complexImageB->GetLargestPossibleRegion().GetSize();


  unsigned int _SizesA[3] = { 1,1,1 };
  unsigned int _SizesB[3] = { 1,1,1 };
  for(unsigned int i = 0; i < VImageDimensions; i++)
    {
      /* the size may be different if one implementation returns
         a fullmatrix but not the other. */
      _SizesA[i] = complexImageSizeA[i];
      _SizesB[i] = complexImageSizeB[i];
    }

  /*Print out the  the frequency domain data obtained after performing the forward transform */
  for(unsigned int i = 0; i < _SizesA[2]; i++)
    {
    unsigned int zStride = i * _SizesA[1] * _SizesA[0];
    for(unsigned int j = 0; j < _SizesA[1]; j++)
      {
      unsigned int yStride = j * _SizesA[0];
      for(unsigned int k = 0; k < _SizesA[0]; k++)
        {
        std::cerr << fftbufA[zStride+yStride+k] << " ";
        }
      std::cerr << std::endl;
      }
    }
  std::cerr << std::endl << std::endl;

  for(unsigned int i = 0; i < _SizesB[2]; i++)
    {
    unsigned int zStride = i * _SizesB[1] * _SizesB[0];
    for(unsigned int j = 0; j < _SizesB[1]; j++)
      {
      unsigned int yStride = j * _SizesB[0];
      for(unsigned int k = 0; k < _SizesB[0]; k++)
        {
        std::cerr << fftbufB[zStride+yStride+k] << " ";
        }
      std::cerr << std::endl;
      }
    }
  std::cerr << std::endl << std::endl;


  /*Subtract the 2 images Pixel Values
    and test whether they are greater than 0.01 for the test to pass*/
  for(unsigned int i = 0; i < vnl_math_min(_SizesA[2],_SizesB[2]); i++)
    {
    unsigned int zStrideA = i * _SizesA[1] * _SizesA[0];
    unsigned int zStrideB = i * _SizesB[1] * _SizesB[0];
    for(unsigned int j = 0; j < vnl_math_min(_SizesA[1],_SizesB[1]); j++)
      {
      unsigned int yStrideA = j * _SizesA[0];
      unsigned int yStrideB = j * _SizesB[0];
      for(unsigned int k = 0; k < vnl_math_min(_SizesA[0],_SizesB[0]); k++)
        {
        double val = std::abs(fftbufA[zStrideA+yStrideA+k]);
        double diff = std::abs(fftbufA[zStrideA+yStrideA+k]-fftbufB[zStrideB+yStrideB+k]);
        if(val != 0)
          {
          diff /= vnl_math_abs(val);
          }
        if(diff > 0.01)
          {
          std::cerr << "Diff found " << fftbufA[zStrideA+yStrideA+k]
                    << " " << fftbufB[zStrideB+yStrideB+k] << " diff " << diff << std::endl;
          return -1;
          }
        }
      }
    }

  std::cerr << std::endl << std::endl;
  return 0;
}
#endif
