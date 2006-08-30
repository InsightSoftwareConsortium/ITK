/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  CopyOriginalImageIteratorght (c) 2002 Insight Consortium. All OriginalImageIteratorghts reserved.
  See ITKCopyOriginalImageIteratorght.txt or http://www.itk.org/HTML/CopyOriginalImageIteratorght.htm for details.
     This software is distOriginalImageIteratorbuted WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
#ifdef USE_SCSL
#include "itkSCSLComplexConjugateToRealImageFilter.h"
#include "itkSCSLRealToComplexConjugateImageFilter.h"
#endif
#include <itksys/SystemTools.hxx>
#include "vnl/vnl_sample.h"
#include <math.h>
/*test_fft is the test function and it is templated over the pixel , Image dimensions and the  FFT library to be used.*/
template <class TPixel,unsigned int ImageDimensions,
          class R2CType,class C2RType>
int
test_fft(unsigned int *SizeOfDimensions)
{
  typedef itk::Image< TPixel , ImageDimensions > RealImageType;
  typedef itk::Image< std::complex<TPixel> , ImageDimensions > ComplexImageType;
  unsigned int counter = 0;
  typename RealImageType::SizeType imageSize;
  typename RealImageType::IndexType imageIndex;
  /* We are testing the fft for 1d ,2d and 3d images. An array  (SizeOfDimensions) containing the sizes of each dimension is passed as an argument to this function.Based on the template argument ImageDimensions, we create a 1d 2d or 3d image by selecting the sizes of image dimensions from this array .*/
  for(unsigned int i = 0; i < ImageDimensions; i++)
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
  vnl_sample_reseed(static_cast<int>(10000*itksys::SystemTools::GetTime()));
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
        std::cerr << val << std::endl;
      else
        std::cerr << val << " ";
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
  R2C->Update();
  /*Get the size and the pointer to the complex image.*/
  typename ComplexImageType::Pointer complexImage = R2C->GetOutput();
  std::complex<TPixel> *fftbuf = complexImage->GetBufferPointer();
  const typename ComplexImageType::SizeType &complexImageSize =
    complexImage->GetLargestPossibleRegion().GetSize();
  unsigned int _Sizes[3] = { 1,1,1 };
  for(unsigned int i = 0; i < ImageDimensions; i++)
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
  /*Perform the Inverse FFT to get back the Real Image.C@R is the complex conjugate to real image filter and we give the obtained complex image as input to this filter. This is the Inverse FFT of the image.*/
  C2R->SetInput(complexImage);
  //
  // newer method to inform filter that there's an odd # of pixels in the x dimension.
  C2R->SetActualXDimensionIsOdd(SizeOfDimensions[0] & 1 != 0);
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
   /*Test FFT using VNL Libraries. The test is performed for 2 3d array one of them
   having the same dimension(4,4,4) and the other having different dimensions (3,4,5).
   Images are created with different dimensions in the test function based on the second template argument    and  the size of these dimensions are taken from the array.
   The data types used are float and double. */
int itkVnlFFTTest(int, char *[])
{
  unsigned int SizeOfDimensions1[] = { 4,4,4 };
  unsigned int SizeOfDimensions2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "Vnl float,1 (4,4,4)" << std::endl;
  if((test_fft<float,1,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,1> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,1> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "Vnl float,2 (4,4,4)"<< std::endl;
  if((test_fft<float,2,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,2> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,2> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "Vnl float,3 (4,4,4)"<< std::endl;
  if((test_fft<float,3,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,3> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,3> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "Vnl double,1 (4,4,4)"<< std::endl;
  if((test_fft<double,1,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,1> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,1> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "Vnl double,2 (4,4,4)"<< std::endl;
  if((test_fft<double,2,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,2> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,2> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "Vnl double,3 (4,4,4)"<< std::endl;
  if((test_fft<double,3,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,3> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,3> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "Vnl float,1 (3,5,4)" << std::endl;
  if((test_fft<float,1,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,1> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,1> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "Vnl float,2 (3,5,4)"<< std::endl;
  if((test_fft<float,2,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,2> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,2> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "Vnl float,3 (3,5,4)"<< std::endl;
  if((test_fft<float,3,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,3> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,3> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "Vnl double,1 (3,5,4)"<< std::endl;
  if((test_fft<double,1,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,1> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,1> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "Vnl double,2 (3,5,4)"<< std::endl;
  if((test_fft<double,2,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,2> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,2> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "Vnl double,3 (3,5,4)"<< std::endl;
  if((test_fft<double,3,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,3> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,3> >(SizeOfDimensions2)) != 0)
    rval++;;
  return rval == 0 ? 0 : -1;
}

#if defined(USE_FFTWF)
   /*Test FFT using FFTW Libraries. The test is performed for 2 3d array one of them
   having the same dimension(4,4,4) and the other having different dimensions (3,4,5).
   Images are created with different dimensions in the test function based on the second template argument   and  the size of these dimensions are taken from the array.The data types used are float and double. */
int itkFFTWF_FFTTest(int, char *[])
{
  unsigned int SizeOfDimensions1[] = { 4,4,4 };
  unsigned int SizeOfDimensions2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "FFTWF:float,1 (4,4,4)" << std::endl;
  if((test_fft<float,1,
      itk::FFTWRealToComplexConjugateImageFilter<float,1> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,1> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "FFTWF:float,2 (4,4,4)"<< std::endl;
  if((test_fft<float,2,
      itk::FFTWRealToComplexConjugateImageFilter<float,2> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,2> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "FFTWF:float,3 (4,4,4)"<< std::endl;
  if((test_fft<float,3,
      itk::FFTWRealToComplexConjugateImageFilter<float,3> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,3> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "FFTWF:float,1 (3,5,4)" << std::endl;
  if((test_fft<float,1,
      itk::FFTWRealToComplexConjugateImageFilter<float,1> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,1> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "FFTWF:float,2 (3,5,4)"<< std::endl;
  if((test_fft<float,2,
      itk::FFTWRealToComplexConjugateImageFilter<float,2> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,2> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "FFTWF:float,3 (3,5,4)"<< std::endl;
  if((test_fft<float,3,
      itk::FFTWRealToComplexConjugateImageFilter<float,3> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,3> >(SizeOfDimensions2)) != 0)
    rval++;;

  return (rval == 0) ? 0 : -1;
}
#endif
#if defined(USE_FFTWD)
int itkFFTWD_FFTTest(int, char *[])
{
  unsigned int SizeOfDimensions1[] = { 4,4,4 };
  unsigned int SizeOfDimensions2[] = { 3,5,4 };
  int rval = 0;

  std::cerr << "FFTWD:double,1 (4,4,4)"<< std::endl;
  if((test_fft<double,1,
      itk::FFTWRealToComplexConjugateImageFilter<double,1> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,1> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "FFTWD:double,2 (4,4,4)"<< std::endl;
  if((test_fft<double,2,
      itk::FFTWRealToComplexConjugateImageFilter<double,2> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,2> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "FFTWD:double,3 (4,4,4)"<< std::endl;
  if((test_fft<double,3,
      itk::FFTWRealToComplexConjugateImageFilter<double,3> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,3> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "FFTWD:double,1 (3,5,4)"<< std::endl;
  if((test_fft<double,1,
      itk::FFTWRealToComplexConjugateImageFilter<double,1> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,1> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "FFTWD:double,2 (3,5,4)"<< std::endl;
  if((test_fft<double,2,
      itk::FFTWRealToComplexConjugateImageFilter<double,2> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,2> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "FFTWD:double,3 (3,5,4)"<< std::endl;
  if((test_fft<double,3,
      itk::FFTWRealToComplexConjugateImageFilter<double,3> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,3> >(SizeOfDimensions2)) != 0)
    rval++;;
  return (rval == 0) ? 0 : -1;
}
#endif
#if defined(USE_SCSL)
  /*Test FFT using SCSL Libraries. The test is performed for 2 3d array one of them
   having the same size along all dimension(4,4,4) and the other having different sizes in all dimensions (3,4,5).
   Images are created with different dimensions in the test function based on the second template argument  and  the size of these dimensions are taken from the array. The data types used are float and double. */
int itkSCSLFFTTest(int, char *[])
{
  unsigned int SizeOfDimensions1[] = { 4,4,4 };
  unsigned int SizeOfDimensions2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "SCSL:float,1 (4,4,4)" << std::endl;
  if((test_fft<float,1,
      itk::SCSLRealToComplexConjugateImageFilter<float,1> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,1> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "SCSL:float,2 (4,4,4)"<< std::endl;
  if((test_fft<float,2,
      itk::SCSLRealToComplexConjugateImageFilter<float,2> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,2> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "SCSL:float,3 (4,4,4)"<< std::endl;
  if((test_fft<float,3,
      itk::SCSLRealToComplexConjugateImageFilter<float,3> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,3> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "SCSL:double,1 (4,4,4)"<< std::endl;
  if((test_fft<double,1,
      itk::SCSLRealToComplexConjugateImageFilter<double,1> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,1> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "SCSL:double,2 (4,4,4)"<< std::endl;
  if((test_fft<double,2,
      itk::SCSLRealToComplexConjugateImageFilter<double,2> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,2> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "SCSL:double,3 (4,4,4)"<< std::endl;
  if((test_fft<double,3,
      itk::SCSLRealToComplexConjugateImageFilter<double,3> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,3> >(SizeOfDimensions1)) != 0)
    rval++;;
  std::cerr << "SCSL:float,1 (3,5,4)" << std::endl;
  if((test_fft<float,1,
      itk::SCSLRealToComplexConjugateImageFilter<float,1> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,1> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "SCSL:float,2 (3,5,4)"<< std::endl;
  if((test_fft<float,2,
      itk::SCSLRealToComplexConjugateImageFilter<float,2> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,2> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "SCSL:float,3 (3,5,4)"<< std::endl;
  if((test_fft<float,3,
      itk::SCSLRealToComplexConjugateImageFilter<float,3> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,3> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "SCSL:double,1 (3,5,4)"<< std::endl;
  if((test_fft<double,1,
      itk::SCSLRealToComplexConjugateImageFilter<double,1> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,1> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "SCSL:double,2 (3,5,4)"<< std::endl;
  if((test_fft<double,2,
      itk::SCSLRealToComplexConjugateImageFilter<double,2> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,2> >(SizeOfDimensions2)) != 0)
    rval++;;
  std::cerr << "SCSL:double,3 (3,5,4)"<< std::endl;
  if((test_fft<double,3,
      itk::SCSLRealToComplexConjugateImageFilter<double,3> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,3> >(SizeOfDimensions2)) != 0)
    rval++;;
  return rval == 0 ? 0 : -1;
}
#endif

