/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.


=========================================================================*/
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include "itkVnlFFTComplexConjugateToRealImageFilter.h"

#ifdef USE_FFTW
#include "itkFFTWComplexConjugateToRealImageFilter.h"
#include "itkFFTWRealToComplexConjugateImageFilter.h"
#endif

#include <itksys/SystemTools.hxx>
#include "vnl/vnl_sample.h"
#include <math.h>

template <class TPixel,unsigned int dim>
int 
test_fft(unsigned int *dims)
{
  typedef itk::Image< TPixel , dim > RealImageType;
  typedef itk::Image< std::complex<TPixel> , dim > ComplexImageType;
  unsigned int counter = 0;
  typename RealImageType::SizeType imageSize;
  typename RealImageType::IndexType imageIndex;
  for(unsigned int i = 0; i < dim; i++)
    {
    imageSize.SetElement(i,dims[i]);
    imageIndex.SetElement(i,0);
    }

  typename RealImageType::RegionType region;
  region.SetSize(imageSize);
  region.SetIndex(imageIndex);
  typename RealImageType::Pointer img = RealImageType::New();

  img->SetLargestPossibleRegion(region);
  img->SetBufferedRegion(region);
  img->SetRequestedRegion(region);
  img->Allocate();

  vnl_sample_reseed(static_cast<int>(10000*itksys::SystemTools::GetTime()));
  itk::ImageRegionIterator<RealImageType> ri(img,region);
  try
    {

    while(!ri.IsAtEnd())
      {
      TPixel val = vnl_sample_uniform(0.0, 16384.0);
      //TPixel val = static_cast<TPixel>(counter);
      if((counter + 1 )% 4 == 0)
        std::cerr << val << std::endl;
      else
        std::cerr << val << " ";
      counter++;
      ri.Set(val);
      ++ri;
      }
    std::cerr << std::endl << std::endl;
    }
  catch(itk::ExceptionObject & ex)
    {
    ex.Print(std::cerr);
    return -1;
    }
  typedef itk::VnlFFTRealToComplexConjugateImageFilter<TPixel,dim> 
    VnlRealToComplexFilterType;

  typedef itk::VnlFFTComplexConjugateToRealImageFilter<TPixel,dim> 
    VnlComplexToRealFilterType;

  typename VnlRealToComplexFilterType::Pointer VnlR2C = 
    VnlRealToComplexFilterType::New();
  typename VnlComplexToRealFilterType::Pointer VnlC2R = 
    VnlComplexToRealFilterType::New();
  VnlR2C->SetInput(img);
  VnlR2C->Update();
  typename ComplexImageType::Pointer VnlcomplexImage = VnlR2C->GetOutput();
  std::complex<TPixel> *fftbuf = VnlcomplexImage->GetBufferPointer();
  const typename ComplexImageType::SizeType &VnlcomplexImageSize = 
    VnlcomplexImage->GetLargestPossibleRegion().GetSize();

  unsigned int _Sizes[3] = { 1,1,1 };
  unsigned int i;
  for(i = 0; i < dim; i++)
    {
    _Sizes[i] = VnlcomplexImageSize[i];
    }
  for(i = 0; i < _Sizes[2]; i++)
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

  VnlC2R->SetInput(VnlcomplexImage);
  VnlC2R->Update();
  typename RealImageType::Pointer Vnlimg2 = VnlC2R->GetOutput();
  itk::ImageRegionIterator<RealImageType> vnlri(Vnlimg2,region);
  counter = 0;
  ri = ri.Begin();
  while(!vnlri.IsAtEnd())
    {
    TPixel val = vnlri.Value();
    TPixel val2 = ri.Value();
    TPixel diff = fabs(val-val2)/fabs(val);
    if(val != 0) diff /= val;
    if(diff > 0.005)
      {
      std::cerr << "Diff found " << val << " " << val2 << " diff " << diff << std::endl;
      return -1;
      }
    if((counter+1) % imageSize[0] == 0)
      std::cerr << val << std::endl;
    else
      std::cerr << val << " ";
    counter++;
    ++vnlri;
    ++ri;
    }
  std::cerr << std::endl << std::endl;
  return 0;

}

#ifdef USE_FFTW
template <class TPixel,unsigned int dim>
int test_fftw(unsigned int *dims)
{
  typedef itk::Image< TPixel , dim > RealImageType;
  typedef itk::Image< std::complex<TPixel> , dim > ComplexImageType;
  unsigned int counter = 0;
  typename RealImageType::SizeType imageSize;
  typename RealImageType::IndexType imageIndex;
  for(unsigned int i = 0; i < dim; i++)
    {
    imageSize.SetElement(i,dims[i]);
    imageIndex.SetElement(i,0);
    }

  typename RealImageType::RegionType region;
  region.SetSize(imageSize);
  region.SetIndex(imageIndex);
  typename RealImageType::Pointer img = RealImageType::New();

  img->SetLargestPossibleRegion(region);
  img->SetBufferedRegion(region);
  img->SetRequestedRegion(region);
  img->Allocate();

  vnl_sample_reseed(static_cast<int>(10000*itksys::SystemTools::GetTime()));
  itk::ImageRegionIterator<RealImageType> ri(img,region);
  try
    {

    while(!ri.IsAtEnd())
      {
      TPixel val = vnl_sample_uniform(0.0, 16384.0);
      //TPixel val = static_cast<TPixel>(counter);
      if((counter + 1 )% 4 == 0)
        std::cerr << val << std::endl;
      else
        std::cerr << val << " ";
      counter++;
      ri.Set(val);
      ++ri;
      }
    std::cerr << std::endl << std::endl;
    }
  catch(itk::ExceptionObject & ex)
    {
    ex.Print(std::cerr);
    return -1;
    }
  typedef itk::FFTWRealToComplexConjugateImageFilter<TPixel,dim> 
    FFTWRealToComplexFilterType;
  typedef itk::FFTWComplexConjugateToRealImageFilter<TPixel,dim> 
    FFTWComplexToRealFilterType;
  typename FFTWRealToComplexFilterType::Pointer FFTWR2C = 
    FFTWRealToComplexFilterType::New();
  typename FFTWComplexToRealFilterType::Pointer FFTWC2R = 
    FFTWComplexToRealFilterType::New();
  FFTWR2C->SetInput(img);
  FFTWR2C->Update();
  typename ComplexImageType::Pointer FFTWcomplexImage = FFTWR2C->GetOutput();
  FFTWC2R->SetInput(FFTWcomplexImage);
  FFTWC2R->Update();
  typename RealImageType::Pointer FFTWimg2 = FFTWC2R->GetOutput();

  itk::ImageRegionIterator<RealImageType> FFTWri(FFTWimg2,region);
  counter = 0;
  ri = ri.Begin();
  while(!FFTWri.IsAtEnd())
    {
    TPixel val = FFTWri.Value();
    TPixel val2 = ri.Value();
    TPixel diff = fabs(val-val2)/fabs(val);
    if(val != 0) diff /= val;
    if(diff > 0.001)
      {
      std::cerr << "Diff found " << val << " " << val2 << " diff " << diff << std::endl;
      return -1;
      }
    if((counter+1) % imageSize[0] == 0)
      std::cerr << val << std::endl;
    else
      std::cerr << val << " ";
    counter++;
    ++FFTWri;
    ++ri;
    }
  std::cerr << std::endl << std::endl;
  return 0;
}
#endif

int itkFFTTest(int, char *[])
{
  unsigned int dims1[] = { 4,4,4 };
  unsigned int dims2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "float,1" << std::endl;
  if((rval = test_fft<float,1>(dims1)) != 0)
    return -1;
  std::cerr << "float,2"<< std::endl;
  if((rval = test_fft<float,2>(dims1)) != 0)
    return -1;
  std::cerr << "float,3"<< std::endl;
  if((rval = test_fft<float,3>(dims1)) != 0)
    return -1;
  std::cerr << "double,1"<< std::endl;
  if((rval = test_fft<double,1>(dims1)) != 0)
    return -1;
  std::cerr << "double,2"<< std::endl;
  if((rval = test_fft<double,2>(dims1)) != 0)
    return -1;
  std::cerr << "double,3"<< std::endl;
  if((rval = test_fft<double,3>(dims1)) != 0)
    return -1;
  std::cerr << "float,1" << std::endl;
  if((rval = test_fft<float,1>(dims2)) != 0)
    return -1;
  std::cerr << "float,2"<< std::endl;
  if((rval = test_fft<float,2>(dims2)) != 0)
    return -1;
  std::cerr << "float,3"<< std::endl;
  if((rval = test_fft<float,3>(dims2)) != 0)
    return -1;
  std::cerr << "double,1"<< std::endl;
  if((rval = test_fft<double,1>(dims2)) != 0)
    return -1;
  std::cerr << "double,2"<< std::endl;
  if((rval = test_fft<double,2>(dims2)) != 0)
    return -1;
  std::cerr << "double,3"<< std::endl;
  if((rval = test_fft<double,3>(dims2)) != 0)
    return -1;
#ifdef USE_FFTW
  std::cerr << "fftw float,1" << std::endl;
  if((rval = test_fftw<float,1>(dims1)) != 0)
    return -1;
  std::cerr << "fftw float,2"<< std::endl;
  if((rval = test_fftw<float,2>(dims1)) != 0)
    return -1;
  std::cerr << "fftw float,3"<< std::endl;
  if((rval = test_fftw<float,3>(dims1)) != 0)
    return -1;
  std::cerr << "fftw double,1"<< std::endl;
  if((rval = test_fftw<double,1>(dims1)) != 0)
    return -1;
  std::cerr << "fftw double,2"<< std::endl;
  if((rval = test_fftw<double,2>(dims1)) != 0)
    return -1;
  std::cerr << "fftw double,3"<< std::endl;
  if((rval = test_fftw<double,3>(dims1)) != 0)
    return -1;
  std::cerr << "fftw float,1" << std::endl;
  if((rval = test_fftw<float,1>(dims2)) != 0)
    return -1;
  std::cerr << "fftw float,2"<< std::endl;
  if((rval = test_fftw<float,2>(dims2)) != 0)
    return -1;
  std::cerr << "fftw float,3"<< std::endl;
  if((rval = test_fftw<float,3>(dims2)) != 0)
    return -1;
  std::cerr << "fftw double,1"<< std::endl;
  if((rval = test_fftw<double,1>(dims2)) != 0)
    return -1;
  std::cerr << "fftw double,2"<< std::endl;
  if((rval = test_fftw<double,2>(dims2)) != 0)
    return -1;
  std::cerr << "fftw double,3"<< std::endl;
  if((rval = test_fftw<double,3>(dims2)) != 0)
    return -1;
#endif
  return rval;
}
