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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"

#include "itkImageRegionIterator.h"


#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include "itkVnlFFTComplexConjugateToRealImageFilter.h"

#ifdef USE_FFTW
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

template <class TPixel,unsigned int dim,
          class R2CType,class C2RType>
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
      if((counter + 1 ) % dims[0] == 0)
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
  typename R2CType::Pointer R2C = 
    R2CType::New();
  typename C2RType::Pointer C2R = 
    C2RType::New();
  R2C->SetInput(img);
  R2C->Update();
  typename ComplexImageType::Pointer complexImage = R2C->GetOutput();
  std::complex<TPixel> *fftbuf = complexImage->GetBufferPointer();
  const typename ComplexImageType::SizeType &complexImageSize = 
    complexImage->GetLargestPossibleRegion().GetSize();
  unsigned int _Sizes[3] = { 1,1,1 };
  
  for(unsigned int i = 0; i < dim; i++)
    {
    _Sizes[i] = complexImageSize[i];
    }

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

  C2R->SetInput(complexImage);
  C2R->Update();
  typename RealImageType::Pointer img2 = C2R->GetOutput();
  itk::ImageRegionIterator<RealImageType> ri2(img2,region);
  counter = 0;

  ri2 = ri2.Begin();
  while(!ri2.IsAtEnd())
    {
    TPixel val = ri2.Value();
    if(counter == imageSize[0])
      {
      std::cerr << val << std::endl;
      counter = 0;
      }
    else
      std::cerr << val << " ";
    counter++;
    ++ri2;
    }
  std::cerr << std::endl << std::endl;

  ri = ri.Begin();
  ri2 = ri2.Begin();
  while(!ri.IsAtEnd())
    {
    TPixel val = ri.Value();
    TPixel val2 = ri2.Value();
    TPixel diff = fabs(val-val2);
    if(val != 0)
      {
      diff /= fabs(val);
      }
    if(diff > 0.01)
      {
      std::cerr << "Diff found " << val << " " << val2 << " diff " << diff << std::endl;
      return -1;
      }
    ++ri;
    ++ri2;
    }
  std::cerr << std::endl << std::endl;
  return 0;

}

int itkVnlFFTTest(int, char *[])
{
  unsigned int dims1[] = { 4,4,4 };
  unsigned int dims2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "Vnl float,1 (4,4,4)" << std::endl;
  if((test_fft<float,1,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,1> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,1> >(dims1)) != 0)
    rval++;;
  std::cerr << "Vnl float,2 (4,4,4)"<< std::endl;
  if((test_fft<float,2,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,2> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,2> >(dims1)) != 0)
    rval++;;
  std::cerr << "Vnl float,3 (4,4,4)"<< std::endl;
  if((test_fft<float,3,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,3> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,3> >(dims1)) != 0)
    rval++;;
  std::cerr << "Vnl double,1 (4,4,4)"<< std::endl;
  if((test_fft<double,1,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,1> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,1> >(dims1)) != 0)
    rval++;;
  std::cerr << "Vnl double,2 (4,4,4)"<< std::endl;
  if((test_fft<double,2,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,2> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,2> >(dims1)) != 0)
    rval++;;
  std::cerr << "Vnl double,3 (4,4,4)"<< std::endl;
  if((test_fft<double,3,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,3> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,3> >(dims1)) != 0)
    rval++;;
  std::cerr << "Vnl float,1 (3,5,4)" << std::endl;
  if((test_fft<float,1,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,1> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,1> >(dims2)) != 0)
    rval++;;
  std::cerr << "Vnl float,2 (3,5,4)"<< std::endl;
  if((test_fft<float,2,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,2> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,2> >(dims2)) != 0)
    rval++;;
  std::cerr << "Vnl float,3 (3,5,4)"<< std::endl;
  if((test_fft<float,3,
      itk::VnlFFTRealToComplexConjugateImageFilter<float,3> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<float,3> >(dims2)) != 0)
    rval++;;
  std::cerr << "Vnl double,1 (3,5,4)"<< std::endl;
  if((test_fft<double,1,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,1> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,1> >(dims2)) != 0)
    rval++;;
  std::cerr << "Vnl double,2 (3,5,4)"<< std::endl;
  if((test_fft<double,2,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,2> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,2> >(dims2)) != 0)
    rval++;;
  std::cerr << "Vnl double,3 (3,5,4)"<< std::endl;
  if((test_fft<double,3,
      itk::VnlFFTRealToComplexConjugateImageFilter<double,3> ,
      itk::VnlFFTComplexConjugateToRealImageFilter<double,3> >(dims2)) != 0)
    rval++;;
  return rval == 0 ? 0 : -1;
}

#ifdef USE_FFTW
int itkFFTWFFTTest(int, char *[])
{
  unsigned int dims1[] = { 4,4,4 };
  unsigned int dims2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "FFTW:float,1 (4,4,4)" << std::endl;
  if((test_fft<float,1,
      itk::FFTWRealToComplexConjugateImageFilter<float,1> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,1> >(dims1)) != 0)
    rval++;;
  std::cerr << "FFTW:float,2 (4,4,4)"<< std::endl;
  if((test_fft<float,2,
      itk::FFTWRealToComplexConjugateImageFilter<float,2> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,2> >(dims1)) != 0)
    rval++;;
  std::cerr << "FFTW:float,3 (4,4,4)"<< std::endl;
  if((test_fft<float,3,
      itk::FFTWRealToComplexConjugateImageFilter<float,3> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,3> >(dims1)) != 0)
    rval++;;
  std::cerr << "FFTW:double,1 (4,4,4)"<< std::endl;
  if((test_fft<double,1,
      itk::FFTWRealToComplexConjugateImageFilter<double,1> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,1> >(dims1)) != 0)
    rval++;;
  std::cerr << "FFTW:double,2 (4,4,4)"<< std::endl;
  if((test_fft<double,2,
      itk::FFTWRealToComplexConjugateImageFilter<double,2> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,2> >(dims1)) != 0)
    rval++;;
  std::cerr << "FFTW:double,3 (4,4,4)"<< std::endl;
  if((test_fft<double,3,
      itk::FFTWRealToComplexConjugateImageFilter<double,3> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,3> >(dims1)) != 0)
    rval++;;
  std::cerr << "FFTW:float,1 (3,5,4)" << std::endl;
  if((test_fft<float,1,
      itk::FFTWRealToComplexConjugateImageFilter<float,1> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,1> >(dims2)) != 0)
    rval++;;
  std::cerr << "FFTW:float,2 (3,5,4)"<< std::endl;
  if((test_fft<float,2,
      itk::FFTWRealToComplexConjugateImageFilter<float,2> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,2> >(dims2)) != 0)
    rval++;;
  std::cerr << "FFTW:float,3 (3,5,4)"<< std::endl;
  if((test_fft<float,3,
      itk::FFTWRealToComplexConjugateImageFilter<float,3> ,
      itk::FFTWComplexConjugateToRealImageFilter<float,3> >(dims2)) != 0)
    rval++;;
  std::cerr << "FFTW:double,1 (3,5,4)"<< std::endl;
  if((test_fft<double,1,
      itk::FFTWRealToComplexConjugateImageFilter<double,1> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,1> >(dims2)) != 0)
    rval++;;
  std::cerr << "FFTW:double,2 (3,5,4)"<< std::endl;
  if((test_fft<double,2,
      itk::FFTWRealToComplexConjugateImageFilter<double,2> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,2> >(dims2)) != 0)
    rval++;;
  std::cerr << "FFTW:double,3 (3,5,4)"<< std::endl;
  if((test_fft<double,3,
      itk::FFTWRealToComplexConjugateImageFilter<double,3> ,
      itk::FFTWComplexConjugateToRealImageFilter<double,3> >(dims2)) != 0)
    rval++;;
  return rval == 0 ? 0 : -1;
}
#endif

#if defined(USE_SCSL)
int itkSCSLFFTTest(int, char *[])
{
  unsigned int dims1[] = { 4,4,4 };
  unsigned int dims2[] = { 3,5,4 };
  int rval = 0;
  std::cerr << "SCSL:float,1 (4,4,4)" << std::endl;
  if((test_fft<float,1,
      itk::SCSLRealToComplexConjugateImageFilter<float,1> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,1> >(dims1)) != 0)
    rval++;;
  std::cerr << "SCSL:float,2 (4,4,4)"<< std::endl;
  if((test_fft<float,2,
      itk::SCSLRealToComplexConjugateImageFilter<float,2> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,2> >(dims1)) != 0)
    rval++;;
  std::cerr << "SCSL:float,3 (4,4,4)"<< std::endl;
  if((test_fft<float,3,
      itk::SCSLRealToComplexConjugateImageFilter<float,3> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,3> >(dims1)) != 0)
    rval++;;
  std::cerr << "SCSL:double,1 (4,4,4)"<< std::endl;
  if((test_fft<double,1,
      itk::SCSLRealToComplexConjugateImageFilter<double,1> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,1> >(dims1)) != 0)
    rval++;;
  std::cerr << "SCSL:double,2 (4,4,4)"<< std::endl;
  if((test_fft<double,2,
      itk::SCSLRealToComplexConjugateImageFilter<double,2> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,2> >(dims1)) != 0)
    rval++;;
  std::cerr << "SCSL:double,3 (4,4,4)"<< std::endl;
  if((test_fft<double,3,
      itk::SCSLRealToComplexConjugateImageFilter<double,3> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,3> >(dims1)) != 0)
    rval++;;
  std::cerr << "SCSL:float,1 (3,5,4)" << std::endl;
  if((test_fft<float,1,
      itk::SCSLRealToComplexConjugateImageFilter<float,1> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,1> >(dims2)) != 0)
    rval++;;
  std::cerr << "SCSL:float,2 (3,5,4)"<< std::endl;
  if((test_fft<float,2,
      itk::SCSLRealToComplexConjugateImageFilter<float,2> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,2> >(dims2)) != 0)
    rval++;;
  std::cerr << "SCSL:float,3 (3,5,4)"<< std::endl;
  if((test_fft<float,3,
      itk::SCSLRealToComplexConjugateImageFilter<float,3> ,
      itk::SCSLComplexConjugateToRealImageFilter<float,3> >(dims2)) != 0)
    rval++;;
  std::cerr << "SCSL:double,1 (3,5,4)"<< std::endl;
  if((test_fft<double,1,
      itk::SCSLRealToComplexConjugateImageFilter<double,1> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,1> >(dims2)) != 0)
    rval++;;
  std::cerr << "SCSL:double,2 (3,5,4)"<< std::endl;
  if((test_fft<double,2,
      itk::SCSLRealToComplexConjugateImageFilter<double,2> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,2> >(dims2)) != 0)
    rval++;;
  std::cerr << "SCSL:double,3 (3,5,4)"<< std::endl;
  if((test_fft<double,3,
      itk::SCSLRealToComplexConjugateImageFilter<double,3> ,
      itk::SCSLComplexConjugateToRealImageFilter<double,3> >(dims2)) != 0)
    rval++;;
  return rval == 0 ? 0 : -1;
}
#endif

