/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUUImageToImageFilterDriver.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkUUImageToImageFilterDriver_h
#define __itkUUImageToImageFilterDriver_h

#include <iostream>
#include "itkImage.h"
#include "itkWriteImage.h"
#include "itkImageSource.h"
#include "itkFilterImageToImage.h"
#include "itkRandomImageSource.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkIndex.h"
#include "itkImageRegionIterator.h"
extern "C" {
#include "time.h"
}

template< class T, unsigned int TDimension >
void PrintRegion(itk::SmartPointer< itk::Image<T, TDimension> > I)
{
  int iDim, i, ArrayLength;
  int nnf[TDimension];
  unsigned long rsz[TDimension];
  unsigned long Location[TDimension];
  
  memcpy(rsz, I->GetRequestedRegion().GetSize().m_Size,
         sizeof(unsigned long) * TDimension);
  memset(Location, 0, sizeof(unsigned long) * TDimension); 
  for (iDim = 0; iDim < TDimension; ++iDim)
    {
      cout << "iDim = " << iDim << endl;
      cout << "\tRegionSize = "
           << I->GetRequestedRegion().GetSize().m_Size[iDim]
           << endl;
      cout << "\tRegionStartIndex = "
           << I->GetRequestedRegion().GetIndex()[iDim] << endl;
    }
  
  itk::ImageRegionIterator<T, TDimension> iter( I, I->GetRequestedRegion());
    
  for (iter.Begin(); ! iter.IsAtEnd(); ++iter)
    {
      cout << itk::ScalarTraits<T>::GetScalar(*iter) << " ";
      
      for (iDim=TDimension-1; iDim>=0; iDim--)
		{
		  Location[iDim]++;
		  if (Location[iDim]==rsz[TDimension-1 -iDim])
            {
              cout << endl;
              Location[iDim]=0;
            }
		  else break;
		}
    }
}

template< class T, unsigned int N >
void FillRegionSequential(itk::SmartPointer< itk::Image<T, N> > I)
{
  unsigned int iTemp, ArrayLength, iDim,i;
  itk::Size<N> Index;
  unsigned long int Location[N];
  unsigned int mult;
  T value;
  
  itk::ImageRegionIterator<T, N> data(I, I->GetRequestedRegion());

  Index = (I->GetRequestedRegion()).GetSize();
  data.Begin();
  
  for (ArrayLength=1, iTemp = 0; iTemp<N; iTemp++)
	{
	  Location[iTemp] =0;
	  ArrayLength*=Index[iTemp];
	}
  
  for (i=0; i<ArrayLength; i++, ++data)
	{
	  for (iDim=0, mult=1, value=0; iDim<N; iDim++, mult*=10)
		{
		  value += mult *  Location[N-iDim-1];
		}
	  *data = value;
	  
	  for (iDim=N-1; iDim>=0; iDim--)
		{
		  Location[iDim]++;
		  if (Location[iDim]==Index[(N-1)-iDim]) Location[iDim]=0;
		  else break;
		}
	  
	}
}

namespace itk { 

template <class TOutputImage>
class ITK_EXPORT NullImageSource: public ImageSource<TOutputImage>
{
public:
  typedef NullImageSource Self;
  typedef SmartPointer<Self> Pointer;
  typedef ImageSource<TOutputImage> Superclass;
  itkNewMacro(Self);
  void SetImageSize(const typename TOutputImage::Size s) {m_ImageSize = s;}
  typename TOutputImage::Pointer GetNonPipelineOutput()
  { return  m_NonPipelineOutput; }
  void NonPipelineExecute();
protected:
  virtual void GenerateData();
  virtual void GenerateOutputInformation();
  
private:
  typename TOutputImage::Size m_ImageSize;
  typename TOutputImage::Pointer m_NonPipelineOutput;
};

template <class TOutputImage>
void
NullImageSource<TOutputImage>::GenerateOutputInformation()
{
  TOutputImage *output;
  typename TOutputImage::Index index = {0};
  output = this->GetOutput(0);
  typename TOutputImage::Region largestPossibleRegion;
  largestPossibleRegion.SetSize( m_ImageSize );
  largestPossibleRegion.SetIndex( index);
  output->SetLargestPossibleRegion( largestPossibleRegion );
}

template <class TOutputImage>
void
NullImageSource<TOutputImage>::GenerateData()
{
  typename TOutputImage::Pointer image = this->GetOutput(0);
  image->SetBufferedRegion( image->GetRequestedRegion() );
  image->Allocate();
  FillRegionSequential<typename TOutputImage::PixelType,
    TOutputImage::ImageDimension>(image);
  //  PrintRegion<typename TOutputImage::PixelType,
  //    TOutputImage::ImageDimension>(image);
}

template <class TOutputImage>
void
NullImageSource<TOutputImage>::NonPipelineExecute()
{
  ImageRegion<TOutputImage::ImageDimension> rg;
  itk::Index<TOutputImage::ImageDimension> idx;
  for (int i = 0; i < TOutputImage::ImageDimension; ++i)  { idx[i]=0; }
  rg.SetSize(m_ImageSize);
  rg.SetIndex(idx);
  
  m_NonPipelineOutput = TOutputImage::New();
  m_NonPipelineOutput->SetBufferedRegion(rg);
  m_NonPipelineOutput->SetLargestPossibleRegion(rg);
  m_NonPipelineOutput->SetRequestedRegion(rg);
  m_NonPipelineOutput->Allocate();
  FillRegionSequential<typename TOutputImage::PixelType, 3>(m_NonPipelineOutput);
}


template <class TInputImage>
class ITK_EXPORT NullImageWriter: public WriteImage<TInputImage>
{
public:
  typedef NullImageWriter Self;
  typedef SmartPointer<Self> Pointer;
  itkNewMacro(Self);
protected:
  void WriteData() {};
  void NonPipelineExecute() {};
};

template <class TInputImage, class TOutputImage>
class ITK_EXPORT UUImageToImageFilterDriver
{
public:
  UUImageToImageFilterDriver() {};
  void SetFilter(FilterImageToImage<TInputImage, TOutputImage> *p)
  {
    m_Filter = p;
  }
  void Execute();
  void SetImageSize(const typename TInputImage::Size s) {m_ImageSize = s;}
  void NonPipelineExecute();
private:
  FilterImageToImage<TInputImage, TOutputImage> *m_Filter;
  typename TInputImage::Size m_ImageSize;
};

template <class TInputImage, class TOutputImage>
void
UUImageToImageFilterDriver<TInputImage, TOutputImage>
::NonPipelineExecute()
{
  NullImageSource< TInputImage >::Pointer source = NullImageSource<
    TInputImage >::New();
  source->SetImageSize(m_ImageSize);
  source->NonPipelineExecute();
  m_Filter->SetInput(source->GetNonPipelineOutput());

  typename TOutputImage::Pointer output = TOutputImage::New();
  output->SetRequestedRegion(
                 source->GetNonPipelineOutput()->GetRequestedRegion());
  output->SetLargestPossibleRegion(
               source->GetNonPipelineOutput()->GetLargestPossibleRegion());
  m_Filter->SetOutput(output);
  m_Filter->Update();  
}

template <class TInputImage, class TOutputImage>
void
UUImageToImageFilterDriver<TInputImage, TOutputImage>
::Execute()
{
    NullImageSource< TInputImage >::Pointer source = NullImageSource<
      TInputImage >::New();

    //  RandomImageSource< TInputImage >::Pointer source
    //        = RandomImageSource<TInputImage>::New();
   NullImageWriter< TOutputImage >::Pointer sink = NullImageWriter<
    TOutputImage >::New();

  source->SetImageSize(m_ImageSize);
  m_Filter->SetInput(source->GetOutput());
  sink->SetInput(m_Filter->GetOutput());
  std::clock_t start = std::clock();
  sink->Write();
  std::clock_t stop = std::clock();
  std::cout << "Approximate filtering time was " << stop-start
            << " clock cycles"  << std::endl;
}
}
#endif
