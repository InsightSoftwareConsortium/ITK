/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNullImageToImageFilterDriver.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/

/**
 * This file contains classes that can be used to drive an image-to-image
 * type filter in or out of an itk pipeline for testing purposes.
 */

#ifndef __itkNullImageToImageFilterDriver_h
#define __itkNullImageToImageFilterDriver_h

#include <iostream>
#include "itkImage.h"
#include "itkWriteImage.h"
#include "itkImageSource.h"
#include "itkFilterImageToImage.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkIndex.h"
#include "itkImageRegionIterator.h"
extern "C" {
#include "time.h"
}

/**
 * Fills an itk::image with a sequence of integer values.
 */
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

/**
 * \class NullImageSource
 * \brief itk::Image source that outputs an image of a specified data type,
 * size, and dimension.
 * 
 * itk::Image source that outputs an image of a specified data type, size, and
 * dimension.  The image is filled with a non-random, but otherwise arbitrary
 * sequence of values. 
 *
 * This object can be executed by the pipeline or be set up and executed
 * "manually".
 */
template <class TOutputImage>
class ITK_EXPORT NullImageSource: public ImageSource<TOutputImage>
{
public:
  /**
   * Standard typedefs.
   */
  typedef NullImageSource Self;
  typedef SmartPointer<Self> Pointer;
  typedef ImageSource<TOutputImage> Superclass;
  
  /**
   * Provides for creation through the itk object factory.
   */
  itkNewMacro(Self);

  /**
   * Sets the image size.
   */
  void SetImageSize(const typename TOutputImage::SizeType s) 
    {m_ImageSize = s;}

  /**
   * Returns an image pointer that represents the output of a non-pipeline
   * execution of this filter
   */
  typename TOutputImage::Pointer GetNonPipelineOutput()
  { return  m_NonPipelineOutput; }

  /**
   * Manually execute this process object.  Requires that the NonPipelineOutput 
   * member variable be set.
   */
  void NonPipelineExecute();
  
protected:
  virtual void GenerateData();
  virtual void GenerateOutputInformation();
  
private:
  typename TOutputImage::SizeType m_ImageSize;
  typename TOutputImage::Pointer  m_NonPipelineOutput;
};


/**
 * Method required by the itk pipeline to set up the output image.  
 */  
template <class TOutputImage>
void
NullImageSource<TOutputImage>::GenerateOutputInformation()
{
  TOutputImage *output;
  typename TOutputImage::IndexType index = {0};
  output = this->GetOutput(0);
  typename TOutputImage::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize( m_ImageSize );
  largestPossibleRegion.SetIndex( index);
  output->SetLargestPossibleRegion( largestPossibleRegion );
}

/**
 * Execution method required by the itk pipeline.
 */
template <class TOutputImage>
void
NullImageSource<TOutputImage>::GenerateData()
{
  std::cout << "Generate data from NUllImageSource" << std::endl;
  typename TOutputImage::Pointer image = this->GetOutput(0);
  image->SetBufferedRegion( image->GetRequestedRegion() );
  image->Allocate();
  FillRegionSequential<typename TOutputImage::PixelType,
    TOutputImage::ImageDimension>(image);
}

/**
 * Non-pipeline method for execution of the process object.
 */
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
  FillRegionSequential<typename TOutputImage::PixelType,
    TOutputImage::ImageDimension> (m_NonPipelineOutput);
}


/**
 * \class NullImageWriter
 * \brief itk::Image writer object that does nothing.
 * This itk::Image writer object does nothing.  It can be used to drive an itk
 * pipeline where the output is not important.
 */
template <class TInputImage>
class ITK_EXPORT NullImageWriter: public WriteImage<TInputImage>
{
public:
  /**
   * Standard itk typedefs.
   */
  typedef NullImageWriter Self;
  typedef SmartPointer<Self> Pointer;

  /**
   * Object factory creation method.
   */
  itkNewMacro(Self);
  
protected:
  void WriteData() {};
  void NonPipelineExecute() {};
};


/**
 * \class NullImageToImageFilterDriver
 * \brief Drives an image-to-image type itk process object with null inputs and 
 *  null outputs.
 *
 * Uses NullImageSource and NullImageWriter as inputs and outputs to a
 * user-supplied itk::FilterImageToImage.  A user can set the size, data-type,
 * and dimensionality of the image.  Reports total filter execution time to std 
 * out.
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT NullImageToImageFilterDriver
{
public:
  NullImageToImageFilterDriver() {};

  /**
   * Set the image-to-image filter to drive.
   */
  void SetFilter(FilterImageToImage<TInputImage, TOutputImage> *p)
  {
    m_Filter = p;
  }

  /**
   * Cause the pipeline to execute by updating the writer.
   */
  void Execute();

  /**
   * Set the size of the input image.
   */
  void SetImageSize(const typename TInputImage::SizeType s) 
    {m_ImageSize = s;}

  /**
   * Drive the filter without using the itk pipeline.
   */
  void NonPipelineExecute();
  
private:
  FilterImageToImage<TInputImage, TOutputImage> *m_Filter;
  typename TInputImage::SizeType m_ImageSize;
};

/**
 *  Drive the filter without using the itk pipeline
 */
template <class TInputImage, class TOutputImage>
void
NullImageToImageFilterDriver<TInputImage, TOutputImage>
::NonPipelineExecute()
{
  // Isolate from pipeline bugs and instability of developing pipeline
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


/**
 * Drive the filter using the itk pipeline.
 */
template <class TInputImage, class TOutputImage>
void
NullImageToImageFilterDriver<TInputImage, TOutputImage>
::Execute()
{
  NullImageSource< TInputImage >::Pointer source = NullImageSource<
    TInputImage >::New();
  
  NullImageWriter< TOutputImage >::Pointer sink = NullImageWriter<
    TOutputImage >::New();
  
  source->SetImageSize(m_ImageSize);
  m_Filter->SetInput(source->GetOutput());
  sink->SetInput(m_Filter->GetOutput());
  //  std::clock_t start = std::clock();
  sink->Write();
  //  std::clock_t stop = std::clock();
  //  std::cout << "Approximate filtering time was " << stop-start
  //            << " clock cycles"  << std::endl;
}

}  // end namespace itk
#endif
