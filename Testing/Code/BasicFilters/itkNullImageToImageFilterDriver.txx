/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNullImageToImageFilterDriver.txx
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
#include "itkImageToImageFilter.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include "itkIndex.h"
extern "C" {
#include "time.h"
}

namespace itk { 

/**
 * \class NullImageToImageFilterDriver
 * \brief Drives an image-to-image type itk process object with null inputs and 
 *  null outputs.
 *
 * Provides a non-pipeline framework for testing image-to-image
 * filters. Allocates an input image and sets up an output image, then calls
 * Update on the filter.  Times the execution of the filter.
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT NullImageToImageFilterDriver
{
public:
  NullImageToImageFilterDriver() {};

  /**
   * Set the image-to-image filter to drive.
   */
  void SetFilter(ImageToImageFilter<TInputImage, TOutputImage> *p)
  {    m_Filter = p;   }

  /**
   * Set the size of the input and output image.
   */
  void SetImageSize(const typename TInputImage::SizeType s) 
    { m_ImageSize = s; }

  /**
   * Drive the filter without using the itk pipeline.
   */
  void Execute();
  
private:
  ImageToImageFilter<TInputImage, TOutputImage> *m_Filter;
  typename TInputImage::SizeType m_ImageSize;
};

/**
 *  Drive the filter without using the itk pipeline
 */
template <class TInputImage, class TOutputImage>
void
NullImageToImageFilterDriver<TInputImage, TOutputImage>
::Execute()
{
  enum { ImageDimension = TInputImage::ImageDimension };

  // Set up input and output images
  typename TOutputImage::Pointer op = TOutputImage::New();
  typename TInputImage::Pointer ip = TInputImage::New();
  typename TOutputImage::IndexType index;
  typename TOutputImage::RegionType region;
  
  for (int i = 0; i < ImageDimension; ++i) index[i] = 0;
  region.SetSize( m_ImageSize );
  region.SetIndex( index);
  op->SetLargestPossibleRegion( region );
  op->SetBufferedRegion(region);
  op->SetRequestedRegion(region);
  ip->SetLargestPossibleRegion( region );
  ip->SetBufferedRegion(region);
  ip->SetRequestedRegion(region);

  // Execute the filter
  ip->Allocate();
  m_Filter->SetInput(ip);
  m_Filter->SetOutput(op);

  clock_t start = ::clock();
  m_Filter->Update();
  clock_t stop = ::clock();
  std::cout << "Execution time was approximately " << (stop - start)
            << " clock cycles." << std::endl;
}

}  // end namespace itk
#endif
