/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNullImageToImageFilterDriver.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

  typedef typename TInputImage::SizeType ImageSizeType;

  /**
   * Set the image-to-image filter to drive.
   */
  void SetFilter(ImageToImageFilter<TInputImage, TOutputImage> *p)
  {    m_Filter = p;   }

  /**
   * Set the size of the input and output image.
   */
  void SetImageSize(const ImageSizeType s) 
    { m_ImageSize = s; }

  /**
   * Drive the filter without using the itk pipeline.
   */
  void Execute();
  
private:
  ImageToImageFilter<TInputImage, TOutputImage> *m_Filter;
  ImageSizeType m_ImageSize;
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

  // Set up input images
  typename TInputImage::Pointer ip = TInputImage::New();
  typename TOutputImage::IndexType index;
  typename TOutputImage::RegionType region;
  
  for (unsigned int i = 0; i < ImageDimension; ++i) index[i] = 0;
  region.SetSize( m_ImageSize );
  region.SetIndex( index);

  // Allocate the input
  ip->SetLargestPossibleRegion( region );
  ip->SetBufferedRegion(region);
  ip->SetRequestedRegion(region);
  ip->Allocate();

  // Setup the filter
  m_Filter->SetInput(ip);

  // print out the ouput object so we can see it modified times and regions
  std::cout << "Output object before filter execution" << std::endl
            << m_Filter->GetOutput() << std::endl;

  ImageToImageFilter<TInputImage, TOutputImage>::Pointer
    sourceBefore = (ImageToImageFilter<TInputImage, TOutputImage> *) m_Filter->GetOutput()->GetSource().GetPointer();
  
  // Execute the filter
  clock_t start = ::clock();
  m_Filter->UpdateLargestPossibleRegion();
  clock_t stop = ::clock();

  // print out the ouput object so we can see it modified times and regions
  std::cout << "Output object after filter execution" << std::endl
            << m_Filter->GetOutput() << std::endl;
  
  ImageToImageFilter<TInputImage, TOutputImage>::Pointer
    sourceAfter = (ImageToImageFilter<TInputImage, TOutputImage> *) m_Filter->GetOutput()->GetSource().GetPointer();

  std::cout << sourceBefore.GetPointer() << ", " << sourceAfter.GetPointer() << std::endl;
  if (sourceBefore.GetPointer() != sourceAfter.GetPointer())
    {
    std::cout << std::endl << "Pipeline corrupt, filter output source different after execution." << std::endl;
    }
  else
    {
    std::cout << std::endl << "Pipeline intact" << std::endl;
    }
  
  std::cout << "Execution time was approximately " << (stop - start)
            << " clock cycles." << std::endl;
}

}  // end namespace itk
#endif
