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
/**
 * This file contains classes that can be used to drive an image-to-image
 * type filter in or out of an itk pipeline for testing purposes.
 */

#ifndef itkNullImageToImageFilterDriver_hxx
#define itkNullImageToImageFilterDriver_hxx

#include "itkPixelTraits.h"
#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkIndex.h"
#include <iostream>
extern "C" {
#include <ctime>
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

template <typename TInputImage, typename TOutputImage>
class NullImageToImageFilterDriver
{
public:
  NullImageToImageFilterDriver():
    m_Filter( ITK_NULLPTR )
    {};

  typedef typename TInputImage::SizeType  ImageSizeType;
  typedef typename TInputImage::PixelType InputPixelType;
  enum {InputPixelDimension=PixelTraits<InputPixelType>::Dimension};

  /**
   * Set the image-to-image filter to drive.
   */
  void SetFilter(ImageToImageFilter<TInputImage, TOutputImage> * filter)
  {    m_Filter = filter;   }

  /**
   * Set the size of the input and output image.
   */
  void SetImageSize(const ImageSizeType size)
    { m_ImageSize = size; }

  /**
   * Drive the filter without using the itk pipeline.
   */
  void Execute();

protected:
  struct DispatchBase {};
  template<unsigned int VDimension>
  struct Dispatch : public DispatchBase {};

  void InitializePixel(InputPixelType &pixel);
  void InitializePixel(const DispatchBase&, InputPixelType &pixel);
  void InitializePixel(const Dispatch<1>&, InputPixelType &pixel);

private:
  ImageToImageFilter<TInputImage, TOutputImage> *m_Filter;
  ImageSizeType                                  m_ImageSize;
};


template <typename TInputImage, typename TOutputImage>
void
NullImageToImageFilterDriver<TInputImage, TOutputImage>
::InitializePixel(InputPixelType &pixel)
{
  this->InitializePixel(Dispatch<InputPixelDimension>(), pixel);
}

template <typename TInputImage, typename TOutputImage>
void
NullImageToImageFilterDriver<TInputImage, TOutputImage>
::InitializePixel(const DispatchBase &, InputPixelType &pixel)
{
  for (unsigned int i=0; i < InputPixelDimension; ++i)
    {
    pixel[i] = NumericTraits<typename PixelTraits<InputPixelType>::ValueType>::ZeroValue();
    }
}

template <typename TInputImage, typename TOutputImage>
void
NullImageToImageFilterDriver<TInputImage, TOutputImage>
::InitializePixel(const Dispatch<1> &, InputPixelType &pixel)
{
  pixel = NumericTraits<InputPixelType>::ZeroValue();
}

/**
 *  Drive the filter without using the itk pipeline
 */
template <typename TInputImage, typename TOutputImage>
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

  // Construct a pixel to fill the image
  InputPixelType pixel;
  this->InitializePixel(pixel);
  ip->FillBuffer(pixel);

  // Setup the filter
  m_Filter->SetInput(ip);

  // print out the output object so we can see it modified times and regions
  //  std::cout << "Output object before filter execution" << std::endl
  //            << m_Filter->GetOutput() << std::endl;

  typedef ImageToImageFilter<TInputImage, TOutputImage> ImageFilterType;
  typename ImageFilterType::Pointer sourceBefore =
     dynamic_cast< ImageFilterType * >( m_Filter->GetOutput()->GetSource().GetPointer() );

  // Execute the filter
  clock_t start = ::clock();
  m_Filter->UpdateLargestPossibleRegion();
  clock_t stop = ::clock();

  // print out the output object so we can see it modified times and regions
  std::cout << "Output object after filter execution" << std::endl
            << m_Filter->GetOutput() << std::endl;

  typename ImageFilterType::Pointer sourceAfter =
    dynamic_cast< ImageFilterType * >( m_Filter->GetOutput()->GetSource().GetPointer() );

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
