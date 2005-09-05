/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageComputeOffsetAndIndexTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>

#include "itkImage.h"
#include "itkImageHelper.h"
#include "itkTimeProbesCollectorBase.h"

template <class TImage>
void ComputeIndex(TImage *image, unsigned int count, unsigned int repeat)
{
  typename TImage::IndexType index;
  for (unsigned j = 0; j < repeat; j++)
    {
    for (unsigned int i = 0; i < count; i++)
      {
      index = image->ComputeIndex (i);
      }
    }
  std::cout << "Last index: " << index << std::endl;
}

template <class TImage>
void ComputeFastIndex(TImage *image, unsigned int count, unsigned int repeat)
{
  typename TImage::IndexType index;
  const typename TImage::IndexType &bufferedRegionIndex = image->GetBufferedRegion().GetIndex();
  const typename TImage::OffsetValueType *offsetTable = image->GetOffsetTable();
  
  for (unsigned j = 0; j < repeat; j++)
    {
    for (long i = 0; i < count; i++)
      {
      itk::ImageHelper<TImage::ImageDimension,TImage::ImageDimension>::ComputeIndex(bufferedRegionIndex,
                                                                                    i,
                                                                                    offsetTable,
                                                                                    index);
      }
    }
  std::cout << "Last index: " << index << std::endl;
}

int itkImageComputeOffsetAndIndexTest(int, char* [] )
{

  itk::TimeProbesCollectorBase   collector;

#define TRYFAST(dim) \
  { \
  typedef float PixelType; \
  typedef itk::Image< PixelType, dim> ImageType; \
  ImageType::Pointer myImage = ImageType::New(); \
  ImageType::SizeType size; \
  ImageType::IndexType index; \
  ImageType::RegionType region; \
  size.Fill(50); \
  index.Fill(0); \
  region.SetSize(size); \
  region.SetIndex(index); \
  myImage->SetLargestPossibleRegion( region ); \
  myImage->SetBufferedRegion( region ); \
  myImage->SetRequestedRegion( region ); \
  myImage->Allocate(); \
  collector.Start("ComputeIndexFast " #dim"D"); \
  unsigned int totalSize = 1; \
  for (unsigned int i = 0; i < dim; i++) totalSize *= size[i]; \
  unsigned int repeat = 10; \
  if (dim > 4) repeat = 1; \
  ComputeFastIndex<ImageType>(myImage, totalSize, repeat); \
  collector.Stop("ComputeIndexFast " #dim"D"); \
  }
#define TRY(dim) \
  { \
  typedef float PixelType; \
  typedef itk::Image< PixelType, dim> ImageType; \
  ImageType::Pointer myImage = ImageType::New(); \
  ImageType::SizeType size; \
  ImageType::IndexType index; \
  ImageType::RegionType region; \
  size.Fill(50); \
  index.Fill(0); \
  region.SetSize(size); \
  region.SetIndex(index); \
  myImage->SetLargestPossibleRegion( region ); \
  myImage->SetBufferedRegion( region ); \
  myImage->SetRequestedRegion( region ); \
  myImage->Allocate(); \
  collector.Start("ComputeIndex " #dim"D"); \
  unsigned int totalSize = 1; \
  for (unsigned int i = 0; i < dim; i++) totalSize *= size[i]; \
  unsigned int repeat = 10; \
  if (dim > 4) repeat = 1; \
  ComputeIndex<ImageType>(myImage, totalSize, repeat); \
  collector.Stop("ComputeIndex " #dim"D"); \
  }
  TRY(1);
  TRY(2);
  TRY(3);
  TRY(4);
  TRY(5);
  TRYFAST(1);
  TRYFAST(2);
  TRYFAST(3);
  TRYFAST(4);
  TRYFAST(5);

  // Print the results of the time probes
  collector.Report();

  return EXIT_SUCCESS;
}
