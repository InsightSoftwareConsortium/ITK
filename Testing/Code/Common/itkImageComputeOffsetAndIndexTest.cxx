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

template <class TImage>
void ComputeOffset(TImage *image, unsigned int count, unsigned int repeat)
{
  typename TImage::OffsetValueType offset;
  typename TImage::IndexType index;
  typename TImage::OffsetType indexIncr;
  indexIncr.Fill(1);

  for (unsigned j = 0; j < repeat; j++)
    {
    index.Fill(0);
    for (unsigned int i = 0; i < count; i++)
      {
      offset = image->ComputeOffset (index);
      index += indexIncr;
      }
    }
  std::cout << "Last offset: " << offset << std::endl;
}

template <class TImage>
void ComputeFastOffset(TImage *image, unsigned int count, unsigned int repeat)
{
  typename TImage::OffsetValueType offset;
  typename TImage::IndexType index;
  typename TImage::OffsetType indexIncr;
  indexIncr.Fill(1);

  const typename TImage::IndexType &bufferedRegionIndex = image->GetBufferedRegion().GetIndex();
  const typename TImage::OffsetValueType *offsetTable = image->GetOffsetTable();
  
  for (unsigned j = 0; j < repeat; j++)
    {
    index.Fill(0);
    for (unsigned int i = 0; i < count; i++)
      {
      itk::ImageHelper<TImage::ImageDimension,TImage::ImageDimension>::ComputeOffset(bufferedRegionIndex,
                                                                                    index,
                                                                                    offsetTable,
                                                                                    offset);
      index += indexIncr;
      }
    }
  std::cout << "Last offset: " << offset << std::endl;
}

int itkImageComputeOffsetAndIndexTest(int, char* [] )
{

  itk::TimeProbesCollectorBase   collector;

#define TRY_FAST_INDEX(dim) \
  { \
  typedef char PixelType; \
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
  unsigned int repeat = 1000; \
  if (dim > 2 ) repeat = 100; \
  if (dim > 3) repeat = 10; \
  if (dim > 4) repeat = 1; \
  ComputeFastIndex<ImageType>(myImage, totalSize, repeat); \
  collector.Stop("ComputeIndexFast " #dim"D"); \
  }
#define TRY_INDEX(dim) \
  { \
  typedef char PixelType; \
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
  unsigned int repeat = 1000; \
  if (dim > 2 ) repeat = 100; \
  if (dim > 3) repeat = 10; \
  if (dim > 4) repeat = 1; \
  ComputeIndex<ImageType>(myImage, totalSize, repeat); \
  collector.Stop("ComputeIndex " #dim"D"); \
  }

#define TRY_FAST_OFFSET(dim) \
  { \
  typedef char PixelType; \
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
  collector.Start("ComputeOffsetFast " #dim"D"); \
  unsigned int repeat = 1; \
  if (dim < 4 ) repeat = 100; \
  unsigned int totalSize = 1; \
  for (unsigned int i = 0; i < dim; i++) totalSize *= size[i]; \
  ComputeFastOffset<ImageType>(myImage, size[0], totalSize*repeat); \
  collector.Stop("ComputeOffsetFast " #dim"D"); \
  }
#define TRY_OFFSET(dim) \
  { \
  typedef char PixelType; \
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
  collector.Start("ComputeOffset " #dim"D"); \
  unsigned int repeat = 1; \
  if (dim < 4 ) repeat = 100;                  \
  unsigned int totalSize = 1; \
  for (unsigned int i = 0; i < dim; i++) totalSize *= size[i]; \
  ComputeOffset<ImageType>(myImage, size[0], totalSize*repeat); \
  collector.Stop("ComputeOffset " #dim"D"); \
  }

  TRY_INDEX(1);
  TRY_INDEX(2);
  TRY_INDEX(3);
  TRY_INDEX(4);
  TRY_FAST_INDEX(1);
  TRY_FAST_INDEX(2);
  TRY_FAST_INDEX(3);
  TRY_FAST_INDEX(4);

  TRY_OFFSET(1);
  TRY_OFFSET(2);
  TRY_OFFSET(3);
  TRY_OFFSET(4);
  TRY_FAST_OFFSET(1);
  TRY_FAST_OFFSET(2);
  TRY_FAST_OFFSET(3);
  TRY_FAST_OFFSET(4);

  // Print the results of the time probes
  collector.Report();

  return EXIT_SUCCESS;
}
