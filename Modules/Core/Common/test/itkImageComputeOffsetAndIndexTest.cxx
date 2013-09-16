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

#include <iostream>

#include "itkImage.h"
#include "itkTimeProbesCollectorBase.h"

template <typename TImage>
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

template <typename TImage>
void ComputeFastIndex(TImage *image, unsigned int count, unsigned int repeat)
{
  typename TImage::IndexType index;
  index.Fill( 0 );
  const typename TImage::IndexType &bufferedRegionIndex = image->GetBufferedRegion().GetIndex();
  const typename TImage::OffsetValueType *offsetTable = image->GetOffsetTable();

  for (unsigned int j = 0; j < repeat; j++)
    {
    for (unsigned int i = 0; i < count; i++)
      {
      itk::ImageHelper<TImage::ImageDimension,TImage::ImageDimension>::ComputeIndex(bufferedRegionIndex,
        i,
        offsetTable,
        index);
      }
    }
  std::cout << "Last index: " << index << std::endl;
}

template <typename TImage>
void ComputeOffset(TImage *image, unsigned int count, unsigned int repeat)
{
  typename TImage::OffsetValueType offset = 0;
  typename TImage::OffsetValueType accum = 0;
  typename TImage::IndexType index;
  typename TImage::OffsetType indexIncr;
  indexIncr.Fill(1);

  for (unsigned j = 0; j < repeat; j++)
    {
    index.Fill(0);
    for (unsigned int i = 0; i < count; i++)
      {

      offset = image->ComputeOffset (index);
      accum += offset;
      index += indexIncr;
      }
    }
  std::cout << "Last offset: " << offset << ": " << accum << std::endl;
}

template <typename TImage>
void ComputeFastOffset(TImage *image, unsigned int count, unsigned int repeat)
{
  typename TImage::OffsetValueType offset = 0;
  typename TImage::OffsetValueType accum = 0;
  typename TImage::IndexType index;
  typename TImage::OffsetType indexIncr;
  indexIncr.Fill(1);

  const typename TImage::OffsetValueType *offsetTable = image->GetOffsetTable();

  for (unsigned j = 0; j < repeat; j++)
    {
    index.Fill(0);
    for (unsigned int i = 0; i < count; i++)
      {
      offset = 0;
      itk::ImageHelper<TImage::ImageDimension,TImage::ImageDimension>::ComputeOffset(image->GetBufferedRegion().GetIndex(),
                                                                                    index,
                                                                                    offsetTable,
                                                                                    offset);
      accum += offset;
      index += indexIncr;
      }
    }
  std::cout << "Last offset: " << offset << ": " << accum << std::endl;
}

int itkImageComputeOffsetAndIndexTest(int, char* [] )
{

  itk::TimeProbesCollectorBase   collector;

#define TRY_FAST_INDEX(dim) \
  { \
  typedef char                        PixelType; \
  typedef itk::Image< PixelType, dim> ImageType; \
  ImageType::Pointer myImage = ImageType::New(); \
  ImageType::SizeType   size; \
  ImageType::IndexType  index; \
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
  typedef char                        PixelType; \
  typedef itk::Image< PixelType, dim> ImageType; \
  ImageType::Pointer myImage = ImageType::New(); \
  ImageType::SizeType   size; \
  ImageType::IndexType  index; \
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
  typedef char                        PixelType; \
  typedef itk::Image< PixelType, dim> ImageType; \
  ImageType::Pointer myImage = ImageType::New(); \
  ImageType::SizeType   size; \
  ImageType::IndexType  index; \
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
  typedef char                        PixelType; \
  typedef itk::Image< PixelType, dim> ImageType; \
  ImageType::Pointer myImage = ImageType::New(); \
  ImageType::SizeType   size; \
  ImageType::IndexType  index; \
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
