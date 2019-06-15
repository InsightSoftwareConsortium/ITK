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

#include "itkProxTVImageFilter.h"

#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include "itkImageScanlineIterator.h"

template <typename TImageType>
typename TImageType::Pointer
Create2DImage()
{
  using ImageType = TImageType;
  typename ImageType::SizeType size;
  size.Fill(64);
  size[1] = 50;
  typename ImageType::Pointer image = ImageType::New();
  image->SetRegions(size);
  image->Allocate();
  image->FillBuffer(1);
  // Set a band
  using ImageIterator = itk::ImageScanlineIterator<ImageType>;
  typename ImageType::RegionType bandRegion;
  bandRegion.SetIndex({ 0, 22 });
  bandRegion.SetSize({ 64, 6 });
  ImageIterator iter(image, bandRegion);

  while (!iter.IsAtEnd())
  {
    while (!iter.IsAtEndOfLine())
    {
      iter.Set(255);
      ++iter;
    }
    iter.NextLine();
  }
  return image;
}

template <typename TImageType>
typename TImageType::Pointer
Create3DImage()
{
  using ImageType = TImageType;
  typename ImageType::SizeType size;
  size.Fill(32);
  size[1] = 16;
  size[2] = 8;
  typename ImageType::Pointer image = ImageType::New();
  image->SetRegions(size);
  image->Allocate();
  image->FillBuffer(1);
  // Set a band
  using ImageIterator = itk::ImageScanlineIterator<ImageType>;
  typename ImageType::RegionType bandRegion;
  bandRegion.SetIndex({ 14, 0, 0 });
  bandRegion.SetSize({ 6, 16, 8 });
  ImageIterator iter(image, bandRegion);

  while (!iter.IsAtEnd())
  {
    while (!iter.IsAtEndOfLine())
    {
      iter.Set(255);
      ++iter;
    }
    iter.NextLine();
  }
  return image;
}

int
itkProxTVImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImage2D";
    std::cerr << " outputImage3D";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  const char *       outputImageFileName = argv[1];
  const char *       outputImage3DFileName = argv[2];
  const unsigned int Dimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using FilterType = itk::ProxTVImageFilter<ImageType, ImageType>;
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, ProxTVImageFilter, ImageToImageFilter);

  auto image = Create2DImage<ImageType>();
  filter->SetInput(image);
  filter->Update();

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(outputImageFileName);
  writer->SetInput(filter->GetOutput());
  writer->SetUseCompression(true);
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }

  /************ 3D *************/
  using Image3DType = itk::Image<PixelType, 3>;
  using Filter3DType = itk::ProxTVImageFilter<Image3DType, Image3DType>;
  Filter3DType::Pointer filter3D = Filter3DType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter3D, ProxTVImageFilter, ImageToImageFilter);
  auto image3D = Create3DImage<Image3DType>();
  filter3D->SetInput(image3D);
  filter3D->Update();

  using Writer3DType = itk::ImageFileWriter<Image3DType>;
  Writer3DType::Pointer writer3d = Writer3DType::New();
  writer3d->SetFileName(outputImage3DFileName);
  writer3d->SetInput(filter3D->GetOutput());
  writer3d->SetUseCompression(true);
  try
  {
    writer3d->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
