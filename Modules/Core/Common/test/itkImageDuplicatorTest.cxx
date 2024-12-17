/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkMath.h"
#include "itkImageDuplicator.h"
#include "itkRGBPixel.h"
#include "itkVectorImage.h"
#include "itkShiftScaleImageFilter.h"
#include "itkTestingMacros.h"

int
itkImageDuplicatorTest(int, char *[])
{
  using ImageType = itk::Image<float, 3>;
  ImageType::RegionType region;
  ImageType::SizeType   size;
  size[0] = 10;
  size[1] = 20;
  size[2] = 30;
  constexpr ImageType::IndexType index{};
  region.SetSize(size);
  region.SetIndex(index);

  {
    /** Create an image image */
    std::cout << "Creating simulated image: ";
    auto m_Image = ImageType::New();
    m_Image->SetRegions(region);
    m_Image->AllocateInitialized();

    itk::ImageRegionIterator<ImageType> it(m_Image, region);
    it.GoToBegin();

    float i = 0;
    while (!it.IsAtEnd())
    {
      it.Set(i);
      i++;
      ++it;
    }

    using ShiftType = itk::ShiftScaleImageFilter<ImageType, ImageType>;
    auto shift = ShiftType::New();
    shift->SetInput(m_Image);
    shift->SetShift(0.0);
    shift->SetScale(1.0);
    shift->Update(); // need to update before duplicator can run

    std::cout << "[DONE]" << std::endl;

    // Test the duplicator
    std::cout << "Testing duplicator with float images: ";
    using DuplicatorType = itk::ImageDuplicator<ImageType>;
    auto duplicator = DuplicatorType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(duplicator, ImageDuplicator, Object);


    duplicator->SetInputImage(shift->GetOutput());
    duplicator->Update();
    ImageType::Pointer ImageCopy = duplicator->GetOutput();


    itk::ImageRegionIterator<ImageType> it2(ImageCopy, ImageCopy->GetLargestPossibleRegion());
    it2.GoToBegin();

    i = 0;
    while (!it2.IsAtEnd())
    {
      if (itk::Math::NotAlmostEquals(it2.Get(), i))
      {
        std::cout << "Error: Pixel value mismatched: " << it2.Get() << " vs. " << i << std::endl;
        return EXIT_FAILURE;
      }
      i++;
      ++it2;
    }

    std::cout << "[DONE]" << std::endl;

    /** Test duplicator after modifying the bulk data of the input */
    std::cout << "Modifying input, testing duplicator again: ";
    shift->SetShift(1);
    shift->Update(); // need to update before duplicator
    duplicator->Update();
    ImageCopy = duplicator->GetOutput();

    itk::ImageRegionIterator<ImageType> it2b(ImageCopy, ImageCopy->GetLargestPossibleRegion());
    it2b.GoToBegin();
    i = 0;
    while (!it2b.IsAtEnd())
    {
      if (itk::Math::NotAlmostEquals(it2b.Get(), i + 1))
      {
        std::cout << "Error: Pixel value mismatched: " << it2b.Get() << " vs. " << i + 1 << std::endl;
        return EXIT_FAILURE;
      }
      i++;
      ++it2b;
    }

    std::cout << "[DONE]" << std::endl;

    /** Test duplicator after modifying the bulk data of the input */
    std::cout << "Rerunning duplicator with no changes: ";
    shift->Update(); // need to update before duplicator
    duplicator->Update();
    ImageCopy = duplicator->GetOutput();

    itk::ImageRegionIterator<ImageType> it2c(ImageCopy, ImageCopy->GetLargestPossibleRegion());
    it2c.GoToBegin();
    i = 0;
    while (!it2c.IsAtEnd())
    {
      if (itk::Math::NotAlmostEquals(it2c.Get(), i + 1))
      {
        std::cout << "Error: Pixel value mismatched: " << it2c.Get() << " vs. " << i + 1 << std::endl;
        return EXIT_FAILURE;
      }
      i++;
      ++it2c;
    }

    std::cout << "[DONE]" << std::endl;
  }

  {
    /** Create an RGB image image */
    using RGBImageType = itk::Image<itk::RGBPixel<unsigned char>, 3>;
    std::cout << "Creating simulated image: ";
    auto m_RGBImage = RGBImageType::New();
    m_RGBImage->SetRegions(region);
    m_RGBImage->Allocate();

    itk::ImageRegionIterator<RGBImageType> it3(m_RGBImage, region);
    it3.GoToBegin();

    unsigned char r = 0;
    unsigned char g = 1;
    unsigned char b = 2;

    while (!it3.IsAtEnd())
    {
      itk::RGBPixel<unsigned char> pixel;
      pixel.SetRed(r);
      pixel.SetGreen(g);
      pixel.SetBlue(b);
      it3.Set(pixel);
      r++;
      if (r == 255)
      {
        r = 0;
      }
      g++;
      if (g == 255)
      {
        g = 0;
      }
      b++;
      if (b == 255)
      {
        b = 0;
      }
      ++it3;
    }

    std::cout << "[DONE]" << std::endl;

    // Test the duplicator


    std::cout << "Testing duplicator with RGB images: ";
    using RGBDuplicatorType = itk::ImageDuplicator<RGBImageType>;
    auto RGBduplicator = RGBDuplicatorType::New();

    RGBduplicator->SetInputImage(m_RGBImage);
    RGBduplicator->Update();
    const RGBImageType::Pointer RGBImageCopy = RGBduplicator->GetOutput();


    itk::ImageRegionIterator<RGBImageType> it4(RGBImageCopy, RGBImageCopy->GetLargestPossibleRegion());
    it3.GoToBegin();

    r = 0;
    g = 1;
    b = 2;

    while (!it4.IsAtEnd())
    {

      const itk::RGBPixel<unsigned char> pixel = it4.Get();
      if (pixel.GetRed() != r)
      {
        std::cout << "Error: Pixel R value mismatched: " << static_cast<float>(pixel.GetRed()) << " vs. "
                  << static_cast<float>(r) << std::endl;
        return EXIT_FAILURE;
      }
      if (pixel.GetGreen() != g)
      {
        std::cout << "Error: Pixel G value mismatched: " << static_cast<float>(pixel.GetGreen()) << " vs. "
                  << static_cast<float>(g) << std::endl;
        return EXIT_FAILURE;
      }
      if (pixel.GetBlue() != b)
      {
        std::cout << "Error: Pixel B value mismatched: " << static_cast<float>(pixel.GetBlue()) << " vs. "
                  << static_cast<float>(b) << std::endl;
        return EXIT_FAILURE;
      }
      r++;
      if (r == 255)
      {
        r = 0;
      }
      g++;
      if (g == 255)
      {
        g = 0;
      }
      b++;
      if (b == 255)
      {
        b = 0;
      }
      ++it4;
    }

    std::cout << "[DONE]" << std::endl;
  }


  {
    constexpr unsigned int Dimension = 3;
    constexpr unsigned int VectorLength = 2 * Dimension;
    using PixelType = float;
    using VectorImageType = itk::VectorImage<PixelType, Dimension>;

    auto                                 vectorImage = VectorImageType::New();
    itk::VariableLengthVector<PixelType> f(VectorLength);
    for (unsigned int i = 0; i < VectorLength; ++i)
    {
      f[i] = i;
    }
    vectorImage->SetVectorLength(VectorLength);
    vectorImage->SetRegions(region);
    vectorImage->Allocate();
    vectorImage->FillBuffer(f);

    // Test the duplicator
    std::cout << "Testing duplicator with Vector images: ";
    using VectorDuplicatorType = itk::ImageDuplicator<VectorImageType>;
    auto Vectorduplicator = VectorDuplicatorType::New();

    Vectorduplicator->SetInputImage(vectorImage);
    Vectorduplicator->Update();
    const VectorImageType::Pointer vectorImageCopy = Vectorduplicator->GetOutput();

    itk::ImageRegionIterator<VectorImageType> it3(vectorImage, vectorImage->GetLargestPossibleRegion());
    itk::ImageRegionIterator<VectorImageType> it4(vectorImageCopy, vectorImageCopy->GetLargestPossibleRegion());
    it3.GoToBegin();
    it4.GoToBegin();

    while (!it4.IsAtEnd())
    {
      const itk::VariableLengthVector<PixelType> pixel4 = it4.Get();
      const itk::VariableLengthVector<PixelType> pixel3 = it3.Get();
      if (pixel4 != pixel3)
      {
        return EXIT_FAILURE;
      }
      ++it4;
      ++it3;
    }

    std::cout << "[DONE]" << std::endl;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
