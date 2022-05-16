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
// The example tests the class itk::Statistics::ImageToListSampleAdaptor.


#include "itkImageToListSampleAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"

int
itkImageToListSampleAdaptorTest2(int, char *[])
{
  constexpr unsigned int MeasurementVectorSize = 8;
  using MeasurementComponentType = unsigned long;
  using PixelType = itk::FixedArray<MeasurementComponentType, MeasurementVectorSize>;

  constexpr unsigned int ImageDimension = 3;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  auto image = ImageType::New();

  ImageType::IndexType start;
  ImageType::SizeType  size;

  start.Fill(0);
  size.Fill(10);

  ImageType::RegionType region(start, size);
  image->SetRegions(region);
  image->Allocate();
  using IteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;
  IteratorType it(image, region);
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    PixelType value;
    for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
    {
      value[i] = i + it.GetIndex()[0];
    }
    it.Set(value);
    ++it;
  }

  // define an adaptor type
  using ImageToListSampleAdaptorType = itk::Statistics::ImageToListSampleAdaptor<ImageType>;
  auto adaptor = ImageToListSampleAdaptorType::New();

  adaptor->SetImage(image);

  ImageType::IndexType index;
  ImageType::PixelType pixel;

  ImageToListSampleAdaptorType::InstanceIdentifier id;

  for (unsigned int i = 0; i < size[2]; ++i)
    for (unsigned int j = 0; j < size[1]; ++j)
      for (unsigned int k = 0; k < size[0]; ++k)
      {
        index[0] = k;
        index[1] = j;
        index[2] = i;

        pixel = image->GetPixel(index);
        id = image->ComputeOffset(index);
        for (unsigned int m = 0; m < adaptor->GetMeasurementVectorSize(); ++m)
        {
          if (adaptor->GetMeasurementVector(id)[m] != pixel[m])
          {
            std::cerr << "Error in pixel value accessed using the adaptor" << std::endl;
            return EXIT_FAILURE;
          }
        }
      }

  //
  // Exercise the iterators
  //
  using ConstIterator = ImageToListSampleAdaptorType::ConstIterator;

  ConstIterator itrBegin = adaptor->Begin();
  ConstIterator itrEnd = adaptor->End();

  ConstIterator citr = itrBegin;

  double frequencySum = 0.0;

  while (citr != itrEnd)
  {
    frequencySum += citr.GetFrequency();
    ++citr;
  }

  if (citr == itrEnd)
  {
    std::cout << "Reached the end successfully" << std::endl;
  }

  std::cout << "Frequency Sum = " << frequencySum << std::endl;

  using VariableLengthPixelType = itk::VariableLengthVector<float>;

  using VariableLengthImageType = itk::Image<VariableLengthPixelType, ImageDimension>;

  constexpr unsigned int vMeasurementVectorSize = 4;

  auto vImage = VariableLengthImageType::New();

  VariableLengthImageType::IndexType vStart;
  VariableLengthImageType::SizeType  vSize;

  vStart.Fill(0);
  vSize.Fill(10);

  VariableLengthImageType::RegionType vRegion(vStart, vSize);
  vImage->SetRegions(vRegion);
  vImage->Allocate();

  using VariableIteratorType = itk::ImageRegionIteratorWithIndex<VariableLengthImageType>;

  VariableIteratorType ivt(vImage, vRegion);

  ivt.GoToBegin();

  while (!ivt.IsAtEnd())
  {
    VariableLengthPixelType value(vMeasurementVectorSize);

    for (unsigned int i = 0; i < vMeasurementVectorSize; ++i)
    {
      value[i] = i + ivt.GetIndex()[0];
    }
    ivt.Set(value);
    ++ivt;
  }

  // define an adaptor for the image with variable length vector type
  using VariableLengthImageToListSampleAdaptorType = itk::Statistics::ImageToListSampleAdaptor<VariableLengthImageType>;

  auto vAdaptor = VariableLengthImageToListSampleAdaptorType::New();

  vAdaptor->SetImage(vImage);

  VariableLengthImageType::IndexType vIndex;
  VariableLengthImageType::PixelType vPixel;

  VariableLengthImageToListSampleAdaptorType::InstanceIdentifier vId;

  for (unsigned int i = 0; i < size[2]; ++i)
  {
    for (unsigned int j = 0; j < size[1]; ++j)
    {
      for (unsigned int k = 0; k < size[0]; ++k)
      {
        vIndex[0] = k;
        vIndex[1] = j;
        vIndex[2] = i;

        vPixel = vImage->GetPixel(vIndex);
        vId = vImage->ComputeOffset(vIndex);
        for (unsigned int m = 0; m < vAdaptor->GetMeasurementVectorSize(); ++m)
        {
          if (itk::Math::NotExactlyEquals(vAdaptor->GetMeasurementVector(vId)[m], vPixel[m]))
          {
            std::cerr << "Error in vPixel value accessed using the vAdaptor" << std::endl;
            return EXIT_FAILURE;
          }
        }
      }
    }
  }


  //
  // Test an RGB image
  //
  using RGBPixelType = itk::RGBPixel<unsigned char>;

  unsigned int rgbMeasurementVectorSize = 3;

  using RGBImageType = itk::Image<RGBPixelType, ImageDimension>;

  auto rgbImage = RGBImageType::New();

  RGBImageType::IndexType rgbStart;
  RGBImageType::SizeType  rgbSize;

  rgbStart.Fill(0);
  rgbSize.Fill(10);

  RGBImageType::RegionType rgbRegion(rgbStart, rgbSize);
  rgbImage->SetRegions(rgbRegion);
  rgbImage->Allocate();

  using RGBIteratorType = itk::ImageRegionIteratorWithIndex<RGBImageType>;

  RGBIteratorType rgbt(rgbImage, rgbRegion);

  rgbt.GoToBegin();

  while (!rgbt.IsAtEnd())
  {
    RGBPixelType value;

    for (unsigned int i = 0; i < rgbMeasurementVectorSize; ++i)
    {
      value[i] = i + rgbt.GetIndex()[0];
    }
    rgbt.Set(value);
    ++rgbt;
  }

  // define an adaptor for the image with variable length vector type
  using RGBImageToListSampleAdaptorType = itk::Statistics::ImageToListSampleAdaptor<RGBImageType>;

  auto rgbAdaptor = RGBImageToListSampleAdaptorType::New();

  rgbAdaptor->SetImage(rgbImage);

  RGBImageType::IndexType rgbIndex;
  RGBImageType::PixelType rgbPixel;

  RGBImageToListSampleAdaptorType::InstanceIdentifier rgbId;

  for (unsigned int i = 0; i < size[2]; ++i)
  {
    for (unsigned int j = 0; j < size[1]; ++j)
    {
      for (unsigned int k = 0; k < size[0]; ++k)
      {
        rgbIndex[0] = k;
        rgbIndex[1] = j;
        rgbIndex[2] = i;

        rgbPixel = rgbImage->GetPixel(rgbIndex);
        rgbId = rgbImage->ComputeOffset(rgbIndex);
        for (unsigned int m = 0; m < rgbAdaptor->GetMeasurementVectorSize(); ++m)
        {
          if (rgbAdaptor->GetMeasurementVector(rgbId)[m] != rgbPixel[m])
          {
            std::cerr << "Error in rgbPixel value accessed using the rgbAdaptor" << std::endl;
            return EXIT_FAILURE;
          }
        }
      }
    }
  }

  std::cerr << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
