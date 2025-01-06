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

#include <fstream>
#include "itkVectorConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkVectorImage.h"
#include "itkTestingMacros.h"

int
itkVectorConnectedComponentImageFilterTest(int argc, char * argv[])
{
  if (argc < 1)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  constexpr unsigned int Dimension = 2;
  using PixelType = itk::Vector<float, Dimension>;
  using OutputPixelType = unsigned long;
  using LabelPixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using LabelImageType = itk::Image<LabelPixelType, Dimension>;

  // create an image of vectors
  auto                  image = ImageType::New();
  ImageType::RegionType region;
  auto                  size = ImageType::SizeType::Filled(100);
  ImageType::IndexType  index{};

  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions(region);
  image->Allocate();

  size = region.GetSize();
  index = region.GetIndex();
  const unsigned int width = size[0];
  size[0] = width / 2;
  size[1] = width / 2;

  index[0] = 0;
  index[1] = 0;
  region.SetSize(size);
  region.SetIndex(index);
  {
    PixelType pixel;
    pixel[0] = 1;
    pixel[1] = 0;
    std::cout << "pixel: " << pixel << std::endl;
    itk::ImageRegionIterator<ImageType> it(image, region);
    std::cout << region;
    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(pixel);
      ++it;
    }
  }

  index[0] = width / 2;
  index[1] = 0;
  region.SetSize(size);
  region.SetIndex(index);
  {
    PixelType pixel;
    pixel[0] = 0;
    pixel[1] = -1;
    std::cout << "pixel: " << pixel << std::endl;
    std::cout << region;
    itk::ImageRegionIterator<ImageType> it(image, region);
    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(pixel);
      ++it;
    }
  }

  index[0] = 0;
  index[1] = width / 2;
  region.SetSize(size);
  region.SetIndex(index);
  {
    PixelType pixel;
    pixel[0] = -1;
    pixel[1] = 0;
    std::cout << "pixel: " << pixel << std::endl;
    std::cout << region;
    itk::ImageRegionIterator<ImageType> it(image, region);
    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(pixel);
      ++it;
    }
  }

  index[0] = width / 2;
  index[1] = width / 2;
  region.SetSize(size);
  region.SetIndex(index);
  {
    PixelType pixel;
    pixel[0] = 0;
    pixel[1] = 1;
    std::cout << "pixel: " << pixel << std::endl;
    std::cout << region;
    itk::ImageRegionIterator<ImageType> it(image, region);
    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(pixel);
      ++it;
    }
  }

  index[0] = width / 4;
  index[1] = width / 4;
  size[0] = width / 2;
  size[1] = width / 2;
  region.SetSize(size);
  region.SetIndex(index);
  {
    PixelType pixel;
    pixel[0] = 1;
    pixel[1] = 1;
    pixel.Normalize();
    std::cout << "pixel: " << pixel << std::endl;
    std::cout << region;
    itk::ImageRegionIterator<ImageType> it(image, region);
    it.GoToBegin();
    while (!it.IsAtEnd())
    {
      it.Set(pixel);
      ++it;
    }
  }

  using VectorFilterType = itk::VectorConnectedComponentImageFilter<ImageType, OutputImageType>;

  auto filter = VectorFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, VectorConnectedComponentImageFilter, ConnectedComponentFunctorImageFilter);


  constexpr typename VectorFilterType::InputValueType distanceThreshold = 0.01;
  filter->SetDistanceThreshold(distanceThreshold);
  ITK_TEST_SET_GET_VALUE(distanceThreshold, filter->GetDistanceThreshold());

  filter->SetInput(image);

  using RelabelComponentType = itk::RelabelComponentImageFilter<OutputImageType, LabelImageType>;
  auto relabel = RelabelComponentType::New();
  relabel->SetInput(filter->GetOutput());

  using WriterType = itk::ImageFileWriter<LabelImageType>;
  auto writer = WriterType::New();

  try
  {
    writer->SetInput(relabel->GetOutput());
    writer->SetFileName(argv[1]);
    writer->Update();
    filter->SetFunctor(filter->GetFunctor());
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }

  // The following just ensures that this filter can be instantiated
  // with VectorImages
  using VectorImageType = itk::VectorImage<float, 2>;
  using VectorImageFilterType =
    itk::VectorConnectedComponentImageFilter<VectorImageType, OutputImageType, LabelImageType>;
  auto vfilter = VectorImageFilterType::New();

  return EXIT_SUCCESS;
}
