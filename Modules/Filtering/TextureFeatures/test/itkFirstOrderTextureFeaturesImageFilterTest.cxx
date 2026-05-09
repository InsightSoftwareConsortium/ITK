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
#include "itkFirstOrderTextureFeaturesImageFilter.h"
#include "itkFlatStructuringElement.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


static void
Test1(const std::string & inFileName, const std::string & outFileName)
{
  constexpr unsigned int ImageDimension = 3;
  using ImageType = itk::Image<unsigned char, ImageDimension>;
  using OImageType = itk::Image<itk::FixedArray<float, 8>, ImageDimension>;
  using KernelType = itk::FlatStructuringElement<ImageDimension>;

  using TextureFilterType = itk::FirstOrderTextureFeaturesImageFilter<ImageType, OImageType, KernelType>;


  using ReaderType = itk::ImageFileReader<ImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inFileName);
  reader->UpdateLargestPossibleRegion();

  KernelType::SizeType radius;
  radius.Fill(5);
  KernelType                 kernel = KernelType::Box(radius);
  TextureFilterType::Pointer filter = TextureFilterType::New();
  filter->SetKernel(kernel);
  filter->SetInput(reader->GetOutput());
  filter->UpdateLargestPossibleRegion();

  using WriterType = itk::ImageFileWriter<OImageType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(outFileName);
  writer->SetInput(filter->GetOutput());
  writer->Update();
}


static void
Test2(std::string inFileName)
{
  constexpr unsigned int ImageDimension = 3;
  using ImageType = itk::Image<unsigned char, ImageDimension>;
  using OImageType = itk::VectorImage<float, ImageDimension>;
  using KernelType = itk::FlatStructuringElement<ImageDimension>;

  using TextureFilterType = itk::FirstOrderTextureFeaturesImageFilter<ImageType, OImageType, KernelType>;


  using ReaderType = itk::ImageFileReader<ImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inFileName);
  reader->UpdateLargestPossibleRegion();

  KernelType::SizeType radius;
  radius.Fill(5);
  KernelType                 kernel = KernelType::Box(radius);
  TextureFilterType::Pointer filter = TextureFilterType::New();
  filter->SetKernel(kernel);
  filter->SetInput(reader->GetOutput());
  filter->UpdateLargestPossibleRegion();

  std::cout << "filter..." << std::endl;
}

int
itkFirstOrderTextureFeaturesImageFilterTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile outputImageFile" << std::endl;
    return EXIT_FAILURE;
  }


  Test1(argv[1], argv[2]);
  Test2(argv[1]);

  return EXIT_SUCCESS;
}
