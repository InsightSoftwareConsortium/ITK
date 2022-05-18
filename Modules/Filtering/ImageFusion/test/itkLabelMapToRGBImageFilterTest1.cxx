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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelMapToRGBImageFilter.h"

#include "itkTestingMacros.h"

int
itkLabelMapToRGBImageFilterTest1(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 2;

  using IType = itk::Image<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ConverterType = itk::LabelImageToLabelMapFilter<IType>;
  auto converter = ConverterType::New();
  converter->SetInput(reader->GetOutput());

  //  using RGBPixelType = itk::RGBPixel< unsigned char >;
  //  using RGBImageType = itk::Image< RGBPixelType, dim >;

  using ColorizerType = itk::LabelMapToRGBImageFilter<ConverterType::OutputImageType>;
  auto colorizer = ColorizerType::New();
  colorizer->SetInput(converter->GetOutput());

  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  using WriterType = itk::ImageFileWriter<ColorizerType::OutputImageType>;
  auto writer = WriterType::New();
  writer->SetInput(colorizer->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  ColorizerType::FunctorType functor;
  functor.ResetColors();
  functor.AddColor(0, 0, 255);

  ITK_TEST_EXPECT_TRUE(colorizer->GetFunctor() != functor);
  colorizer->SetFunctor(functor);
  ITK_TEST_EXPECT_TRUE(ColorizerType::ConstPointer(colorizer)->GetFunctor() == functor);
  colorizer->GetFunctor().AddColor(0, 255, 0);
  ITK_TEST_EXPECT_TRUE(colorizer->GetFunctor() != functor);

  return EXIT_SUCCESS;
}
