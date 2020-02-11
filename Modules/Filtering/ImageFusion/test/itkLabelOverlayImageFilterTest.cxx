/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkVectorImage.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int
itkLabelOverlayImageFilterTest(int argc, char * argv[])
{
  constexpr int Dimension = 2;

  if (argc < 5)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " InputImage  LabelImage Opacity OutputImage" << std::endl;
    return 1;
  }

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ColorPixelType = itk::RGBPixel<unsigned char>;
  using ColorImageType = itk::Image<ColorPixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  // Read in the input image
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Read in the label image
  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  // Instantiate the filter
  using FilterType = itk::LabelOverlayImageFilter<ImageType, ImageType, ColorImageType>;
  FilterType::Pointer filter = FilterType::New();

  // Exercising Background Value methods
  filter->SetBackgroundValue(10);
  if (filter->GetBackgroundValue() != 10)
  {
    std::cerr << "Background value Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise set/get opacity values
  filter->SetOpacity(2);
  if (itk::Math::NotExactlyEquals(filter->GetOpacity(), 2))
  {
    std::cerr << "Opacity Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
  }
  filter->SetOpacity(3);
  if (itk::Math::NotExactlyEquals(filter->GetOpacity(), 3))
  {
    std::cerr << "Opacity Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
  }


  // Set the filter input and label images
  filter->SetInput(reader->GetOutput());
  filter->SetLabelImage(reader2->GetOutput());
  filter->SetBackgroundValue(13);

  // Set opacity
  filter->SetOpacity(std::stod(argv[3]));

  itk::SimpleFilterWatcher watcher(filter, "filter");

  // Instantiate output image
  using WriterType = itk::ImageFileWriter<ColorImageType>;
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[4]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // exercise the methods to change the colors
  unsigned int numberOfColors1 = filter->GetNumberOfColors();
  filter->AddColor(1, 255, 255);

  unsigned int numberOfColors2 = filter->GetNumberOfColors();

  if (numberOfColors2 != numberOfColors1 + 1)
  {
    std::cerr << "Error in GetNumberOfColors() or AddColor() " << std::endl;
    return EXIT_FAILURE;
  }

  filter->ResetColors();
  filter->AddColor(255, 255, 255);

  unsigned int numberOfColors3 = filter->GetNumberOfColors();

  if (numberOfColors3 != 1)
  {
    std::cerr << "Error in GetNumberOfColors() or ResetColors() or AddColor() " << std::endl;
    return EXIT_FAILURE;
  }

  // the following just ensures the filter can be instantiated with a
  // VectorImage as output.
  // using VectorImageType = itk::VectorImage< short, Dimension >;
  // using LabelOverlayToVectorFilterType = itk::LabelOverlayImageFilter< ImageType, ImageType, VectorImageType >;
  // std::cout << LabelOverlayToVectorFilterType::New();

  return EXIT_SUCCESS;
}
