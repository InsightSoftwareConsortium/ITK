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
#include "itkVectorImage.h"
#include "itkGradientImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

namespace
{

template <typename TInputImage>
int
DoIt(const std::string & infname, const std::string & outfname)
{
  using InputImageType = TInputImage;

  const unsigned int ImageDimension = InputImageType::ImageDimension;
  using InputPixelType = typename InputImageType::PixelType;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(infname);

  using FilterType = itk::GradientImageFilter<InputImageType>;
  auto filter = FilterType::New();

  filter->SetInput(reader->GetOutput());

  using OutputImageType = typename FilterType::OutputImageType;
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(outfname);
  writer->SetNumberOfStreamDivisions(5);
  writer->Update();

  std::cout << filter;

  using VectorImageType = itk::VectorImage<InputPixelType, ImageDimension>;

  using VectorFilterType = itk::GradientImageFilter<InputImageType, float, float, VectorImageType>;
  auto vectorFilter = VectorFilterType::New();
  vectorFilter->SetInput(reader->GetOutput());
  vectorFilter->Update();

  filter->UpdateLargestPossibleRegion();

  itk::ImageRegionConstIterator<OutputImageType> iter(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  itk::ImageRegionConstIterator<VectorImageType> viter(vectorFilter->GetOutput(),
                                                       vectorFilter->GetOutput()->GetBufferedRegion());

  // Check the filter output
  bool diff = false;
  while (!iter.IsAtEnd())
  {

    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      if (!itk::Math::FloatAlmostEqual(iter.Get()[i], viter.Get()[i]))
      {
        diff = true;
      }
    }

    ++viter;
    ++iter;
  }

  if (diff)
  {
    std::cerr << "VectorImage output does not match covariant!" << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

} // namespace

int
itkGradientImageFilterTest2(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " Inputimage OutputImage" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string infname = argv[1];
  const std::string outfname = argv[2];

  itk::ImageIOBase::Pointer iobase =
    itk::ImageIOFactory::CreateImageIO(infname.c_str(), itk::ImageIOFactory::IOFileModeEnum::ReadMode);

  if (iobase.IsNull())
  {
    itkGenericExceptionMacro("Unable to determine ImageIO reader for \"" << infname << "\"");
  }

  using TestImageType = itk::Image<short, 3>;
  using FilterType = itk::GradientImageFilter<TestImageType>;

  auto filter = FilterType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, GradientImageFilter, ImageToImageFilter);

  const unsigned int dimension = iobase->GetNumberOfDimensions();

  if (dimension == 2)
  {
    return DoIt<itk::Image<float, 2>>(infname, outfname);
  }
  else if (dimension == 3)
    return DoIt<itk::Image<float, 3>>(infname, outfname);

  return EXIT_FAILURE;
}
