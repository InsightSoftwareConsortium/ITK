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

#include "itkSLICImageFilter.h"
#include "itkVectorImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkCommand.h"
#include "itkTestingMacros.h"

namespace
{

template <typename TFilterType>
void
iterationEventCallback(itk::Object * object, const itk::EventObject & event, void *)
{
  static unsigned int iterationCount = 0;
  const auto *        slicFilter = dynamic_cast<const TFilterType *>(object);
  if (!itk::IterationEvent().CheckEvent(&event) || !slicFilter)
  {
    return;
  }

  std::cout << "Iterations #: " << iterationCount++ << " Average Residual: " << slicFilter->GetAverageResidual()
            << std::endl;
}


template <typename TInputImageType>
int
itkSLICImageFilterTestHelper(const std::string & inFileName,
                             const std::string & outFileName,
                             const unsigned int  gridSize,
                             bool                enforceConnectivity)
{

  const unsigned int Dimension = TInputImageType::ImageDimension;

  using OutputImageType = itk::Image<unsigned short, Dimension>;

  using InputImageType = TInputImageType;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inFileName);

  using FilterType = itk::SLICImageFilter<InputImageType, OutputImageType>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, SLICImageFilter, ImageToImageFilter);


  filter->SetInput(reader->GetOutput());
  filter->SetSuperGridSize(gridSize);
  filter->SetEnforceConnectivity(enforceConnectivity);
  filter->DebugOn();


  itk::CStyleCommand::Pointer command = itk::CStyleCommand::New();
  command->SetCallback(iterationEventCallback<FilterType>);

  filter->AddObserver(itk::IterationEvent(), command);


  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outFileName);
  writer->SetInput(filter->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
} // namespace

int
itkSLICImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "inputFileName outputFileName [gridSize] [enforceConnectivity]" << std::endl;
    return EXIT_FAILURE;
  }


  const bool         enforceConnectivity = (argc > 4) ? std::stoi(argv[4]) : true;
  const unsigned int gridSize = (argc > 3) ? std::stoi(argv[3]) : 20;
  const char *       inFileName = argv[1];
  const char *       outFileName = argv[2];

  const unsigned int VDimension = 2;
  using InputImageType = itk::VectorImage<float, VDimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inFileName);
  reader->UpdateOutputInformation();


  const unsigned int Dimension = reader->GetImageIO()->GetNumberOfDimensions();
  const unsigned int numberOfComponents = reader->GetImageIO()->GetNumberOfComponents();
  switch (Dimension)
  {
    case 1:
    case 2:
      if (numberOfComponents == 1)
      {
        itkSLICImageFilterTestHelper<itk::Image<float, 2>>(inFileName, outFileName, gridSize, enforceConnectivity);
      }
      else
      {
        itkSLICImageFilterTestHelper<itk::VectorImage<float, 2>>(
          inFileName, outFileName, gridSize, enforceConnectivity);
      }
      break;
    case 3:
      if (numberOfComponents == 1)
      {
        itkSLICImageFilterTestHelper<itk::Image<float, 3>>(inFileName, outFileName, gridSize, enforceConnectivity);
      }
      else
      {
        itkSLICImageFilterTestHelper<itk::VectorImage<float, 3>>(
          inFileName, outFileName, gridSize, enforceConnectivity);
      }
      break;
    default:
      std::cerr << "Unsupported Dimensions: " << Dimension << std::endl;
      return EXIT_FAILURE;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
