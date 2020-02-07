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
/**
 *
 * \author Ken Urish
 *
 * An example program to see how ThresholdMaximumConnectedComponentsImageFilter
 * works for the Insight Journal:
 *
 * Urish KL, August J, Huard J. "Unsupervised segmentation for myofiber
 * counting in immunoflourescent images". Insight Journal.
 * ISC/NA-MIC/MICCAI Workshop on Open-Source Software (2005)
 *
 * Dspace handle: https://hdl.handle.net/1926/48 */


// itk header files
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkThresholdMaximumConnectedComponentsImageFilter.h"
#include "itkTestingMacros.h"

int
itkThresholdMaximumConnectedComponentsImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << std::endl;
    std::cerr << " 1: InputImage Name 2:OutputImage Name" << std::endl;
    std::cerr << " 3: minimumPixelArea" << std::endl;
    return EXIT_FAILURE;
  }


  using InputPixelType = unsigned char;
  using OutputPixelType = unsigned char;
  constexpr unsigned int Dimension = 2;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  InputPixelType maxLabel = itk::NumericTraits<InputPixelType>::max();
  InputPixelType minLabel = itk::NumericTraits<InputPixelType>::NonpositiveMin();

  const unsigned int minimumPixelArea = std::stoi(argv[3]);

  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(argv[1]);

  // Read the Input Image
  std::cout << "About to load input image " << std::endl;

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "Exception Caught!" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // ************************************************************************
  // Automatic Threshold Filter
  // This filter essentially chooses the optimum place to threshold the object.
  // It also indirectly will count the number of objects for you.
  // As a note, SetInsideValue(maxLabel)/SetOutsideValue(minLabel) will count
  // the number of light objects. If the reverse, it will count the number of
  // dark objects.

  unsigned int numberOfObjects;
  unsigned int thresholdValue;

  using ThresholdType = itk::ThresholdMaximumConnectedComponentsImageFilter<InputImageType>;
  ThresholdType::Pointer automaticThreshold = ThresholdType::New();

  automaticThreshold->SetInput(reader->GetOutput());
  automaticThreshold->SetMinimumObjectSizeInPixels(minimumPixelArea);

  // For counting Myofibers, the inside value should be the minLabel
  // If you wanted to count a solid object (ie dapi nuclei) set the
  // inside value to minLabel.
  //
  automaticThreshold->SetInsideValue(minLabel);
  automaticThreshold->SetOutsideValue(maxLabel);

  try
  {
    automaticThreshold->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "Exception Caught!" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  numberOfObjects = automaticThreshold->GetNumberOfObjects();
  thresholdValue = automaticThreshold->GetThresholdValue();

  std::cout << "Number of Objects = " << numberOfObjects << std::endl;
  std::cout << "Threshold Value   = " << thresholdValue << std::endl;

  // *****************************************************************
  // Image File Writer
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(automaticThreshold->GetOutput());

  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "Exception Caught!" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
