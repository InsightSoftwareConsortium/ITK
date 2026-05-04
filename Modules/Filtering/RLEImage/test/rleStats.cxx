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
#include "itkRLEImage.h"
#include "itkTestDriverIncludeRequiredIOFactories.h"
#include <iostream>
#include <string>

int
main(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage" << std::endl;
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<short, 3>;
  using myRLEImage = itk::RLEImage<short, 3>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using inConverterType = itk::RegionOfInterestImageFilter<ImageType, myRLEImage>;
  inConverterType::Pointer inConv = inConverterType::New();

  try
  {
    RegisterRequiredFactories();
    reader->Update();
    inConv->SetInput(reader->GetOutput());
    inConv->SetRegionOfInterest(reader->GetOutput()->GetLargestPossibleRegion());
    inConv->Update();
    myRLEImage::Pointer test = inConv->GetOutput();
    std::cout << test;
    std::cout << "Invoking CleanUp() method\n";
    test->CleanUp();
    std::cout << test;
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
