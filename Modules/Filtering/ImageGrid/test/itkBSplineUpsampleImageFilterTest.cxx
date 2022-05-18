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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#include <iostream>

#include "itkBSplineUpsampleImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkBSplineUpsampleImageFilterTest(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Error: Missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "inputImage outputImage splineOrder" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  constexpr unsigned int Dimension = 3;

  using ImageType = itk::Image<PixelType, Dimension>;

  using UpsamplerFilterType = itk::BSplineUpsampleImageFilter<ImageType, ImageType>;

  auto filter = UpsamplerFilterType::New();

  itk::SimpleFilterWatcher watcher(filter, "BSplineUpsampleImageFilter");

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  const unsigned int splineOrder = std::stoi(argv[3]);

  filter->SetSplineOrder(splineOrder);

  filter->SetInput(reader->GetOutput());
  writer->SetInput(filter->GetOutput());

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
