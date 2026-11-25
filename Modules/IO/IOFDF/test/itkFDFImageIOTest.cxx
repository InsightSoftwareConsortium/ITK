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

#include "itkFDFImageIOFactory.h"
#include "itkFDFImageIO.h"

#include "itkImage.h"

int
itkFDFImageIOTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: itkFDFImageIO <output_directory> <inputfile" << std::endl;
    return EXIT_FAILURE;
  }
  using PixelType = float;
  constexpr unsigned int Dimension = 2;

  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  // Register FDF Factory
  itk::FDFImageIOFactory::RegisterOneFactory();

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(argv[2]);

  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & exp)
  {
    std::cerr << "Exception caught" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
  }
  ImageType::Pointer im = reader->GetOutput();

  std::cerr << im->GetDirection() << std::endl << im->GetOrigin() << std::endl << im->GetSpacing() << std::endl;

  return EXIT_SUCCESS;
}
