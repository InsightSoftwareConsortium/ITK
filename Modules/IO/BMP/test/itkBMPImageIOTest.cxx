/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include <fstream>


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkBMPImageIOTest( int ac, char* av[] )
{

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " Input Output\n";
    return EXIT_FAILURE;
    }


  // ATTENTION THIS IS THE PIXEL TYPE FOR
  // THE RESULTING IMAGE
  typedef itk::RGBPixel<unsigned char> PixelType;
  typedef itk::Image<PixelType, 2>     myImage;

  itk::ImageFileReader<myImage>::Pointer reader
                                  = itk::ImageFileReader<myImage>::New();

  reader->SetFileName(av[1]);
  reader->UpdateOutputInformation();

  std::cout << "PixelType: " << reader->GetImageIO()->GetPixelTypeAsString(reader->GetImageIO()->GetPixelType()) << std::endl;
  std::cout << "ComponentType: " << reader->GetImageIO()->GetComponentTypeAsString(reader->GetImageIO()->GetComponentType()) << std::endl;
  std::cout << "NumberOfComponents: " << reader->GetImageIO()->GetNumberOfComponents() << std::endl;
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in image file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  myImage::Pointer image = reader->GetOutput();

  image->Print(std::cout );

  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  // Print the IO
  reader->GetImageIO()->Print(std::cout);

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName(av[2]);
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in image file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Print the IO
  writer->GetImageIO()->Print(std::cout);

  return EXIT_SUCCESS;

}
