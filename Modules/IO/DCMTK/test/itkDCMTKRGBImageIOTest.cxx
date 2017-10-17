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
#include "itkDCMTKImageIO.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkRGBPixel.h"

#include <fstream>

#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkDCMTKRGBImageIOTest(int ac, char* av[])
{

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " DicomImage OutputImage\n";
    return EXIT_FAILURE;
    }

  typedef itk::RGBPixel<unsigned char>            PixelType;
  typedef itk::Image< PixelType, 2 >              InputImageType;
  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::DCMTKImageIO                       ImageIOType;
  ImageIOType::Pointer dcmtkImageIO = ImageIOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( av[1] );
  reader->SetImageIO( dcmtkImageIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image< PixelType, 2 >              WriteImageType;
  typedef itk::ImageFileWriter< WriteImageType >  Writer2Type;
  Writer2Type::Pointer writer2 = Writer2Type::New();
  writer2->SetFileName( av[2] );
  writer2->SetInput( reader->GetOutput() );

  try
    {
    writer2->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  dcmtkImageIO->Print( std::cout );

  return EXIT_SUCCESS;

}
