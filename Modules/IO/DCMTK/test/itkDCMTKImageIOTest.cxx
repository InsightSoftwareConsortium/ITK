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

#include <fstream>

#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkDCMTKImageIOTest(int ac, char* av[])
{

  if(ac < 5)
    {
    std::cerr << "Usage: " << av[0] << " DicomImage OutputDicomImage OutputImage RescalDicomImage\n";
    return EXIT_FAILURE;
    }

  typedef short                                   InputPixelType;
  typedef itk::Image< InputPixelType, 2 >         InputImageType;
  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::DCMTKImageIO                       ImageIOType;
  ImageIOType::Pointer dcmtkImageIO = ImageIOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( av[1] );
  reader->SetImageIO( dcmtkImageIO );
  // reader->DebugOn();

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

  // Rescale intensities and rewrite the image in another format
  //
  typedef unsigned char                   WritePixelType;
  typedef itk::Image< WritePixelType, 2 > WriteImageType;
  typedef itk::RescaleIntensityImageFilter<
    InputImageType, WriteImageType >      RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( reader->GetOutput() );

  typedef itk::ImageFileWriter< WriteImageType >  Writer2Type;
  Writer2Type::Pointer writer2 = Writer2Type::New();
  // writer2->DebugOn();
  writer2->SetFileName( av[3] );
  writer2->SetInput( rescaler->GetOutput() );

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
