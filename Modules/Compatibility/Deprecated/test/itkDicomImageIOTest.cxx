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

#include <fstream>
#include "itkDicomImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkDicomImageIOTest(int ac, char* av[])
{

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " DicomImage OutputImage\n";
    return EXIT_FAILURE;
    }


  // ATTENTION THIS IS THE PIXEL TYPE FOR
  // THE RESULTING IMAGE
  typedef short PixelType;

  typedef itk::Image<PixelType, 2> myImage;

  itk::ImageFileReader<myImage>::Pointer reader
                                  = itk::ImageFileReader<myImage>::New();
  itk::DicomImageIOFactory::RegisterOneFactory();

  reader->SetFileName(av[1]);

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

  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

  typedef itk::RescaleIntensityImageFilter<
               myImage, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );


  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( av[2] );

  // Software Guide : BeginCodeSnippet
  rescaler->SetInput( reader->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();
  return EXIT_SUCCESS;

}
