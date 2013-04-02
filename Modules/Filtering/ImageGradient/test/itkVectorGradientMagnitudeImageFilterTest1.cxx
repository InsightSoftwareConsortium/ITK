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
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

int itkVectorGradientMagnitudeImageFilterTest1(int ac, char* av[] )
{
  typedef itk::RGBPixel<unsigned short>                         RGBPixelType;
  typedef itk::Image<unsigned char, 2>                          CharImageType;
  typedef itk::Image<RGBPixelType, 2>                           RGBImageType;
  typedef itk::VectorGradientMagnitudeImageFilter<RGBImageType> FilterType;
  typedef itk::ImageFileReader<RGBImageType>                    ReaderType;
  typedef itk::RescaleIntensityImageFilter<FilterType::OutputImageType,
    CharImageType>                                              RescaleFilterType;
  typedef itk::ImageFileWriter<CharImageType>                   WriterType;

  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage Mode\n";
    return EXIT_FAILURE;
    }

  // Create a reader and filter
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(av[1]);
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(reader->GetOutput());

  int mode = ::atoi( av[3] );
  if ( mode == 1)
    {
    filter->SetUsePrincipleComponentsOn();
    }
  else
    {
    filter->SetUsePrincipleComponentsOff();
    }

  RescaleFilterType::Pointer rescale = RescaleFilterType::New();
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);
  rescale->SetInput( filter->GetOutput() );

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescale->GetOutput() );
  writer->SetFileName( av[2] );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise the Print method
  std::cout << "-- Test of the Print method --------------" << std::endl;
  filter->Print( std::cout );
  std::cout << "-- End of Print method test --------------" << std::endl;


  std::cout <<  "The gradient image range was (low, high) = ("
            <<  rescale->GetInputMinimum() << ", " << rescale->GetInputMaximum()
            << ")" << std::endl;
  std::cout <<  "Output was scaled, shifted = " << rescale->GetScale() << ", "
            << rescale->GetShift() << std::endl;

  return EXIT_SUCCESS;
}
