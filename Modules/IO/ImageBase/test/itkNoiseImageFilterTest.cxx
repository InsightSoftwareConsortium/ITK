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
#include "itkNoiseImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkFilterWatcher.h"

int itkNoiseImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage\n";
    return -1;
    }

  itk::Size<2> radius;
  typedef itk::Image<unsigned short, 2> myImageIn;
  typedef itk::Image<float, 2>          myImageOut;
  typedef itk::Image<unsigned char, 2>  myImageChar;
  itk::ImageFileReader<myImageIn>::Pointer input
    = itk::ImageFileReader<myImageIn>::New();
  input->SetFileName(av[1]);

  // Create a filter
  typedef itk::NoiseImageFilter<myImageIn,myImageOut> FilterType;

  FilterType::Pointer filter = FilterType::New();
  FilterWatcher filterWatch(filter);

  typedef itk::RescaleIntensityImageFilter<myImageOut,myImageChar> RescaleFilterType;

  RescaleFilterType::Pointer rescale = RescaleFilterType::New();
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);
  rescale->SetInput( filter->GetOutput() );

  try
    {
    radius.Fill(5);
    filter->SetInput (input->GetOutput());
    filter->SetRadius (radius);
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }

  // Generate test image
  itk::ImageFileWriter<myImageChar>::Pointer writer;
    writer = itk::ImageFileWriter<myImageChar>::New();
    writer->SetInput( rescale->GetOutput() );
    writer->SetFileName( av[2] );
    writer->Update();

    return EXIT_SUCCESS;
}
