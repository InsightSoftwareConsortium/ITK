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

#include "itkBlackTopHatImageFilter.h"
#include "itkWhiteTopHatImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkFilterWatcher.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkTopHatImageFilterTest(int argc, char* argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImage outputImage 0/1(Black/White) radius" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned int Dimension = 2;

  // Define the pixel type
  typedef unsigned char PixelType;

  // Declare the types of the images
  typedef itk::Image<PixelType, Dimension>  ImageType;

  // Declare the reader and writer
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;


  // Declare the type for the structuring element
  typedef itk::BinaryBallStructuringElement<
                            PixelType, Dimension> KernelType;

  // Declare the type for the morphology Filter
  typedef itk::BlackTopHatImageFilter<
                           ImageType, ImageType, KernelType> BlackFilterType;

  typedef itk::WhiteTopHatImageFilter<
                           ImageType, ImageType, KernelType> WhiteFilterType;

  // Create the reader and writer
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Create the structuring element
  KernelType ball;
  KernelType::SizeType ballSize;
  ballSize[0] = atoi(argv[4]);
  ballSize[1] = atoi(argv[4]);
  ball.SetRadius(ballSize);
  ball.CreateStructuringElement();

  switch (atoi(argv[3]))
    {
    case 0:
    {
    // Create the filter
    BlackFilterType::Pointer filter = BlackFilterType::New();
    FilterWatcher watcher(filter, "filter");

    // Connect the structuring element
    filter->SetKernel( ball );

    // Connect the pipeline
    filter->SetInput( reader->GetOutput() );
    filter->Update();
    writer->SetInput( filter->GetOutput() );

    break;
    }
    case 1:
    {
    // Create the filter
    WhiteFilterType::Pointer filter = WhiteFilterType::New();
    FilterWatcher watcher(filter, "filter");

    // Connect the structuring element
    filter->SetKernel( ball );

    // Connect the pipeline
    filter->SetInput( reader->GetOutput() );
    filter->Update();
    writer->SetInput( filter->GetOutput() );

    break;
    }
    default:
      std::cerr << "Invalid filter selector: " << atoi(argv[3]) << std::endl;
      return EXIT_FAILURE;

    }
  // Execute the filter
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception caught during pipeline Update\n"  << e;
    return EXIT_FAILURE;
    }

  // All objects should be automatically destroyed at this point

  return EXIT_SUCCESS;

}
