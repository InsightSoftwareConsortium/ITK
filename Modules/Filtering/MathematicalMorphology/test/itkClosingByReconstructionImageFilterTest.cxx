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

#include "itkClosingByReconstructionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSubtractImageFilter.h"


int itkClosingByReconstructionImageFilterTest(int argc, char* argv [] )
{
 if ( argc < 5 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " Inputimage OutputImage Radius PreserveIntensities(0,1) [Diffmage]" << std::endl;
    return EXIT_FAILURE;
  }

  const int Dimension = 2;
  typedef unsigned char                      PixelType;
  typedef itk::Image< PixelType, Dimension > InputImageType;
  typedef itk::Image< PixelType, Dimension > OutputImageType;

  typedef itk::ImageFileReader< InputImageType >   ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  // Declare the type of the Structuring element to be used
  typedef itk::BinaryBallStructuringElement<
                            PixelType,
                            Dimension>                  StructuringElementType;

  // Declare the type for the Morphology Filters to be Tested
  typedef itk::ClosingByReconstructionImageFilter<
                                InputImageType,
                                OutputImageType,
                                StructuringElementType >  MorphologicalFilterType;

  ReaderType::Pointer           reader = ReaderType::New();
  WriterType::Pointer           writer = WriterType::New();

  // Create the reader and writer
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Create the filter
  MorphologicalFilterType::Pointer   filter = MorphologicalFilterType::New();

  StructuringElementType   structuringElement;
  structuringElement.SetRadius(atoi(argv[3]));
  structuringElement.CreateStructuringElement();

  filter->SetKernel( structuringElement );
  if (atoi(argv[4]) == 0)
    {
    filter->PreserveIntensitiesOff();
    }
  else
    {
    filter->PreserveIntensitiesOn();
    }

  // Connect the pipelines
  filter->SetInput ( reader-> GetOutput() );
  writer->SetInput ( filter-> GetOutput() );

  // Execute print
  filter->Print( std::cout );

  // Execute the filter
  try
  {
    writer->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << "Exception caught:" << excp << std::endl;
    return  EXIT_FAILURE;
  }
  // Create a difference image if one is requested
  if (argc == 6)
    {
    itk::SubtractImageFilter<InputImageType, OutputImageType, OutputImageType>::Pointer subtract = itk::SubtractImageFilter<InputImageType, OutputImageType, OutputImageType>::New();
    subtract->SetInput( 1, reader->GetOutput() );
    subtract->SetInput( 0, filter->GetOutput() );
    try
      {
      writer->SetFileName( argv[5] );
      writer->SetInput( subtract->GetOutput() );
      writer->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception caught writing diff image:" << excp << std::endl;
      return  EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;

}
