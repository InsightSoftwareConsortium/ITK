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

#include <iostream>

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkVectorIndexSelectionCastImageFilter.h"

int itkVectorIndexSelectionCastImageFilterTest(int argc, char * argv [] )
{

  if( argc < 4 )
    {
    std::cerr << "itkVectorIndexSelectionCastImageFilterTest "
              << " InputVectorImage OutputScalarImage indexToExtract"
              << std::endl;
    return EXIT_FAILURE;
    }

  typedef unsigned short InputPixelType;
  typedef unsigned short OutputPixelType;

  const unsigned int ImageDimension = 2;

  typedef itk::VectorImage< InputPixelType, ImageDimension > InputImageType;
  typedef itk::Image< OutputPixelType, ImageDimension  >     OutputImageType;

  typedef itk::ImageFileReader< InputImageType  > ReaderType;
  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  typedef itk::VectorIndexSelectionCastImageFilter<
                                     InputImageType,
                                     OutputImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );

  const unsigned int index = atoi( argv[3] );

  filter->SetIndex( index );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }


  std::cout << "Test the exception if the index is too large" << std::endl;

  InputImageType::ConstPointer inputImage = reader->GetOutput();

  const unsigned int maximumIndex =
    inputImage->GetNumberOfComponentsPerPixel();

  filter->SetIndex( maximumIndex ); // this index is an invalid value;

  bool exceptionCaught = false;

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cerr << "Exception caught as expected: "  << e;
    exceptionCaught = true;
    }

  if( !exceptionCaught )
    {
    std::cerr << "Failed to catch exception "
              << "when index is too large !!" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
