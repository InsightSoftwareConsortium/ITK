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

#include "itkSimpleFilterWatcher.h"
#include "itkGrayscaleGeodesicDilateImageFilter.h"
#include "itkGrayscaleGeodesicErodeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkShiftScaleImageFilter.h"

// This test should produce the same results as the
// itkHMaximaMinimaImageFilterTest.

int itkGrayscaleGeodesicErodeDilateImageFilterTest(int argc, char* argv [] )
{
  if ( argc < 4 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " Inputimage OutputImage Height" << std::endl;
    return EXIT_FAILURE;
  }
  const int Dimension = 2;
  typedef unsigned char                      PixelType;
  typedef itk::Image< PixelType, Dimension > InputImageType;
  typedef itk::Image< PixelType, Dimension > OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  typedef itk::ShiftScaleImageFilter <InputImageType,InputImageType>
    ShiftFilterType;
  typedef itk::GrayscaleGeodesicDilateImageFilter < InputImageType,
                                                   OutputImageType >
    DilateFilterType;
  typedef itk::GrayscaleGeodesicErodeImageFilter < InputImageType,
                                                   OutputImageType >
    ErodeFilterType;

  ReaderType::Pointer           reader = ReaderType::New();
  WriterType::Pointer           writer = WriterType::New();
  ShiftFilterType::Pointer      shiftErode = ShiftFilterType::New();
  ShiftFilterType::Pointer      shiftDilate = ShiftFilterType::New();
  ErodeFilterType::Pointer      erode = ErodeFilterType::New();
  DilateFilterType::Pointer     dilate = DilateFilterType::New();

  // Create the reader and writer
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Create the marker image for erosion
  shiftDilate->SetInput( reader->GetOutput());
  shiftDilate->SetShift( -1 * atoi(argv[3]) );

  // Dilate
  dilate->SetMarkerImage ( shiftDilate-> GetOutput() );
  dilate->SetMaskImage ( reader->GetOutput() );
  dilate->FullyConnectedOn();

  // Create the marker image for erode
  shiftErode->SetInput( dilate->GetOutput());
  shiftErode->SetShift( atoi(argv[3]) );

  // Erode
  erode->SetMarkerImage ( shiftErode->GetOutput() );
  erode->SetMaskImage ( dilate->GetOutput() );
  erode->FullyConnectedOn();

  writer->SetInput ( erode->GetOutput() );

  itk::SimpleFilterWatcher watchDilate(dilate);
  itk::SimpleFilterWatcher watchErode(erode);

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

  return EXIT_SUCCESS;
}
