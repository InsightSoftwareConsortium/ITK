/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleGeodesicErodeDilateImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkSimpleFilterWatcher.h"
#include "itkGrayscaleGeodesicDilateImageFilter.h"
#include "itkGrayscaleGeodesicErodeImageFilter.h"
#include "itkImage.h"
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
  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, Dimension >   InputImageType;
  typedef itk::Image< PixelType, Dimension >   OutputImageType;
  
  typedef itk::ImageFileReader< InputImageType >
    ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >
    WriterType;
  
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




