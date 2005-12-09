/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpeningByReconstructionImageFilterTest.cxx
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

#include "itkOpeningByReconstructionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSubtractImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkOpeningByReconstructionImageFilterTest(int argc, char* argv [] ) 
{
 if ( argc < 5 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " Inputimage OutputImage Radius PreserveIntensities(0,1) [Diffmage]" << std::endl;
    return EXIT_FAILURE;
  } 
  
  const int Dimension = 2;
  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, Dimension >   InputImageType;
  typedef itk::Image< PixelType, Dimension >   OutputImageType;
  
  typedef itk::ImageFileReader< InputImageType >   ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  
  // Declare the type of the Structuring element to be used
  typedef itk::BinaryBallStructuringElement< 
                            PixelType,
                            Dimension>                  StructuringElementType;

  // Declare the type for the Morphology Filters to be Tested
  typedef itk::OpeningByReconstructionImageFilter< 
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
  itk::SimpleFilterWatcher watcher(filter, "Opening"); watcher.QuietOn();
 
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
    subtract->SetInput( 0, reader->GetOutput() );
    subtract->SetInput( 1, filter->GetOutput() );
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




