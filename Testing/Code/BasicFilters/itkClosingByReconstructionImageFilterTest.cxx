/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkClosingByReconstructionImageFilterTest.cxx
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

#include "itkClosingByReconstructionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkBinaryBallStructuringElement.h"


int itkClosingByReconstructionImageFilterTest(int argc, char* argv [] ) 
{
 if ( argc < 4 )
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << argv[0] << " Inputimage OutputImage Radius" << std::endl;
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
  filter->SetKernel( structuringElement );

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
   
  return EXIT_SUCCESS;

}




