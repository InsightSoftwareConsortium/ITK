/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorIndexSelectionCastImageFilterTest.cxx
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

#include <iostream>

#include "itkImage.h"
#include "itkVectorImage.h"
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
  typedef itk::Image< OutputPixelType, ImageDimension  > OutputImageType;

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

