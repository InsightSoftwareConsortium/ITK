/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageReadImageSeriesWrite.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//  
//  This example illustrates how to save an image using the
//  \doxgen{ImageSeriesWriter}. This class allows to save a 3D volume as a set
//  of files containting one 2D slice per file.
//
//  \index{itk::ImageFileReader!header}
//  \index{itk::ImageSeriesWriter!header}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"

int main( int argc, char *argv[] )
{
  if (argc < 3 )
    {
    std::cout << "Usage: WriteOutPNG3D inputFile outputPrefix" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned char,3> ImageType;
  typedef itk::Image<unsigned char,2> Image2DType;
  typedef itk::ImageFileReader<ImageType> ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading the image" << std::endl;
    std::cerr << excp << std::endl;
    }

  reader->GetOutput()->Print( std::cout );

  typedef itk::ImageSeriesWriter< ImageType, Image2DType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( reader->GetOutput() );

  char format[500];
  sprintf( format, "%s%03d.png", argv[2] );


  writer->DebugOn();

  writer->SetSeriesFormat( format );
  writer->SetStartIndex( 0 );
  writer->SetIncrementIndex( 1 );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading the image" << std::endl;
    std::cerr << excp << std::endl;
    }


  return EXIT_SUCCESS;
}


