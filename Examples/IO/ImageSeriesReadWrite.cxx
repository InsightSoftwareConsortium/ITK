/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageSeriesReadWrite.cxx
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

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a series of 2D slices from independent
//  files in order to compose a volume. The class \doxygen{ImageSeriesReader}
//  is used for this purpose. This class works in combination with a generator
//  of filenames that will provide a list of files to be read. In this
//  particular example we use the \doxygen{NumericSeriesFileNames} class as
//  filename generator. This generator uses a \code{printf} style of string format
//  with a ``\%d'' field that will be succesively replaced by a number specified
//  by the user. Here we will use a format like ``file\%04d.png'' for reading 
//  PNG files named file0001.png, file0002.png, file0003.png... and so on.
//
//  This requires the following headers as shown.
//
//  \index{itk::ImageSeriesReader!header}
//  \index{itk::NumericSeriesFileNames!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkPNGImageIO.h"
// Software Guide : EndCodeSnippet


int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "first last  outputImageFile " << std::endl;
    return -1;
    }


  // Software Guide : BeginLatex
  //
  // We start by defining the \code{PixelType} and \code{ImageType}.
  //
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef unsigned char                       PixelType;
  const unsigned int Dimension = 3;

  typedef itk::Image< PixelType, Dimension >  ImageType;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The image type is used as a template parameter to instantiate
  // the reader and writer.
  //
  // \index{itk::ImageSeriesReader!Instantiation}
  // \index{itk::ImageFileWriter!Instantiation}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageSeriesReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter<   ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  // Software Guide : EndCodeSnippet


  const unsigned int first = atoi( argv[1] );
  const unsigned int last  = atoi( argv[2] );

  const char * outputFilename = argv[3];

  // Software Guide : BeginCodeSnippet
  typedef itk::NumericSeriesFileNames    NameGeneratorType;

  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();
   
  nameGenerator->SetStartIndex( first );
  nameGenerator->SetEndIndex( last );
  nameGenerator->SetIncrementIndex( 1 );

  nameGenerator->SetSeriesFormat( "vwe%04d.png" );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The ImageIO object that actually performs the read process
  //  is now connected to the ImageSeriesReader.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  reader->SetImageIO( itk::PNGImageIO::New() );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The filenames of the input and output files must be provided to the
  //  reader and writer respectively.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  reader->SetFileNames( nameGenerator->GetFileNames()  );

  writer->SetFileName( outputFilename );
  // Software Guide : EndCodeSnippet

  try
    { 
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error reading the series " << std::endl;
    std::cerr << excp << std::endl;
    }

  ImageType::Pointer image = reader->GetOutput();
  writer->SetInput( image );


  //  Software Guide : BeginLatex
  //
  //  Finally, execution of the pipeline can be triggered by invoking the
  //  Update() method in the writer.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  writer->Update();
  // Software Guide : EndCodeSnippet


  return 0;
}



