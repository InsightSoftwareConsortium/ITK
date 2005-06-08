/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RGBImageSeriesReadWrite.cxx
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
//  RGB images are commonly used for representing data acquired from
//  cryogenic sections, optical microscopy and endoscopy. This example
//  illustrates how to read and write RGB color images to and from a file.
//  This requires the following headers as shown.
//
//  \index{itk::RGBPixel!Image}
//  \index{RGB!writing Image}
//  \index{RGB!reading Image}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkRGBPixel.h"
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
    std::cerr << argv[0] << "first last  outputRGBImageFile " << std::endl;
    return EXIT_FAILURE;
    }


  // Software Guide : BeginLatex
  //
  // The \doxygen{RGBPixel} class is templated over the type used to
  // represent each one of the red, green and Blue components. A typical
  // instantiation of the RGB image class might be as follows.
  //
  //  \index{itk::RGBPixel!Instantiation}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::RGBPixel< unsigned char >        PixelType;
  const unsigned int Dimension = 3;

  typedef itk::Image< PixelType, Dimension >    ImageType;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The image type is used as a template parameter to instantiate
  // the reader and writer.
  //
  // \index{itk::ImageSeriesReader!RGB Image}
  // \index{itk::ImageFileWriter!RGB Image}
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

  nameGenerator->SetSeriesFormat( "vwe%03d.png" );
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

  //  Software Guide : BeginLatex
  //
  //  You may have noticed that apart from the declaraton of the
  //  \code{PixelType} there is nothing in this code that is specific for RGB
  //  images. All the actions required to support color images are implemented
  //  internally in the \doxygen{ImageIO} objects.
  //
  //  Software Guide : EndLatex 


  return EXIT_SUCCESS;
}



