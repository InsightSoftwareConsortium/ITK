/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RGBImageReadWrite.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/




//  Software Guide : BeginLatex
//
//  RGB images are commonly used for representing data acquired from cryogenic
//  sections, optical microscopy and endoscopy. This example illustrates how to
//  read RGB color images from a file, perform a simple processing on them and
//  write them down to a file. This requires the following headers
//
//  \index{itk::RGBPixel!Image}
//  \index{RGB!writing Image}
//  \index{RGB!reading Image}
//
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkRGBPixel.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet

#include "itkMetaImageIO.h"


int main( int argc, char ** argv )
{

  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputRGBImageFile  outputRGBImageFile " << std::endl;
    return -1;
    }



  // Software Guide : BeginLatex
  //
  // The RGB pixel class is templated over the type used to represent each
  // one of the Red, Green and Blue components. A typical instantiation of 
  // the RGB image class could be as follows.
  //
  //  \index{itk::RGBPixel!Instantiation}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::RGBPixel< unsigned char >   PixelType;
  typedef itk::Image< PixelType, 2 >       ImageType;
  // Software Guide : EndCodeSnippet



  // Software Guide : BeginLatex
  //
  // The image type can be used for instantiating the reader and writer types
  // and then creating one object of each type.
  //
  // \index{itk::ImageFileReader!RGB Image}
  // \index{itk::ImageFileWriter!RGB Image}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  // Software Guide : EndCodeSnippet


  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];


  //  Software Guide : BeginLatex
  //
  //  The filenames of the input and output files must be provided to the
  //  reader and writer respectively.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );
  // Software Guide : EndCodeSnippet


  ImageType::Pointer image = reader->GetOutput();
  
  writer->SetInput( image );


  //  Software Guide : BeginLatex
  //
  //  Finally, execution of the pipeline can be triggered by invoking the
  //  \code{Update()} method in the writer.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  writer->Update();
  // Software Guide : EndCodeSnippet



  return 0;


}



