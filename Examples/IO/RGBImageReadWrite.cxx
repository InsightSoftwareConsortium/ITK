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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputRGBImageFile  outputRGBImageFile " << std::endl;
    return EXIT_FAILURE;
    }


  // Software Guide : BeginLatex
  //
  // The \doxygen{RGBPixel} class is templated over the type used to
  // represent each one of the red, green and blue components. A typical
  // instantiation of the RGB image class might be as follows.
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
  // The image type is used as a template parameter to instantiate
  // the reader and writer.
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
  //  Update() method in the writer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  writer->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  You may have noticed that apart from the declaration of the
  //  \code{PixelType} there is nothing in this code specific to RGB
  //  images. All the actions required to support color images are implemented
  //  internally in the \doxygen{ImageIO} objects.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
