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
//  This example illustrates how to read and write an image of pixel type
//  \doxygen{Vector}.
//
//  \index{itk::ImageFileRead!Vector images}
//  \index{itk::ImageFileWrite!Vector images}
//  \index{Vector images!Reading}
//  \index{Vector images!Writing}
//
//  Software Guide : EndLatex

// Software Guide : BeginLatex
//
// We should include the header files for the Image, the ImageFileReader and
// the ImageFileWriter.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv [] )
{
  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputImageFile " << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // Then we define the specific type of vector to be used as pixel type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int VectorDimension = 3;

  typedef itk::Vector< float, VectorDimension >    PixelType;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We define the image dimension, and along with the pixel type we use it for
  // fully instantiating the image type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int ImageDimension = 2;

  typedef itk::Image< PixelType, ImageDimension > ImageType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Having the image type at hand, we can instantiate the reader and writer
  // types, and use them for creating one object of each type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // A filename must be provided to both the reader and the writer. In this
  // particular case we take those filenames from the command line arguments.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // This being a minimal example, we create a short pipeline where we simply
  // connect the output of the reader to the input of the writer.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  writer->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The execution of this short pipeline is triggered by invoking the writer's
  // \code{Update()} method. This invocation must be placed inside a
  // \code{try/catch} block since its execution may result in exceptions being
  // thrown.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Of course, you could envision the addition of filters in between the reader
  // and the writer. Those filters could perform operations on the vector image.
  //
  // Software Guide : EndLatex

  return EXIT_SUCCESS;
}
