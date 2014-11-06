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
//  This example illustrates how to read a series of 2D slices from independent
//  files in order to compose a volume. The class \doxygen{ImageSeriesReader}
//  is used for this purpose. This class works in combination with a generator
//  of filenames that will provide a list of files to be read. In this
//  particular example we use the \doxygen{NumericSeriesFileNames} class as a
//  filename generator. This generator uses a \code{printf} style of string format
//  with a ``\code{\%d}'' field that will be successively replaced by a number specified
//  by the user. Here we will use a format like ``\code{file\%03d.png}'' for reading
//  PNG files named file001.png, file002.png, file003.png... and so on.
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
    std::cerr << argv[0] << " firstSliceValue lastSliceValue  outputImageFile " << std::endl;
    return EXIT_FAILURE;
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


  // Software Guide : BeginLatex
  //
  // Then, we declare the filename generator type and create one instance of it.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::NumericSeriesFileNames    NameGeneratorType;

  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The filename generator requires us to provide a pattern of text for the
  // filenames, and numbers for the initial value, last value and increment to be
  // used for generating the names of the files.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  nameGenerator->SetSeriesFormat( "vwe%03d.png" );

  nameGenerator->SetStartIndex( first );
  nameGenerator->SetEndIndex( last );
  nameGenerator->SetIncrementIndex( 1 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The ImageIO object that actually performs the read process is now connected
  //  to the ImageSeriesReader. This is the safest way of making sure that we use
  //  an ImageIO object that is appropriate for the type of files that we want to
  //  read.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->SetImageIO( itk::PNGImageIO::New() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The filenames of the input files must be provided to the reader, while the
  //  writer is instructed to write the same volume dataset in a single file.
  //
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->SetFileNames( nameGenerator->GetFileNames()  );

  writer->SetFileName( outputFilename );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We connect the output of the reader to the input of the writer.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  writer->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally, execution of the pipeline can be triggered by invoking the
  //  \code{Update()} method in the writer. This call must be placed in a
  //  \code{try/catch} block since exceptions be potentially be thrown in the
  //  process of reading or writing the images.
  //
  //  Software Guide : EndLatex

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

  return EXIT_SUCCESS;
}
