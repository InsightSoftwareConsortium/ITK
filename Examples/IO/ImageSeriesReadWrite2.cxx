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
//  particular example we use the \doxygen{RegularExpressionSeriesFileNames} class as
//  filename generator. This generator uses a regular expression for generating a list
//  of filenames. The filenames are then ordered according to sub expression.
//
// Regular expressions are a powerful,  compact mechanism for parsing strings.
// Expressions consist of the following metacharacters:
//
// \begin{itemize}
//
//  \item \^        Matches at beginning of a line
//
//  \item \$        Matches at end of a line
//
//  \item \.         Matches any single character
//
//  \item  \[ \]       Matches any character(s) inside the brackets
//
//  \item  \[\^ \]      Matches any character(s) not inside the brackets
//
//  \item \-        Matches any character in range on either side of a dash
//
//  \item \*        Matches preceding pattern zero or more times
//
//  \item \+        Matches preceding pattern one or more times
//
//  \item \?        Matches preceding pattern zero or once only
//
//  \item \(\)        Saves a matched expression and uses it in a  later match
// \begin{itemize}
//
// Note that more than one of these metacharacters can be  used in  a  single
// regular expression in order to create complex search patterns. For example,
// the pattern \{ [^ab1-9] }  says  to match  any  character  sequence that
// does not begin with the characters "ab"  followed  by  numbers  in  the
// series  one through nine.
//
//
// In order to use the RegularExpressionSeriesFileNames class we should include
// the following headers as shown.
//
//  \index{itk::ImageSeriesReader!header}
//  \index{itk::RegularExpressionSeriesFileNames!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"
#include "itkRegularExpressionSeriesFileNames.h"
#include "itkPNGImageIO.h"
// Software Guide : EndCodeSnippet


int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "directory regularExression ";
    std::cerr << "sortingExpression outputImageFile " << std::endl;
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


  std::string directory = argv[1];
  std::string regularExpression = argv[2];

  const unsigned int subMatch = atoi( argv[3] );

  std::string outputFilename = argv[4];


  // Software Guide : BeginLatex
  //
  // Then, we declare the filenames generator type and create one instance of it.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RegularExpressionSeriesFileNames    NameGeneratorType;

  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The filenames generator requires us to provide a pattern of text for the
  // regular expression, the sorting expression, as well as the directory where
  // the files are stored.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  nameGenerator->SetRegularExpression( regularExpression );
  nameGenerator->SetSubMatch( subMatch );

  nameGenerator->SetDirectory( directory );
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
  //  The filenames of the input files must be provided to the reader. While the
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
  //  Update() method in the writer. This call must be placed in a try/catch
  //  block since exceptions be potentially be thrown in the process of reading
  //  or writing the images.
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
  //  Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
