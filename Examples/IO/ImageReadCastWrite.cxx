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
//  Given that \href{https://www.itk.org}{ITK} is based on the Generic
//  Programming paradigm, most of the types are defined at compilation
//  time. It is sometimes important to anticipate conversion between different
//  types of images. The following example illustrates the common case of
//  reading an image of one pixel type and writing it as a different pixel
//  type. This process not only involves casting but also rescaling the image
//  intensity since the dynamic range of the input and output pixel types can
//  be quite different.  The \doxygen{RescaleIntensityImageFilter} is used
//  here to linearly rescale the image values.
//
//  The first step in this example is to include the appropriate headers.
//
//  \index{itk::ImageFileReader!header}
//  \index{itk::ImageFileWriter!header}
//  \index{itk::RescaleIntensityImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"


int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputImageFile " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Then, as usual, a decision should be made about the pixel type that
  //  should be used to represent the images. Note that when reading an
  //  image, this pixel type \textbf{is not necessarily} the pixel type of
  //  the image stored in the file.  Instead, it is the type that will be
  //  used to store the image as soon as it is read into memory.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float               InputPixelType;
  typedef unsigned char       OutputPixelType;
  const   unsigned int        Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >    InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that the dimension of the image in memory should match the one of
  //  the image in the file. There are a couple of special cases in which this
  //  condition may be relaxed, but in general it is better to ensure that both
  //  dimensions match.
  //
  //  We can now instantiate the types of the reader and writer. These two
  //  classes are parameterized over the image type.
  //
  //  \index{itk::ImageFileReader!Instantiation}
  //  \index{itk::ImageFileWriter!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Below we instantiate the RescaleIntensityImageFilter class that will
  //  linearly scale the image intensities.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RescaleIntensityImageFilter<
                                  InputImageType,
                                  OutputImageType >    FilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  A filter object is constructed and the minimum and maximum values of
  //  the output are selected using the \code{SetOutputMinimum()} and
  //  \code{SetOutputMaximum()} methods.
  //
  //  \index{itk::RescaleIntensityImageFilter!SetOutputMinimum()}
  //  \index{itk::RescaleIntensityImageFilter!SetOutputMaximum()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  filter->SetOutputMinimum(   0 );
  filter->SetOutputMaximum( 255 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, we create the reader and writer and connect the pipeline.
  //
  //  \index{itk::ImageFileReader!New()}
  //  \index{itk::ImageFileWriter!New()}
  //  \index{itk::ImageFileReader!SmartPointer}
  //  \index{itk::ImageFileWriter!SmartPointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  // Software Guide : EndCodeSnippet


  //
  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];


  //  Software Guide : BeginLatex
  //
  //  The name of the files to be read and written are passed with the
  //  \code{SetFileName()} method.
  //
  //  \index{itk::ImageFileReader!SetFileName()}
  //  \index{itk::ImageFileWriter!SetFileName()}
  //  \index{SetFileName()!itk::ImageFileReader}
  //  \index{SetFileName()!itk::ImageFileWriter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally we trigger the execution of the pipeline with the \code{Update()}
  //  method on the writer. The output image will then be the scaled and cast
  //  version of the input image.
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
