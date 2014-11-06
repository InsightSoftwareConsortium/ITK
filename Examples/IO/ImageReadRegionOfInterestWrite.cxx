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
//  This example should arguably be placed in the previous filtering
//  chapter. However its usefulness for typical IO operations makes it
//  interesting to mention here. The purpose of this example is to read an
//  image, extract a subregion and write this subregion to a file. This is a
//  common task when we want to apply a computationally intensive method to
//  the region of interest of an image.
//
//  As usual with ITK IO, we begin by including the appropriate header files.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The \doxygen{RegionOfInterestImageFilter} is the filter used to extract a
//  region from an image. Its header is included below.
//
//  \index{itk::RegionOfInterestImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRegionOfInterestImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"


int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc < 7 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputImageFile " << std::endl;
    std::cerr << " startX startY sizeX sizeY" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Image types are defined below.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef signed short        InputPixelType;
  typedef signed short        OutputPixelType;
  const   unsigned int        Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >    InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The types for the \doxygen{ImageFileReader} and \doxygen{ImageFileWriter}
  //  are instantiated using the image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The RegionOfInterestImageFilter type is instantiated using
  //  the input and output image types. A filter object is created with the
  //  \code{New()} method and assigned to a \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RegionOfInterestImageFilter< InputImageType,
                                            OutputImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The RegionOfInterestImageFilter requires a region to be
  //  defined by the user. The region is specified by an \doxygen{Index}
  //  indicating the pixel where the region starts and an \doxygen{Size}
  //  indicating how many pixels the region has along each dimension. In this
  //  example, the specification of the region is taken from the command line
  //  arguments (this example assumes that a 2D image is being processed).
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  OutputImageType::IndexType start;
  start[0] = atoi( argv[3] );
  start[1] = atoi( argv[4] );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  OutputImageType::SizeType size;
  size[0] = atoi( argv[5] );
  size[1] = atoi( argv[6] );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  An \doxygen{ImageRegion} object is created and initialized with start
  //  and size obtained from the command line.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  OutputImageType::RegionType desiredRegion;
  desiredRegion.SetSize(  size  );
  desiredRegion.SetIndex( start );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then the region is passed to the filter using the
  //  \code{SetRegionOfInterest()} method.
  //
  //  \index{itk::RegionOfInterestImageFilter!SetRegionOfInterest()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->SetRegionOfInterest( desiredRegion );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Below, we create the reader and writer using the \code{New()} method and
  //  assign the result to a \doxygen{SmartPointer}.
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
  // Software Guide : EndCodeSnippet


  //
  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];


  //  Software Guide : BeginLatex
  //
  //  The name of the file to be read or written is passed with the
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
  //  Below we connect the reader, filter and writer to form the data
  //  processing pipeline.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally we execute the pipeline by invoking \code{Update()} on the
  //  writer. The call is placed in a \code{try/catch} block in case exceptions
  //  are thrown.
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
