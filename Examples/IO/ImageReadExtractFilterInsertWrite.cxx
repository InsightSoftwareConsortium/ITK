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
//  This example illustrates the common task of extracting a 2D slice from a
//  3D volume. Perform some processing on that slice and then paste it on an
//  output volume of the same size as the volume from the input.
//
//  In this example we start by including the appropriate header files.
//
//  Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The filter used to extract a region from an image is the
//  \doxygen{ExtractImageFilter}. Its header is included below.  This filter
//  is capable of extracting a slice from the input image.
//
//  \index{itk::ExtractImageFilter!header}
//
//  Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
#include "itkExtractImageFilter.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The filter used to place the processed image in a region of the output
//  image is the \doxygen{PasteImageFilter}. Its header is included below.
//  This filter is capable of inserting the processed image into the
//  destination image.
//
//  \index{itk::PasteImageFilter!header}
//
//  Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
#include "itkPasteImageFilter.h"
// Software Guide : EndCodeSnippet
// Software Guide : BeginCodeSnippet
#include "itkMedianImageFilter.h"
// Software Guide : EndCodeSnippet
int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc <= 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " input3DImageFile  output3DImageFile " << std::endl;
    std::cerr << " sliceNumber " << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Image types are defined below. Note that the input image type is $3D$ and
  //  the output image type is a $3D$ image as well.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef unsigned char                       InputPixelType;
  typedef unsigned char                       MiddlePixelType;
  typedef unsigned char                       OutputPixelType;
  typedef itk::Image< InputPixelType,  3 >    InputImageType;
  typedef itk::Image< MiddlePixelType, 3 >    MiddleImageType;
  typedef itk::Image< OutputPixelType, 3 >    OutputImageType;
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

  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];

  //  Software Guide : BeginLatex
  //
  //  Below, we create the reader and writer  using the New() method and
  //  assigning the result to a \doxygen{SmartPointer}.
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

  //  Software Guide : BeginLatex
  //
  //  The name of the file to be read or written is passed with the
  //  SetFileName() method.
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
  //  The ExtractImageFilter type is instantiated using the input and
  //  output image types. A filter object is created with the New()
  //  method and assigned to a SmartPointer.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::ExtractImageFilter< InputImageType, MiddleImageType >
    ExtractFilterType;
  ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
  extractFilter->SetDirectionCollapseToSubmatrix();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The ExtractImageFilter requires a region to be defined by the user. The
  //  region is specified by an \doxygen{Index} indicating the pixel where the
  //  region starts and an \doxygen{Size} indication how many pixels the region
  //  has along each dimension. In order to extract a $2D$ image from a $3D$
  //  data set, it is enough to set the size of the region to $1$ in one
  //  dimension. Note that, strictly speaking, we are extracting here a $3D$
  //  image of a single slice. Here we take the region from the buffered region
  //  of the input image. Note that Update() is being called first on the
  //  reader, since otherwise the output would have invalid data.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->Update();
  const InputImageType * inputImage = reader->GetOutput();
  InputImageType::RegionType inputRegion = inputImage->GetBufferedRegion();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We take the size from the region and collapse the size in the $Z$
  //  component by setting its value to $1$.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  InputImageType::SizeType size = inputRegion.GetSize();
  size[2] = 1;
  // Software Guide : EndCodeSnippet
  //  Software Guide : BeginLatex
  //
  //  Note that in this case we are extracting a $Z$ slice, and for that
  //  reason, the dimension to be collapsed in the one with index $2$. You
  //  may keep in mind the association of index components
  //  $\{X=0,Y=1,Z=2\}$. If we were interested in extracting a slice
  //  perpendicular to the $Y$ axis we would have set \code{size[1]=1;}.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  //  Then, we take the index from the region and set its $Z$ value to the
  //  slice number we want to extract. In this example we obtain the slice
  //  number from the command line arguments.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  InputImageType::IndexType start = inputRegion.GetIndex();
  const unsigned int sliceNumber = atoi( argv[3] );
  start[2] = sliceNumber;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally, an \doxygen{ImageRegion} object is created and initialized with
  //  the start and size we just prepared using the slice information.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  InputImageType::RegionType desiredRegion;
  desiredRegion.SetSize(  size  );
  desiredRegion.SetIndex( start );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then the region is passed to the filter using the
  //  SetExtractionRegion() method.
  //
  //  \index{itk::ExtractImageFilter!SetExtractionRegion()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  extractFilter->SetExtractionRegion( desiredRegion );
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginCodeSnippet
  typedef itk::PasteImageFilter< MiddleImageType,
                                 OutputImageType > PasteFilterType;
  PasteFilterType::Pointer pasteFilter = PasteFilterType::New();
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginCodeSnippet
  typedef itk::MedianImageFilter< MiddleImageType,
                                  MiddleImageType > MedianFilterType;
  MedianFilterType::Pointer medianFilter = MedianFilterType::New();
  // Software Guide : EndCodeSnippet
  //  Software Guide : BeginLatex
  //
  //  Below we connect the reader, filter and writer to form the data
  //  processing pipeline.
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  extractFilter->SetInput( inputImage );
  medianFilter->SetInput( extractFilter->GetOutput() );
  pasteFilter->SetSourceImage( medianFilter->GetOutput() );
  pasteFilter->SetDestinationImage( inputImage );
  pasteFilter->SetDestinationIndex( start );
  MiddleImageType::SizeType indexRadius;
  indexRadius[0] = 1; // radius along x
  indexRadius[1] = 1; // radius along y
  indexRadius[2] = 0; // radius along z
  medianFilter->SetRadius( indexRadius );
  medianFilter->UpdateLargestPossibleRegion();
  const MiddleImageType * medianImage = medianFilter->GetOutput();
  pasteFilter->SetSourceRegion( medianImage->GetBufferedRegion() );
  writer->SetInput( pasteFilter->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally we execute the pipeline by invoking Update() on the writer. The
  //  call is placed in a \code{try/catch} block in case exceptions are
  //  thrown.
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
