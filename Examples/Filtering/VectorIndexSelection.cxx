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

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkRGBPixel.h"

// Software Guide : EndCodeSnippet


#include "itkImage.h"


int main( int argc, char * argv[] )
{
  // Verify the number of parameters in the command line
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputImageFile component" << std::endl;
    return EXIT_FAILURE;
    }


  // Software Guide : BeginCodeSnippet
  typedef itk::RGBPixel<unsigned char>  InputPixelType;
  typedef unsigned char                 OutputPixelType;
  const   unsigned int                  Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >    InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType  >     ReaderType;
  typedef itk::VectorIndexSelectionCastImageFilter<
                  InputImageType, OutputImageType  >  FilterType;
  typedef itk::ImageFileWriter< OutputImageType >     WriterType;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  filter->SetIndex(atoi(argv[3]));
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


  // Software Guide : BeginCodeSnippet
  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally we trigger the execution of the pipeline with the Update()
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
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet


  return EXIT_SUCCESS;
}
