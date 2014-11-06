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
//  This example illustrates how to write an image whose pixel type is
//  \code{CovariantVector}. For practical purposes all the content in this
//  example is applicable to images of pixel type \doxygen{Vector},
//  \doxygen{Point} and \doxygen{FixedArray}. These pixel types are similar
//  in that they are all arrays of fixed size in which the components have
//  the same representational type.
//
//  In order to make this example a bit more interesting we setup a pipeline
//  to read an image, compute its gradient and write the gradient to a file.
//  Gradients are represented with \doxygen{CovariantVector}s as opposed to
//  Vectors. In this way, gradients are transformed correctly under
//  \doxygen{AffineTransform}s or in general, any transform having
//  anisotropic scaling.
//
//  Let's start by including the relevant header files.
//
//  \index{ImageFileWriter!Vector images}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  We use the \doxygen{GradientRecursiveGaussianImageFilter} in order to
//  compute the image gradient. The output of this filter is an image whose
//  pixels are CovariantVectors.
//
//  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGradientRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputVectorImageFile " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We read an image of \code{signed short} pixels and compute the
  //  gradient to produce an image of CovariantVectors where each
  //  component is of type \code{float}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef signed short          InputPixelType;
  typedef float                 ComponentType;
  const   unsigned int          Dimension = 2;

  typedef itk::CovariantVector< ComponentType,
                                    Dimension  >      OutputPixelType;

  typedef itk::Image< InputPixelType,  Dimension >    InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{ImageFileReader} and \doxygen{ImageFileWriter} are
  //  instantiated using the image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The GradientRecursiveGaussianImageFilter class is instantiated
  //  using the input and output image types. A filter object is created with
  //  the \code{New()} method and assigned to a \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientRecursiveGaussianImageFilter<
                                          InputImageType,
                                          OutputImageType    > FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We select a value for the $\sigma$ parameter of the
  //  GradientRecursiveGaussianImageFilter. Note that $\sigma$
  //  for this filter is specified in millimeters.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->SetSigma( 1.5 );      // Sigma in millimeters
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
  //  The name of the file to be read or written is passed to the
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
  //  Finally we execute the pipeline by invoking \code{Update()} on the writer.
  //  The call is placed in a \code{try/catch} block in case exceptions are
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
