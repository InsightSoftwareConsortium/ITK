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
//  This example illustrates the use of the \doxygen{GradientRecursiveGaussianImageFilter}.
//
//  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header
//  file.
//
//  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGradientRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputVectorImageFile   sigma" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Types should be instantiated based on the pixels of the input and
  //  output images.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int  Dimension = 3;
  typedef    float    InputPixelType;
  typedef    float    OutputComponentPixelType;

  typedef itk::CovariantVector<
    OutputComponentPixelType, Dimension > OutputPixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  With them, the input and output image types can be instantiated.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types.
  //
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientRecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!New()}
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The standard deviation of the Gaussian smoothing kernel is now set.
  //
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!SetSigma()}
  //  \index{SetSigma()!itk::Gradient\-Recursive\-Gaussian\-Image\-Filter}
  //
  //  Software Guide : EndLatex
  const double sigma = atof( argv[3] );


  // Software Guide : BeginCodeSnippet
  filter->SetSigma( sigma );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the filter is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::Gradient\-Recursive\-Gaussian\-Image\-Filter!Update()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  If connected to other filters in a pipeline, this filter will
  //  automatically update when any downstream filters are updated.  For
  //  example, we may connect this gradient magnitude filter to an image file
  //  writer and then update the writer.
  //
  //  Software Guide : EndLatex


  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );


  // Software Guide : BeginCodeSnippet
  writer->SetInput( filter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
