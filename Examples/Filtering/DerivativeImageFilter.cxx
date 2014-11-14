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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    ARGUMENTS: {DerivativeImageFilterFloatOutput.mhd}
//    OUTPUTS: {DerivativeImageFilterOutput.png}
//    ARGUMENTS:    1 0
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The \doxygen{DerivativeImageFilter} is used for computing the partial
//  derivative of an image, the derivative of an image along a particular axial
//  direction.
//
//  \index{itk::DerivativeImageFilter}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The header file corresponding to this filter should be included first.
//
//  \index{itk::DerivativeImageFilter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkDerivativeImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputImageFile  normalizedOutputImageFile ";
    std::cerr << " derivativeOrder direction" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Next, the pixel types for the input and output images must be defined and, with
  //  them, the image types can be instantiated. Note that it is important to
  //  select a signed type for the image, since the values of the derivatives
  //  will be positive as well as negative.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   float  InputPixelType;
  typedef   float  OutputPixelType;

  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  //  Software Guide : BeginLatex
  //
  //  Using the image types, it is now possible to define the filter type
  //  and create the filter object.
  //
  //  \index{itk::DerivativeImageFilter!instantiation}
  //  \index{itk::DerivativeImageFilter!New()}
  //  \index{itk::DerivativeImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::DerivativeImageFilter<
               InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The order of the derivative is selected with the \code{SetOrder()}
  //  method.  The direction along which the derivative will be computed is
  //  selected with the \code{SetDirection()} method.
  //
  //  \index{itk::DerivativeImageFilter!SetOrder()}
  //  \index{itk::DerivativeImageFilter!SetDirection()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetOrder(     atoi( argv[4] ) );
  filter->SetDirection( atoi( argv[5] ) );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input to the filter can be taken from any other filter, for example
  //  a reader. The output can be passed down the pipeline to other filters,
  //  for example, a writer. An \code{Update()} call on any downstream filter will
  //  trigger the execution of the derivative filter.
  //
  //  \index{itk::DerivativeImageFilter!SetInput()}
  //  \index{itk::DerivativeImageFilter!GetOutput()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{DerivativeImageFilterOutput}
  // \itkcaption[Effect of the Derivative filter.]{Effect of the Derivative filter
  // on a slice from a MRI proton density brain image.}
  // \label{fig:DerivativeImageFilterOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:DerivativeImageFilterOutput} illustrates the effect of
  //  the DerivativeImageFilter on a slice of MRI brain image. The derivative
  //  is taken along the $x$ direction.  The sensitivity to noise in the image
  //  is evident from this result.
  //
  //  Software Guide : EndLatex


  typedef itk::Image< unsigned char, Dimension >  WriteImageType;

  typedef itk::RescaleIntensityImageFilter<
                                  OutputImageType,
                                  WriteImageType >    NormalizeFilterType;

  typedef itk::ImageFileWriter< WriteImageType >       NormalizedWriterType;

  NormalizeFilterType::Pointer normalizer = NormalizeFilterType::New();
  NormalizedWriterType::Pointer normalizedWriter = NormalizedWriterType::New();

  normalizer->SetInput( filter->GetOutput() );
  normalizedWriter->SetInput( normalizer->GetOutput() );

  normalizer->SetOutputMinimum(   0 );
  normalizer->SetOutputMaximum( 255 );

  normalizedWriter->SetFileName( argv[3] );
  normalizedWriter->Update();

  return EXIT_SUCCESS;
}
