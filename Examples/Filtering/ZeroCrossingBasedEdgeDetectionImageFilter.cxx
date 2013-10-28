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
//    OUTPUTS: {ZeroCrossingBasedEdgeDetectionImageFilter.png}
//    ARGUMENTS:    1.0  0.1
//  Software Guide : EndCommandLineArgs
//
//  Software Guide : BeginLatex
//
//  The \doxygen{ZeroCrossingBasedEdgeDetectionImageFilter} performs
//  edge detection by combining a sequence of Gaussian smoothing,
//  Laplacian filter, and Zero cross detections on the Laplacian.
//
//  \index{itk::ZeroCrossingBasedEdgeDetectionImageFilter}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

//  Software Guide : BeginLatex
//
//  The header file corresponding to this filter should be included first.
//
//  \index{itk::ZeroCrossingBasedEdgeDetectionImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkZeroCrossingBasedEdgeDetectionImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkRescaleIntensityImageFilter.h"


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputImageFile variance maxerror" << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginCodeSnippet
  typedef double        InputPixelType;
  typedef double        OutputPixelType;
  typedef unsigned char CharPixelType;

  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  typedef itk::Image< CharPixelType, Dimension >     CharImageType;
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< CharImageType >    WriterType;

  typedef itk::RescaleIntensityImageFilter< OutputImageType, CharImageType>
                                                            RescaleFilterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  typedef itk::ZeroCrossingBasedEdgeDetectionImageFilter< InputImageType, OutputImageType>  FilterType;
  FilterType::Pointer filter = FilterType::New();


  //  Software Guide : BeginLatex
  //
  //  The filter requires two parameters. First the value of the variance to be
  //  used by the Gaussian smoothing stage. This value is provided in the
  //  method \code{SetVariance} and it is given in pixel units. Second the
  //  filter expects the acceptable error for computing the approximation to
  //  the Gaussian kernel. This error is expected to be in the range between 0
  //  and 1. Values outside that range will result in Exceptions being thrown.
  //
  //
  //  \index{itk::ZeroCrossingBasedEdgeDetectionImageFilter!SetVariance}
  //  \index{itk::ZeroCrossingBasedEdgeDetectionImageFilter!SetMaximumError}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetVariance( atof( argv[3] ) );
  filter->SetMaximumError( atof( argv[4] ) );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  As with most filters, we connect the input and output of this
  //  filter in order to create a pipeline. In this particular case the
  //  input is taken from a reader and the output is sent to a writer.
  //  Given that the zero crossing filter is producing a float image as
  //  output, we use a \doxygen{RescaleIntensityImageFilter} to convert
  //  this image to an eight bits image before sending it to the writer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  // Software Guide : EndCodeSnippet

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );

  try
  {
  writer->Update();
  }
  catch( itk::ExceptionObject & excp )
  {
    std::cerr << excp << std::endl;
  }


  return EXIT_SUCCESS;
}
