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
//  This example introduces the use of the
//  \doxygen{CannyEdgeDetectionImageFilter}. This filter is widely used for
//  edge detection since it is the optimal solution satisfying the constraints
//  of good sensitivity, localization and noise robustness.
//
//  \index{itk::CannyEdgeDetectionImageFilter|textbf}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"


//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file.
//
//  \index{itk::CannyEdgeDetectionImageFilter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkCannyEdgeDetectionImageFilter.h"
// Software Guide : EndCodeSnippet


int main(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage [variance upperThreshold lowerThreshold]" << std::endl;
    return EXIT_FAILURE;
    }

  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];
  float variance = 2.0;
  float upperThreshold = 0.0;
  float lowerThreshold = 0.0;

  if( argc > 3 )
    {
    variance = atof( argv[3] );
    }

  if( argc > 4 )
    {
    upperThreshold = atof( argv[4] );
    }

  if( argc > 5 )
    {
    lowerThreshold = atof( argv[5] );
    }

  std::cout << "Variance = " << variance << std::endl;
  std::cout << "UpperThreshold = " << upperThreshold << std::endl;
  std::cout << "LowerThreshold = " << lowerThreshold << std::endl;

  typedef unsigned char    CharPixelType;  //  IO
  typedef double           RealPixelType;  //  Operations
  const   unsigned int     Dimension = 2;

  typedef itk::Image<CharPixelType, Dimension>    CharImageType;
  typedef itk::Image<RealPixelType, Dimension>    RealImageType;

  typedef itk::ImageFileReader< CharImageType >  ReaderType;
  typedef itk::ImageFileWriter< CharImageType >  WriterType;

  //  Software Guide : BeginLatex
  //
  //  This filter operates on image of pixel type float. It is then necessary
  //  to cast the type of the input images that are usually of integer type.
  //  The \doxygen{CastImageFilter} is used here for that purpose. Its image
  //  template parameters are defined for casting from the input type to the
  //  float type using for processing.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter< CharImageType, RealImageType>
                                                         CastToRealFilterType;
  // Software Guide : EndCodeSnippet

  typedef itk::RescaleIntensityImageFilter<RealImageType, CharImageType > RescaleFilter;


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{CannyEdgeDetectionImageFilter} is instantiated using the float image type.
  //
  //  \index{itk::CannyEdgeDetectionImageFilter|textbf}
  //
  //  Software Guide : EndLatex


  typedef itk::CannyEdgeDetectionImageFilter<RealImageType, RealImageType> CannyFilter;

  //Setting the IO

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  CastToRealFilterType::Pointer toReal = CastToRealFilterType::New();
  RescaleFilter::Pointer rescale = RescaleFilter::New();

  //Setting the ITK pipeline filter

  CannyFilter::Pointer cannyFilter = CannyFilter::New();

  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );

  //The output of an edge filter is 0 or 1
  rescale->SetOutputMinimum(   0 );
  rescale->SetOutputMaximum( 255 );

  toReal->SetInput( reader->GetOutput() );

  cannyFilter->SetInput( toReal->GetOutput() );
  cannyFilter->SetVariance( variance );
  cannyFilter->SetUpperThreshold( upperThreshold );
  cannyFilter->SetLowerThreshold( lowerThreshold );

  rescale->SetInput( cannyFilter->GetOutput() );
  writer->SetInput( rescale->GetOutput() );

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


  return EXIT_SUCCESS;

}
