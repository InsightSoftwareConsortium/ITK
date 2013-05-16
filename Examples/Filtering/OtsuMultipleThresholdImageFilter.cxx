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
//    INPUTS: {BrainProtonDensitySlice.png}
//    ARGUMENTS:   OtsuMultipleThresholdsOutput png 4
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
// This example illustrates how to use the \doxygen{OtsuMultipleThresholdsCalculator}.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkOtsuMultipleThresholdsCalculator.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarImageToHistogramGenerator.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkNumericTraits.h"

#include <stdio.h>
int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFileBase ";
    std::cerr << "  outputImageFileExtension numberOfThresholdsToCalculate "  << std::endl;
    return EXIT_FAILURE;
    }

  //Convenience typedefs
  typedef  unsigned short  InputPixelType;
  typedef  unsigned char   OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;

  // Software Guide : BeginLatex
  //
  // OtsuMultipleThresholdsCalculator calculates thresholds for a given
  // histogram so as to maximize the between-class variance. We use
  // ScalarImageToHistogramGenerator to generate histograms. The histogram type
  // defined by the generator is then used for instantiating the type of the
  // Otsu threshold calculator.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ScalarImageToHistogramGenerator<
                         InputImageType > ScalarImageToHistogramGeneratorType;

  typedef ScalarImageToHistogramGeneratorType::HistogramType HistogramType;

  typedef itk::OtsuMultipleThresholdsCalculator< HistogramType >
                                                               CalculatorType;
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  // Software Guide : BeginLatex
  //
  // Once thresholds are computed we will use BinaryThresholdImageFilter to
  // segment the input image into segments.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryThresholdImageFilter<
  InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet

  //Create using static New() method

  // Software Guide : BeginCodeSnippet
  ScalarImageToHistogramGeneratorType::Pointer scalarImageToHistogramGenerator
    = ScalarImageToHistogramGeneratorType::New();

  CalculatorType::Pointer calculator = CalculatorType::New();
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  //Set Properties

  // Software Guide : BeginCodeSnippet
  scalarImageToHistogramGenerator->SetNumberOfBins( 128 );
  calculator->SetNumberOfThresholds( atoi( argv[4] ) );
  // Software Guide : EndCodeSnippet

  const OutputPixelType outsideValue = 0;
  const OutputPixelType insideValue = 255;

  filter->SetOutsideValue( outsideValue );
  filter->SetInsideValue(  insideValue  );

  //Connect Pipeline
  reader->SetFileName( argv[1] );

  // Software Guide : BeginLatex
  // The pipeline will look as follows:
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  scalarImageToHistogramGenerator->SetInput( reader->GetOutput() );
  calculator->SetInputHistogram(
                               scalarImageToHistogramGenerator->GetOutput() );
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  // Software Guide : EndCodeSnippet


  //Invoke pipeline
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading image" << excp << std::endl;
    }
  scalarImageToHistogramGenerator->Compute();

  try
    {
    calculator->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << excp << std::endl;
    }

  // Software Guide : BeginLatex
  // Thresholds are obtained using the \code{GetOutput} method
  // \index{itk::OtsuMultipleThresholdsCalculator!GetOutput()}
  // Software Guide : EndLatex
  //Get Thresholds
  // Software Guide : BeginCodeSnippet
  const CalculatorType::OutputType &thresholdVector = calculator->GetOutput();
  CalculatorType::OutputType::const_iterator itNum = thresholdVector.begin();
  // Software Guide : EndCodeSnippet

  //Threshold into separate segments and write out as binary images
  std::string outputFileBase = argv[2];
  std::string outputFile;

  InputPixelType lowerThreshold = 0;
  InputPixelType upperThreshold;

  char outputFilename[1000];
  outputFile = outputFileBase + "%03d.";
  outputFile += argv[3];   // filename extension


  // Software Guide : BeginCodeSnippet
  for(; itNum < thresholdVector.end(); itNum++)
    {
    std::cout << "OtsuThreshold["
              << (int)(itNum - thresholdVector.begin())
              << "] = "
              << static_cast<itk::NumericTraits<
                          CalculatorType::MeasurementType>::PrintType>(*itNum)
              << std::endl;
    // Software Guide : EndCodeSnippet

    upperThreshold = static_cast<InputPixelType>(*itNum);

    filter->SetLowerThreshold( lowerThreshold );
    filter->SetUpperThreshold( upperThreshold );

    lowerThreshold = upperThreshold;

    sprintf (outputFilename, outputFile.c_str(), (itNum - thresholdVector.begin()));
    writer->SetFileName( outputFilename );


    try
      {
      writer->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << excp << std::endl;
      }
    // Software Guide : BeginCodeSnippet
    }
  // Software Guide : EndCodeSnippet

  // Also write out the image thresholded between the upper threshold and
  // the max intensity.
  upperThreshold = itk::NumericTraits<InputPixelType>::max();
  filter->SetLowerThreshold( lowerThreshold );
  filter->SetUpperThreshold( upperThreshold );

  sprintf (outputFilename, outputFile.c_str(), (thresholdVector.size() ));
  writer->SetFileName( outputFilename );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << excp << std::endl;
    }

  return EXIT_SUCCESS;
}

