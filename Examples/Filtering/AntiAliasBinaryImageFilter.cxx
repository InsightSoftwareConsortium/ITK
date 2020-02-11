/*=========================================================================
 *
 *  Copyright NumFOCUS
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
//  This example introduces the use of the \doxygen{AntiAliasBinaryImageFilter}. This
//  filter expect a binary mask as input, and using Level Sets it smooths the
//  image by keeping the edge of the structure within 1 pixel distance from the
//  original location. It is usually desirable to run this filter before
//  extracting isocontour with surface extraction methods.
//
//  \index{itk::AntiAliasBinaryImageFilter|textbf}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"


//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::AntiAliasBinaryImageFilter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkAntiAliasBinaryImageFilter.h"
// Software Guide : EndCodeSnippet


int
main(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImageDoublePixelType ";
    std::cerr << " outputImage8BitsPixelType [RMS] [numberOfIterations]" << std::endl;
    return EXIT_FAILURE;
  }

  const char * inputFilename = argv[1];
  const char * outputFilename1 = argv[2];
  const char * outputFilename2 = argv[3];

  double       maximumRMSError = 0.01;
  unsigned int numberOfIterations = 50;

  if (argc > 4)
  {
    maximumRMSError = std::stod(argv[4]);
  }

  if (argc > 5)
  {
    numberOfIterations = std::stoi(argv[5]);
  }


  using CharPixelType = unsigned char; //  IO
  using RealPixelType = double;        //  Operations
  constexpr unsigned int Dimension = 3;

  using CharImageType = itk::Image<CharPixelType, Dimension>;
  using RealImageType = itk::Image<RealPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<CharImageType>;
  using WriterType = itk::ImageFileWriter<CharImageType>;

  using RealWriterType = itk::ImageFileWriter<RealImageType>;

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
  using CastToRealFilterType = itk::CastImageFilter<CharImageType, RealImageType>;
  // Software Guide : EndCodeSnippet

  using RescaleFilter = itk::RescaleIntensityImageFilter<RealImageType, CharImageType>;


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{AntiAliasBinaryImageFilter} is instantiated using the float image
  //  type.
  //
  //  \index{itk::AntiAliasBinaryImageFilter|textbf}
  //
  //  Software Guide : EndLatex


  using AntiAliasFilterType =
    itk::AntiAliasBinaryImageFilter<RealImageType, RealImageType>;

  // Setting the IO

  ReaderType::Pointer reader = ReaderType::New();

  CastToRealFilterType::Pointer toReal = CastToRealFilterType::New();
  RescaleFilter::Pointer        rescale = RescaleFilter::New();

  // Setting the ITK pipeline filter

  // Software Guide : BeginCodeSnippet
  AntiAliasFilterType::Pointer antiAliasFilter = AntiAliasFilterType::New();

  reader->SetFileName(inputFilename);

  // The output of an edge filter is 0 or 1
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  toReal->SetInput(reader->GetOutput());

  antiAliasFilter->SetInput(toReal->GetOutput());
  antiAliasFilter->SetMaximumRMSError(maximumRMSError);
  antiAliasFilter->SetNumberOfIterations(numberOfIterations);
  antiAliasFilter->SetNumberOfLayers(2);

  RealWriterType::Pointer realWriter = RealWriterType::New();
  realWriter->SetInput(antiAliasFilter->GetOutput());
  realWriter->SetFileName(outputFilename1);

  try
  {
    realWriter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }


  WriterType::Pointer rescaledWriter = WriterType::New();
  rescale->SetInput(antiAliasFilter->GetOutput());
  rescaledWriter->SetInput(rescale->GetOutput());
  rescaledWriter->SetFileName(outputFilename2);
  try
  {
    rescaledWriter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "Completed in " << antiAliasFilter->GetNumberOfIterations() << std::endl;

  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
