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
//  This example illustrates how to convert an RGB image into a grayscale one.
//  The \doxygen{RGBToLuminanceImageFilter} is the central piece of this
//  example.
//
//  \index{itk::RGBToLuminanceImageFilter!RGB Images}
//
//  Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::RGBToLuminanceImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRGBToLuminanceImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkRGBPixel.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int
main(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputRGBImageFile  outputGrayscaleImageFile "
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = itk::RGBPixel<unsigned char>;
  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<unsigned char, Dimension>;


  using ReaderType = itk::ImageFileReader<InputImageType>;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(argv[1]);


  using FilterType =
    itk::RGBToLuminanceImageFilter<InputImageType, OutputImageType>;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(reader->GetOutput());


  using WriterType = itk::ImageFileWriter<OutputImageType>;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(filter->GetOutput());

  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception Thrown" << std::endl;
    std::cerr << excp << std::endl;
  }

  return EXIT_SUCCESS;
}
