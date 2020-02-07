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
//  This example illustrates the use of Mathematical Morphology filters for
//  image enhancement. One of the difficulties of image enhancement is that it
//  is defined based on human visual perception and it is related to a
//  particular set of features that are of interest in the image. In this
//  context, what is considered enhancement for one person, may be seen as
//  image degradation by another person.
//
//  \index{itk::AntiAliasBinaryImageFilter|textbf}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkPNGImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkGrayscaleMorphologicalOpeningImageFilter.h"
#include "itkGrayscaleMorphologicalClosingImageFilter.h"
#include "itkBinaryBallStructuringElement.h"

#include "itkConstrainedValueAdditionImageFilter.h"
#include "itkConstrainedValueDifferenceImageFilter.h"

#include "itkRescaleIntensityImageFilter.h"

int
main(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile radius " << std::endl;
    return EXIT_FAILURE;
  }

  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;
  using WritePixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;
  using WriteImageType = itk::Image<WritePixelType, Dimension>;
  // readers/writers
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<WriteImageType>;

  // structuring element
  using StructuringElementType =
    itk::BinaryBallStructuringElement<PixelType, Dimension>;
  // define the opening and closing types
  using OpeningFilterType =
    itk::GrayscaleMorphologicalOpeningImageFilter<ImageType,
                                                  ImageType,
                                                  StructuringElementType>;
  using ClosingFilterType =
    itk::GrayscaleMorphologicalClosingImageFilter<ImageType,
                                                  ImageType,
                                                  StructuringElementType>;
  // define arithmetic operation filters
  using AdditionFilterType =
    itk::ConstrainedValueAdditionImageFilter<ImageType, ImageType, ImageType>;
  using SubtractionFilterType =
    itk::ConstrainedValueDifferenceImageFilter<ImageType, ImageType, ImageType>;
  // define rescaling filter
  using RescaleFilterType = itk::RescaleIntensityImageFilter<ImageType, WriteImageType>;

  // Create structuring element
  StructuringElementType structuringElement;
  // (argv[3]+1) x (argv[3]+1) structuring element
  structuringElement.SetRadius(std::stoi(argv[3]));
  structuringElement.CreateStructuringElement();

  // Setup the input and output files
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);

  // reading input image
  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Problems reading input image" << std::endl;
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // Create the opening closing filters
  OpeningFilterType::Pointer opening = OpeningFilterType::New();
  ClosingFilterType::Pointer closing = ClosingFilterType::New();
  // Setup the opening and closing methods
  opening->SetKernel(structuringElement);
  closing->SetKernel(structuringElement);
  // Setup minnimum and maximum of rescale filter
  RescaleFilterType::Pointer rescaleFilter = RescaleFilterType::New();
  rescaleFilter->SetOutputMinimum(0);
  rescaleFilter->SetOutputMaximum(255);
  // creation of the pipeline. The enhancement operation is given by:
  // Original Image + Top Hat Image - Bottom Hat Image
  opening->SetInput(reader->GetOutput());
  closing->SetInput(reader->GetOutput());
  SubtractionFilterType::Pointer topHat = SubtractionFilterType::New();
  topHat->SetInput1(reader->GetOutput());
  topHat->SetInput2(opening->GetOutput());
  SubtractionFilterType::Pointer bottomHat = SubtractionFilterType::New();
  bottomHat->SetInput1(closing->GetOutput());
  bottomHat->SetInput2(reader->GetOutput());
  AdditionFilterType::Pointer internalAddition = AdditionFilterType::New();
  internalAddition->SetInput1(reader->GetOutput());
  internalAddition->SetInput2(topHat->GetOutput());

  SubtractionFilterType::Pointer imageEnhancement = SubtractionFilterType::New();
  imageEnhancement->SetInput1(internalAddition->GetOutput());
  imageEnhancement->SetInput2(bottomHat->GetOutput());
  rescaleFilter->SetInput(imageEnhancement->GetOutput());
  writer->SetInput(rescaleFilter->GetOutput());
  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
