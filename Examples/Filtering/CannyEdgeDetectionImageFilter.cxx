/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
//  \doxygen{CannyEdgeDetectionImageFilter}. Canny edge detection is widely
//  used for edge detection since it is the optimal solution satisfying the
//  constraints of good sensitivity, localization and noise robustness.  To
//  achieve this end, Canny edge detection is implemented internally as a
//  multi-stage algorithm, which involves Gaussian smoothing to remove noise,
//  calculation of gradient magnitudes to localize edge features, non-maximum
//  suppression to remove spurious features, and finally thresholding to yield
//  a binary image. Though the specifics of this internal pipeline are largely
//  abstracted from the user of the class, it is nonetheless beneficial to
//  have a general understanding of these components so that parameters can be
//  appropriately adjusted.
//
//  \index{itk::CannyEdgeDetectionImageFilter|textbf}
//
//  The first step required for using this filter is to include its header
//  file.
//
//  \index{itk::CannyEdgeDetectionImageFilter!header}
//
//  Software Guide : EndLatex

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

// Software Guide : BeginCodeSnippet
#include "itkCannyEdgeDetectionImageFilter.h"
// Software Guide : EndCodeSnippet

int
main(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage"
              << " [variance upperThreshold lowerThreshold]" << std::endl;
    return EXIT_FAILURE;
  }

  const char * inputFilename = argv[1];
  const char * outputFilename = argv[2];

  float variance = 2.0;
  float upperThreshold = 0.0;
  float lowerThreshold = 0.0;

  if (argc > 3)
  {
    variance = std::stod(argv[3]);
  }

  if (argc > 4)
  {
    upperThreshold = std::stod(argv[4]);
  }

  if (argc > 5)
  {
    lowerThreshold = std::stod(argv[5]);
  }

  std::cout << "Variance = " << variance << std::endl;
  std::cout << "UpperThreshold = " << upperThreshold << std::endl;
  std::cout << "LowerThreshold = " << lowerThreshold << std::endl;

  //  Software Guide : BeginLatex
  //
  //  In this example, images are read and written with \code{unsigned char}
  //  pixel type.  However, Canny edge detection requires floating point
  //  pixel types in order to avoid numerical errors.  For this reason,
  //  a separate internal image type with pixel type \code{double} is defined
  //  for edge detection.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  constexpr unsigned int Dimension = 2;
  using CharPixelType = unsigned char; //  IO
  using RealPixelType = double;        //  Operations

  using CharImageType = itk::Image<CharPixelType, Dimension>;
  using RealImageType = itk::Image<RealPixelType, Dimension>;

  //  Software Guide : EndCodeSnippet

  using ReaderType = itk::ImageFileReader<CharImageType>;
  using WriterType = itk::ImageFileWriter<CharImageType>;

  //  Software Guide : BeginLatex
  //
  //  The \code{CharImageType} image is cast to and from \code{RealImageType}
  //  using \doxygen{CastImageFilter} and \code{RescaleIntensityImageFilter},
  //  respectively; both the input and output of
  //  \code{CannyEdgeDetectionImageFilter} are \code{RealImageType}.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  using CastToRealFilterType =
    itk::CastImageFilter<CharImageType, RealImageType>;
  using CannyFilterType =
    itk::CannyEdgeDetectionImageFilter<RealImageType, RealImageType>;
  using RescaleFilterType =
    itk::RescaleIntensityImageFilter<RealImageType, CharImageType>;

  //  Software Guide : EndCodeSnippet

  // Setting the IO

  auto reader = ReaderType::New();
  auto toReal = CastToRealFilterType::New();
  auto cannyFilter = CannyFilterType::New();
  auto rescale = RescaleFilterType::New();
  auto writer = WriterType::New();

  reader->SetFileName(inputFilename);
  writer->SetFileName(outputFilename);

  toReal->SetInput(reader->GetOutput());
  cannyFilter->SetInput(toReal->GetOutput());
  rescale->SetInput(cannyFilter->GetOutput());
  writer->SetInput(rescale->GetOutput());

  //  Software Guide : BeginLatex
  //
  //  In this example, three parameters of the Canny edge detection
  //  filter may be set via the \code{SetVariance()},
  //  \code{SetUpperThreshold()}, and \code{SetLowerThreshold()} methods.
  //  Based on the previous discussion of the steps in the internal pipeline,
  //  we understand that \code{variance} adjusts the amount of Gaussian
  //  smoothing and \code{upperThreshold} and \code{lowerThreshold} control
  //  which edges are selected in the final step.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  cannyFilter->SetVariance(variance);
  cannyFilter->SetUpperThreshold(upperThreshold);
  cannyFilter->SetLowerThreshold(lowerThreshold);

  //  Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally, \code{Update()} is called on \code{writer} to trigger
  //  execution of the pipeline.  As usual, the call is wrapped in a
  //  \code{try/catch} block.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
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

  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
