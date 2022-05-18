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

// Software Guide : BeginLatex
//
// The following example illustrates how to preprocess and segment images
// using the \doxygen{WatershedImageFilter} for the particular case of
// grayscale scalar image.
//
// Software Guide : EndLatex

#include "itkWatershedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarToRGBPixelFunctor.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"


int
main(int argc, char * argv[])
{


  if (argc < 5)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage lowerThreshold  outputScaleLevel"
              << std::endl;
    return EXIT_FAILURE;
  }

  using InternalPixelType = float;
  using RGBPixelType = itk::RGBPixel<unsigned char>;

  constexpr unsigned int Dimension = 3;

  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using RGBImageType = itk::Image<RGBPixelType, Dimension>;


  //
  // We instantiate reader and writer types
  //
  using ReaderType = itk::ImageFileReader<InternalImageType>;
  using WriterType = itk::ImageFileWriter<RGBImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);


  //
  //  Instantiate the GradientMagnitude image filter
  //
  using GradientMagnitudeFilterType =
    itk::GradientMagnitudeRecursiveGaussianImageFilter<InternalImageType,
                                                       InternalImageType>;

  auto gradienMagnitudeFilter = GradientMagnitudeFilterType::New();

  gradienMagnitudeFilter->SetInput(reader->GetOutput());
  gradienMagnitudeFilter->SetSigma(1.0);


  //
  //  Instantiate the Watershed filter
  //

  using WatershedFilterType = itk::WatershedImageFilter<InternalImageType>;

  auto watershedFilter = WatershedFilterType::New();

  watershedFilter->SetInput(gradienMagnitudeFilter->GetOutput());

  watershedFilter->SetThreshold(std::stod(argv[3]));
  watershedFilter->SetLevel(std::stod(argv[4]));


  //
  //  Instantiate the filter that will encode the label image
  //  into a color image (random color attribution).
  //

  using ColormapFunctorType =
    itk::Functor::ScalarToRGBPixelFunctor<unsigned long>;

  using LabeledImageType = WatershedFilterType::OutputImageType;

  using ColormapFilterType =
    itk::UnaryFunctorImageFilter<LabeledImageType,
                                 RGBImageType,
                                 ColormapFunctorType>;

  auto colorMapFilter = ColormapFilterType::New();

  colorMapFilter->SetInput(watershedFilter->GetOutput());

  writer->SetInput(colorMapFilter->GetOutput());

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
