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

#include "itkHessianToObjectnessMeasureImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkHessianRecursiveGaussianImageFilter.h"
#include "itkTestingMacros.h"


int
itkHessianToObjectnessMeasureImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage"
              << " outputImage [ObjectDimension] [Bright/Dark]" << std::endl;
    return EXIT_FAILURE;
  }

  // Define the dimension of the images
  constexpr unsigned char Dimension = 2;

  using PixelType = float;

  // Declare the types of the images
  using ImageType = itk::Image<PixelType, Dimension>;

  using FileReaderType = itk::ImageFileReader<ImageType>;

  // Declare the type of the recursive Gaussian filter
  using GaussianImageFilterType = itk::HessianRecursiveGaussianImageFilter<ImageType>;

  using HessianImageType = GaussianImageFilterType::OutputImageType;

  // Delcare the type of objectness measure image filter

  using ObjectnessFilterType = itk::HessianToObjectnessMeasureImageFilter<HessianImageType, ImageType>;

  FileReaderType::Pointer imageReader = FileReaderType::New();
  imageReader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(imageReader->Update());


  // Create a Gaussian filter
  GaussianImageFilterType::Pointer gaussianFilter = GaussianImageFilterType::New();

  // Create an objectness filter
  ObjectnessFilterType::Pointer objectnessFilter = ObjectnessFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(objectnessFilter, HessianToObjectnessMeasureImageFilter, ImageToImageFilter);


  // Connect the input images
  gaussianFilter->SetInput(imageReader->GetOutput());
  objectnessFilter->SetInput(gaussianFilter->GetOutput());

  // Set the filter properties
  bool scaleObjectnessMeasure = false;
  ITK_TEST_SET_GET_BOOLEAN(objectnessFilter, ScaleObjectnessMeasure, scaleObjectnessMeasure);

  bool brightObject = true;
  ITK_TEST_SET_GET_BOOLEAN(objectnessFilter, BrightObject, brightObject);

  double alphaValue = 0.5;
  objectnessFilter->SetAlpha(alphaValue);
  ITK_TEST_SET_GET_VALUE(alphaValue, objectnessFilter->GetAlpha());

  double betaValue = 0.5;
  objectnessFilter->SetBeta(betaValue);
  ITK_TEST_SET_GET_VALUE(betaValue, objectnessFilter->GetBeta());

  double gammaValue = 0.5;
  objectnessFilter->SetGamma(gammaValue);
  ITK_TEST_SET_GET_VALUE(gammaValue, objectnessFilter->GetGamma());


  // Check that an exception is thrown if the object dimension is larger than
  // the image dimension
  objectnessFilter->SetObjectDimension(3);

  ITK_TRY_EXPECT_EXCEPTION(objectnessFilter->Update());


  if (argc >= 3)
  {
    unsigned int objectDimension = std::stoi(argv[3]);
    objectnessFilter->SetObjectDimension(objectDimension);
    ITK_TEST_SET_GET_VALUE(objectDimension, objectnessFilter->GetObjectDimension());
  }

  if (argc >= 4)
  {
    brightObject = std::stoi(argv[4]);
    objectnessFilter->SetBrightObject(brightObject);
    ITK_TEST_SET_GET_VALUE(brightObject, objectnessFilter->GetBrightObject());
  }


  ITK_TRY_EXPECT_NO_EXCEPTION(objectnessFilter->Update());


  // Write the output image
  using FileWriterType = itk::ImageFileWriter<ImageType>;
  FileWriterType::Pointer writer = FileWriterType::New();
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();
  writer->SetInput(objectnessFilter->GetOutput());


  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
