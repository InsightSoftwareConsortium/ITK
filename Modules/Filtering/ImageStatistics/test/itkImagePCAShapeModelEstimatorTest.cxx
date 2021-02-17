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

// Insight classes
#include "itkLightProcessObject.h"
#include "itkTextOutput.h"

#include "itkImagePCAShapeModelEstimator.h"
#include "itkPrintHelper.h"
#include "itkTestingMacros.h"

using namespace itk::print_helper;

// class to support progress feedback


class ShowProgressObject
{
public:
  ShowProgressObject(itk::LightProcessObject * o) { m_Process = o; }
  void
  ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  itk::LightProcessObject::Pointer m_Process;
};


int
itkImagePCAShapeModelEstimatorTest(int, char *[])
{
  // Data definitions
  constexpr itk::Size<2u>::SizeValueType IMGWIDTH = 2;
  constexpr itk::Size<2u>::SizeValueType IMGHEIGHT = 2;
  constexpr unsigned int                 NDIMENSION = 2;
  constexpr unsigned int                 NUMTRAINIMAGES = 3;
  constexpr unsigned int                 NUMLARGESTPC = 2;

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  // Create 3 simple test images with
  using InputImageType = itk::Image<double, NDIMENSION>;
  using OutputImageType = itk::Image<double, NDIMENSION>;

  using InputImageIterator = itk::ImageRegionIterator<InputImageType>;
  using OutputImageIterator = itk::ImageRegionIterator<OutputImageType>;

  InputImageType::Pointer image1 = InputImageType::New();

  InputImageType::Pointer image2 = InputImageType::New();

  InputImageType::Pointer image3 = InputImageType::New();

  InputImageType::SizeType inputImageSize = { { IMGWIDTH, IMGHEIGHT } };

  InputImageType::IndexType index;
  index.Fill(0);
  InputImageType::RegionType region;

  region.SetSize(inputImageSize);
  region.SetIndex(index);

  // Set up Image 1 first

  image1->SetLargestPossibleRegion(region);
  image1->SetBufferedRegion(region);
  image1->Allocate();

  // Set up the iterators
  InputImageIterator image1It(image1, image1->GetBufferedRegion());

  // Set up Image 2 first

  image2->SetLargestPossibleRegion(region);
  image2->SetBufferedRegion(region);
  image2->Allocate();

  // Set up the iterators
  InputImageIterator image2It(image2, image2->GetBufferedRegion());

  // Set up Image 3 first

  image3->SetLargestPossibleRegion(region);
  image3->SetBufferedRegion(region);
  image3->Allocate();

  // Set up the iterators
  InputImageIterator image3It(image3, image3->GetBufferedRegion());

  // Manually create and store each vector

  // Image no. 1
  for (int i = 0; i < 4; i++)
  {
    image1It.Set(1);
    ++image1It;
  }
  // Image no. 2
  image2It.Set(2);
  ++image2It;
  image2It.Set(0);
  ++image2It;
  image2It.Set(0);
  ++image2It;
  image2It.Set(2);
  ++image2It;

  // Image no. 3
  image3It.Set(0);
  ++image3It;
  image3It.Set(3);
  ++image3It;
  image3It.Set(3);
  ++image3It;
  image3It.Set(0);
  ++image3It;

  // Test code for the Shape model estimator

  // Set the image model estimator
  using ImagePCAShapeModelEstimatorType = itk::ImagePCAShapeModelEstimator<InputImageType, OutputImageType>;

  ImagePCAShapeModelEstimatorType::Pointer applyPCAShapeEstimator = ImagePCAShapeModelEstimatorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(applyPCAShapeEstimator, ImagePCAShapeModelEstimator, ImageShapeModelEstimatorBase);

  // Set the parameters of the clusterer
  applyPCAShapeEstimator->SetNumberOfTrainingImages(NUMTRAINIMAGES);
  applyPCAShapeEstimator->SetNumberOfPrincipalComponentsRequired(NUMLARGESTPC + 1);
  applyPCAShapeEstimator->SetNumberOfPrincipalComponentsRequired(NUMLARGESTPC);
  applyPCAShapeEstimator->SetInput(0, image1);
  applyPCAShapeEstimator->SetInput(1, image2);
  applyPCAShapeEstimator->SetInput(2, image3);

  applyPCAShapeEstimator->Update();

  // Print out the number of training images and the number of principal
  // components
  std::cout << "The number of training images are: " << applyPCAShapeEstimator->GetNumberOfTrainingImages()
            << std::endl;

  std::cout << "The number of principal components desired are: "
            << applyPCAShapeEstimator->GetNumberOfPrincipalComponentsRequired() << std::endl;

  // Print the eigen vectors
  vnl_vector<double> eigenValues = applyPCAShapeEstimator->GetEigenValues();
  unsigned int       numEigVal = eigenValues.size();
  std::cout << "Number of returned eign-values: " << numEigVal << std::endl;

  std::cout << "The " << applyPCAShapeEstimator->GetNumberOfPrincipalComponentsRequired()
            << " largest eigen values are:" << std::endl;

  for (unsigned int i = 0; i < std::min(numEigVal, NUMLARGESTPC); i++)
  {
    std::cout << eigenValues[i] << std::endl;
  }
  std::cout << "" << std::endl;
  std::cout << "" << std::endl;


  // Print the MeanImage
  OutputImageType::Pointer outImage = applyPCAShapeEstimator->GetOutput(0);
  OutputImageIterator      outImageIt(outImage, outImage->GetBufferedRegion());
  outImageIt.GoToBegin();

  std::cout << "The mean image is:" << std::endl;
  while (!outImageIt.IsAtEnd())
  {
    std::cout << (double)(outImageIt.Get()) << ";" << std::endl;
    ++outImageIt;
  }
  std::cout << "  " << std::endl;

  // Print the largest two eigen vectors
  for (unsigned int j = 1; j < NUMLARGESTPC + 1; j++)
  {
    OutputImageType::Pointer outImage2 = applyPCAShapeEstimator->GetOutput(j);
    OutputImageIterator      outImage2It(outImage2, outImage2->GetBufferedRegion());
    outImage2It.GoToBegin();

    std::cout << "" << std::endl;
    std::cout << "The eigen vector number: " << j << " is:" << std::endl;
    while (!outImage2It.IsAtEnd())
    {
      std::cout << (double)(outImage2It.Get()) << ";" << std::endl;
      ++outImage2It;
    }
    std::cout << "  " << std::endl;
  }

  // Test for the eigen values for the test case precomputed using Matlab/Splus
  std::cout << "" << std::endl;
  if ((eigenValues[2] < 6 || eigenValues[2] > 6.1) || (eigenValues[1] > 0.1))
  {
    std::cout << "Test Passed" << std::endl;
  }
  else
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
