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

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkVectorMagnitudeImageFilter.h"
#include "itkGradientVectorFlowImageFilter.h"
#include "itkTestingMacros.h"

int
itkGradientVectorFlowImageFilterTest(int, char *[])
{
  // Define the dimension of the images
  constexpr unsigned int myDimension = 2;

  // Declare gradient type
  using myGradientType = itk::CovariantVector<double, myDimension>;

  // Declare the types of the images
  using myImageType = itk::Image<double, myDimension>;
  using myGradientImageType = itk::Image<myGradientType, myDimension>;

  // Declare the type of the index to access images
  using myIndexType = itk::Index<myDimension>;

  // Declare the type of the size
  using mySizeType = itk::Size<myDimension>;

  // Declare the type of the Region
  using myRegionType = itk::ImageRegion<myDimension>;

  using myLaplacianFilterType = itk::LaplacianImageFilter<myImageType, myImageType>;
  using myGVFFilterType = itk::GradientVectorFlowImageFilter<myGradientImageType, myGradientImageType>;

  using myGFilterType = itk::GradientImageFilter<myImageType, double, double>;

  using myVectorMagnitudeFilterType = itk::VectorMagnitudeImageFilter<myGradientImageType, myImageType>;
  // Create the image
  auto inputImage = myImageType::New();
  auto interImage = myImageType::New();
  auto inter1Image = myImageType::New();


  // Define their size, and start index
  mySizeType size;
  size[0] = 128;
  size[1] = 128;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  // Initialize Image A
  inputImage->SetRegions(region);
  inputImage->Allocate();

  interImage->SetRegions(region);
  interImage->Allocate();

  inter1Image->SetRegions(region);
  inter1Image->Allocate();

  // Declare Iterator types apropriated for each image
  using myIteratorType = itk::ImageRegionIteratorWithIndex<myImageType>;

  using myOutputIteratorType = itk::ImageRegionIteratorWithIndex<myGradientImageType>;

  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it(inputImage, inputImage->GetRequestedRegion());

  // Initialize the content of Image A
  std::cout << "Input Image initialization " << std::endl;

  while (!it.IsAtEnd())
  {
    it.Set(0.0);
    ++it;
  }

  size[0] = 100;
  size[1] = 100;

  start[0] = 14;
  start[1] = 14;

  // Create one iterator for an internal region
  region.SetSize(size);
  region.SetIndex(start);
  myIteratorType itb(inputImage, region);

  // Initialize the content the internal region
  while (!itb.IsAtEnd())
  {
    itb.Set(100.0);
    ++itb;
  }

  // Declare the type for the
  using myFilterType = itk::GradientRecursiveGaussianImageFilter<myImageType, myGradientImageType>;

  // Create a  Filter
  auto filter = myFilterType::New();

  auto gfilter = myGFilterType::New();
  auto gtomfilter = myVectorMagnitudeFilterType::New();

  // Connect the input images
  filter->SetInput(inputImage);

  // Set sigma
  filter->SetSigma(2.0);

  // Execute the filter
  filter->Update();

  std::cout << "Filter: " << filter;

  auto m_LFilter = myLaplacianFilterType::New();
  auto m_GVFFilter = myGVFFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(m_GVFFilter, GradientVectorFlowImageFilter, ImageToImageFilter);


  m_GVFFilter->SetInput(gfilter->GetOutput());

  m_GVFFilter->SetLaplacianFilter(m_LFilter);
  ITK_TEST_SET_GET_VALUE(m_LFilter, m_GVFFilter->GetLaplacianFilter());

  double noiseLevel = 500;
  m_GVFFilter->SetNoiseLevel(noiseLevel);
  ITK_TEST_SET_GET_VALUE(noiseLevel, m_GVFFilter->GetNoiseLevel());

  double timeStep = 0.001;
  m_GVFFilter->SetTimeStep(timeStep);
  ITK_TEST_SET_GET_VALUE(timeStep, m_GVFFilter->GetTimeStep());

  int iterationNum = 2;
  m_GVFFilter->SetIterationNum(iterationNum);
  ITK_TEST_SET_GET_VALUE(iterationNum, m_GVFFilter->GetIterationNum());


  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myGradientImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the output image
  myOutputIteratorType itg(outputImage, outputImage->GetRequestedRegion());

  //  Print the content of the result image
  std::cout << " Result " << std::endl;

  gtomfilter->SetInput(filter->GetOutput());
  gtomfilter->Update();

  gfilter->SetInput(gtomfilter->GetOutput());
  gfilter->Update();

  m_GVFFilter->Update();

  std::cout << m_GVFFilter->GetTimeStep() << std::endl;

  std::cout << m_GVFFilter->GetNoiseLevel() << std::endl;

  std::cout << m_GVFFilter->GetIterationNum() << std::endl;

  myOutputIteratorType itgvf(m_GVFFilter->GetOutput(), m_GVFFilter->GetOutput()->GetRequestedRegion());

  std::cout << "Completed" << std::endl;
  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
