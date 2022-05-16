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

#include <iostream>
#include "itkDisplacementFieldJacobianDeterminantFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkStdStreamStateSave.h"
#include "itkTestingMacros.h"


static bool
TestDisplacementJacobianDeterminantValue()
{
  bool                   testPassed = true;
  constexpr unsigned int ImageDimension = 2;

  using VectorType = itk::Vector<float, ImageDimension>;
  using FieldType = itk::Image<VectorType, ImageDimension>;

  // In this case, the image to be warped is also a vector field.
  using VectorImageType = FieldType;


  std::cout << "Create the dispacementfield image pattern." << std::endl;
  VectorImageType::RegionType region;
  // NOTE:  Making the image size much larger than necessary in order to get
  //       some meaningful time measurements.  Simulate a 256x256x256 image.
  VectorImageType::SizeType size = { { 4096, 4096 } };
  region.SetSize(size);

  auto dispacementfield = VectorImageType::New();
  dispacementfield->SetLargestPossibleRegion(region);
  dispacementfield->SetBufferedRegion(region);
  dispacementfield->Allocate();

  VectorType values;
  values[0] = 0;
  values[1] = 0;
  using Iterator = itk::ImageRegionIteratorWithIndex<VectorImageType>;
  for (Iterator inIter(dispacementfield, region); !inIter.IsAtEnd(); ++inIter)
  {
    const unsigned int i = inIter.GetIndex()[0];
    const unsigned int j = inIter.GetIndex()[1];
    values[0] = 0.125 * i * i + 0.125 * j;
    values[1] = 0.125 * i * j + 0.25 * j;
    inIter.Set(values);
    // std::cout << "Setting: " << values << " at " << inIter.GetIndex() << std::endl;
  }

  // Displacement field:
  //|-------------------------------------------|
  //| [0.25;0.5]   | [0.375;0.75] | [0.75;1]    |
  //|-------------------------------------------|
  //| [0.125;0.25] | [0.25;0.375] | [0.625;0.5] |
  //|-------------------------------------------|
  //| [0.0;0.0]    | [0.125;0.0]  | [0.5;0]     |
  //|-------------------------------------------|
  //
  // J(1,1) = [ (.625-.125)/2 (.5-.25)/2; (.375-.125)/2 (.75-0.0)/2] =[ .25  .125; .125 .375]
  // det((J+I)(1,1))=((.25+1.0)*(.375+1.0))-(.125*.125) = 1.703125;
  const float expectedJacobianDeterminant = (((.25 + 1.0) * (.375 + 1.0)) - (.125 * .125));

  using FilterType = itk::DisplacementFieldJacobianDeterminantFilter<VectorImageType, float>;
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, DisplacementFieldJacobianDeterminantFilter, ImageToImageFilter);


  bool useImageSpacing = true;
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  if (useImageSpacing)
  {
    filter->SetUseImageSpacingOn();
  }
  else
  {
    filter->SetUseImageSpacingOff();
  }
#endif
  ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);

  filter->SetInput(dispacementfield);
  filter->Update();


  itk::Image<float, 2>::Pointer output = filter->GetOutput();

  VectorImageType::IndexType index;
  index[0] = 1;
  index[1] = 1;
  float jacobianDeterminant = output->GetPixel(index);
  // std::cout << "Output "  << output->GetPixel(index) << std::endl;
  double epsilon = 1e-13;
  if (itk::Math::abs(jacobianDeterminant - expectedJacobianDeterminant) > epsilon)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in pixel value at index [" << index << "]" << std::endl;
    std::cerr << "Expected value " << jacobianDeterminant << std::endl;
    std::cerr << " differs from " << expectedJacobianDeterminant;
    std::cerr << " by more than " << epsilon << std::endl;
    testPassed = false;
  }
  else
  {
    std::cout << "Test passed." << std::endl;
  }

  return testPassed;
}

int
itkDisplacementFieldJacobianDeterminantFilterTest(int, char *[])
{
  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope
  // scope.
  itk::StdStreamStateSave coutState(std::cout);

  bool ValueTestPassed = TestDisplacementJacobianDeterminantValue();
  try
  {
    using VectorType = itk::Vector<float, 3>;
    using VectorImageType = itk::Image<VectorType, 3>;
    using ScalarVectorImageType = itk::Image<float, 3>;

    // Set up filter

    using FilterType = itk::DisplacementFieldJacobianDeterminantFilter<VectorImageType, float>;
    auto filter = FilterType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, DisplacementFieldJacobianDeterminantFilter, ImageToImageFilter);


    // Run the test
    itk::Size<3> sz;
    sz[0] = 100;
    sz[1] = 100;
    sz[2] = 100;
    itk::NullImageToImageFilterDriver<VectorImageType, ScalarVectorImageType> test1;
    test1.SetImageSize(sz);
    test1.SetFilter(filter);
    test1.Execute();
    filter->Print(std::cout);

    // Run the test again with ImageSpacingOn
    bool useImageSpacing = true;
    ITK_TEST_SET_GET_BOOLEAN(filter, UseImageSpacing, useImageSpacing);


    test1.Execute();
    filter->Print(std::cout);

    // Run the test again with specified weights
    typename FilterType::WeightsType weights{ { { 1.0, 2.0, 3.0 } } };
    filter->SetDerivativeWeights(weights);
    ITK_TEST_SET_GET_VALUE(weights, filter->GetDerivativeWeights());

    test1.Execute();
    filter->Print(std::cout);
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }
  if (ValueTestPassed == false)
  {
    return EXIT_FAILURE;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
