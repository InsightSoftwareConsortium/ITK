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

#include "itkBinaryThresholdSpatialFunction.h"
#include "itkSphereSignedDistanceFunction.h"

#include "itkFloodFilledSpatialFunctionConditionalConstIterator.h"
#include "itkTestingMacros.h"

/**
 * This module tests the sphereality of the
 * BinaryThresholdSpatialFunction class.
 *
 * In particular, it creates a SphereSignedDistanceFunction object
 * connect it to a BinaryThresholdSpatialFunction class.
 *
 * The sphere parameters are set at radius of 5 and center of (0,0).
 * Membership (i.e. with user specified threshold) is evaluated at
 * several point and compared to expected values.
 * The test fails if the evaluated results is not the same as expected
 * results.
 *
 */

int
itkBinaryThresholdSpatialFunctionTest(int, char *[])
{
  using CoordRep = double;
  constexpr unsigned int Dimension = 2;

  using SphereFunctionType = itk::SphereSignedDistanceFunction<CoordRep, Dimension>;
  using FunctionType = itk::BinaryThresholdSpatialFunction<SphereFunctionType>;
  using PointType = SphereFunctionType::PointType;
  using ParametersType = SphereFunctionType::ParametersType;

  SphereFunctionType::Pointer sphere = SphereFunctionType::New();

  // We must initialize the sphere before use
  sphere->Initialize();

  ParametersType parameters(sphere->GetNumberOfParameters());
  parameters.Fill(0.0);
  parameters[0] = 5.0;

  sphere->SetParameters(parameters);

  std::cout << "SphereParameters: " << sphere->GetParameters() << std::endl;

  // Create a binary threshold function
  FunctionType::Pointer function = FunctionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(function, BinaryThresholdSpatialFunction, SpatialFunction);


  // Connect the sphere function
  function->SetFunction(sphere);

  // Set the thresholds
  double lowerThreshold = -3.0;
  double upperThreshold = 4.0;
  function->SetLowerThreshold(lowerThreshold);
  ITK_TEST_SET_GET_VALUE(lowerThreshold, function->GetLowerThreshold());
  function->SetUpperThreshold(upperThreshold);
  ITK_TEST_SET_GET_VALUE(upperThreshold, function->GetUpperThreshold());

  PointType point;

  for (double p = 0.0; p < 10.0; p += 1.0)
  {
    point.Fill(p);
    FunctionType::OutputType output = function->Evaluate(point);
    std::cout << "f(" << point << ") = " << output;
    std::cerr << " [" << function->GetFunction()->Evaluate(point);
    std::cout << "] " << std::endl;

    // Check results
    CoordRep val = p * std::sqrt(2.0) - parameters[0];
    bool     expected = (lowerThreshold <= val && upperThreshold >= val);
    if (output != expected)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error at point: " << point << std::endl;
      std::cerr << "Expected function value is: " << expected << ", but got: " << output << std::endl;
      return EXIT_FAILURE;
    }
  }

  //
  // In the following, we demsonstrate how BinaryThresholdSpatialFunction
  // can be used to iterate over pixels whose signed distance is
  // within [lowerThreshold, upperThreshold] of the zero level set defining
  // the sphere.
  //

  // Set up a dummy image
  using ImageType = itk::Image<unsigned char, Dimension>;
  ImageType::Pointer  image = ImageType::New();
  ImageType::SizeType size;
  size.Fill(10);
  image->SetRegions(size);
  image->Allocate();
  image->FillBuffer(255);

  // Set up the conditional iterator
  using IteratorType = itk::FloodFilledSpatialFunctionConditionalConstIterator<ImageType, FunctionType>;

  IteratorType iterator(image, function);
  iterator.SetOriginInclusionStrategy();

  // Add a seed that already inside the region
  ImageType::IndexType index;
  index[0] = 0;
  index[1] = 3;
  iterator.AddSeed(index);

  // Get the seeds and display them
  std::cout << "Iterator seeds";
  for (auto seed : iterator.GetSeeds())
  {
    std::cout << " " << seed;
  }
  std::cout << std::endl;

  iterator.GoToBegin();

  while (!iterator.IsAtEnd())
  {
    index = iterator.GetIndex();
    image->TransformIndexToPhysicalPoint(index, point);
    double value = sphere->Evaluate(point);

    // Check if value is within range
    if (value < lowerThreshold || value > upperThreshold)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error at index: " << index << std::endl;
      std::cout << "Point value: " << value << " is not within thresholds [" << lowerThreshold << ", " << upperThreshold
                << "]" << std::endl;
      return EXIT_FAILURE;
    }

    ++iterator;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
