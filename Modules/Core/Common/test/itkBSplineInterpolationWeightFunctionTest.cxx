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

#include "itkBSplineInterpolationWeightFunction.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkTestingMacros.h"

// Test template instantiation for TCoordRep = float and VSplineOrder = 1.
// Note that this particular template instantiation would take forever to
// compile on VS2015 Update 3 64-bit Release when using ITK 4.13, but
// itkBSplineInterpolationWeightFunction.hxx can now handle this Visual C++
// compiler bug.
template class itk::BSplineInterpolationWeightFunction<float, 2U, 1U>;


// Test NumberOfWeights:
static_assert((itk::BSplineInterpolationWeightFunction<float, 2, 1>::NumberOfWeights == 4) &&
                (itk::BSplineInterpolationWeightFunction<float, 2, 2>::NumberOfWeights == 9) &&
                (itk::BSplineInterpolationWeightFunction<float, 2, 3>::NumberOfWeights == 16) &&
                (itk::BSplineInterpolationWeightFunction<float, 3, 3>::NumberOfWeights == 64),
              "NumberOfWeights must be (SplineOrder+1) ^ SpaceDimension");


/*
 * This test exercises methods in the
 * BSplineInterpolationWeightFunction class.
 */
int
itkBSplineInterpolationWeightFunctionTest(int, char *[])
{

  { // Creating a local scope
    using CoordRepType = double;
    constexpr unsigned int SpaceDimension = 1;
    constexpr unsigned int SplineOrder = 2;

    std::cout << "Testing SpaceDimension= " << SpaceDimension;
    std::cout << " and SplineOrder= " << SplineOrder << "  ";

    using FunctionType = itk::BSplineInterpolationWeightFunction<CoordRepType, SpaceDimension, SplineOrder>;
    using ContinuousIndexType = FunctionType::ContinuousIndexType;
    using IndexType = FunctionType::IndexType;
    using WeightsType = FunctionType::WeightsType;

    auto function = FunctionType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(function, BSplineInterpolationWeightFunction, FunctionBase);


#if !defined(ITK_LEGACY_REMOVE)
#  if defined(ITK_LEGACY_SILENT)
    typename FunctionType::SizeType supportSize = FunctionType::SizeType::Filled(SplineOrder + 1);
    ITK_TEST_EXPECT_EQUAL(supportSize, function->GetSupportSize());

    unsigned int numberOfWeights = WeightsType::Length;
    ITK_TEST_EXPECT_EQUAL(numberOfWeights, function->GetNumberOfWeights());
#  endif
#endif

    WeightsType weights1;
    WeightsType weights2;

    ContinuousIndexType position1;
    ContinuousIndexType position2;

    IndexType startIndex1;
    IndexType startIndex2;

    bool testFailed = false;

    for (double x = 0.0; x <= 2.0; x += 0.1)
    {

      position1[0] = x;
      position2[0] = -x;

      function->Evaluate(position1, weights1, startIndex1);
      function->Evaluate(position2, weights2, startIndex2);

      const unsigned int numberOfWeigts = weights1.size();

      const int indexDifference = itk::Math::abs(startIndex2[0] + startIndex1[0]) & 1;


      const double tolerance = 1e-6;
      bool         symmetryForXBroken = false;

      for (unsigned int nw = 0; nw < numberOfWeigts - indexDifference; ++nw)
      {
        if (itk::Math::abs(weights1[nw] - weights2[numberOfWeigts - nw - 1 - indexDifference]) > tolerance)
        {
          symmetryForXBroken = true;
        }
      }

      if (symmetryForXBroken)
      {
        std::cerr << std::endl;
        std::cerr << "Error in weights symmetry for X = " << x << std::endl;
        testFailed = true;
        std::cerr << "indexDifference= " << indexDifference << std::endl;
        for (unsigned int nw = 0; nw < numberOfWeigts; ++nw)
        {
          std::cerr << weights1[nw] << "\t";
        }
        std::cerr << std::endl;
        for (unsigned int nw = 0; nw < numberOfWeigts; ++nw)
        {
          std::cerr << weights2[nw] << "\t";
        }
        std::cerr << std::endl;
        for (unsigned int sd = 0; sd < SpaceDimension; ++sd)
        {
          std::cerr << startIndex1[sd] << "\t";
        }
        std::cerr << std::endl;
        for (unsigned int sd = 0; sd < SpaceDimension; ++sd)
        {
          std::cerr << startIndex2[sd] << "\t";
        }
        std::cerr << std::endl;
      }
    }
    if (testFailed)
    {
      std::cerr << "Test Failed !" << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "Test passed. " << std::endl;
  }
  { // Creating a local scope
    using CoordRepType = double;
    constexpr unsigned int SpaceDimension = 1;
    constexpr unsigned int SplineOrder = 3;

    std::cout << "Testing SpaceDimension= " << SpaceDimension;
    std::cout << " and SplineOrder= " << SplineOrder << "  ";

    using FunctionType = itk::BSplineInterpolationWeightFunction<CoordRepType, SpaceDimension, SplineOrder>;
    using ContinuousIndexType = FunctionType::ContinuousIndexType;
    using IndexType = FunctionType::IndexType;
    using WeightsType = FunctionType::WeightsType;

    auto function = FunctionType::New();


    WeightsType weights1;
    WeightsType weights2;

    ContinuousIndexType position1;
    ContinuousIndexType position2;

    IndexType startIndex1;
    IndexType startIndex2;

    bool testFailed = false;

    for (double x = 0.0; x <= 2.0; x += 0.1)
    {

      position1[0] = x;
      position2[0] = -x;

      function->Evaluate(position1, weights1, startIndex1);
      function->Evaluate(position2, weights2, startIndex2);

      const unsigned int numberOfWeigts = weights1.size();

      const int indexDifference = itk::Math::abs(startIndex2[0] + startIndex1[0] + 1) & 1;


      const double tolerance = 1e-6;
      bool         symmetryForXBroken = false;

      for (unsigned int nw = 0; nw < numberOfWeigts - indexDifference; ++nw)
      {
        if (itk::Math::abs(weights1[nw] - weights2[numberOfWeigts - nw - 1 - indexDifference]) > tolerance)
        {
          symmetryForXBroken = true;
        }
      }

      if (symmetryForXBroken)
      {
        std::cerr << std::endl;
        std::cerr << "Error in weights symmetry for X = " << x << std::endl;
        testFailed = true;
        std::cerr << "indexDifference= " << indexDifference << std::endl;
        for (unsigned int nw = 0; nw < numberOfWeigts; ++nw)
        {
          std::cerr << weights1[nw] << "\t";
        }
        std::cerr << std::endl;
        for (unsigned int nw = 0; nw < numberOfWeigts; ++nw)
        {
          std::cerr << weights2[nw] << "\t";
        }
        std::cerr << std::endl;
        for (unsigned int sd = 0; sd < SpaceDimension; ++sd)
        {
          std::cerr << startIndex1[sd] << "\t";
        }
        std::cerr << std::endl;
        for (unsigned int sd = 0; sd < SpaceDimension; ++sd)
        {
          std::cerr << startIndex2[sd] << "\t";
        }
        std::cerr << std::endl;
      }
    }
    if (testFailed)
    {
      std::cerr << "Test Failed !" << std::endl;
      return EXIT_FAILURE;
    }

    std::cout << "Test passed. " << std::endl;
  }

  { // Creating a local scope
    using CoordRepType = double;
    constexpr unsigned int SpaceDimension = 3;
    constexpr unsigned int SplineOrder = 3;
    std::cout << "Testing SpaceDimension= " << SpaceDimension;
    std::cout << " and SplineOrder= " << SplineOrder << "  ";

    using FunctionType = itk::BSplineInterpolationWeightFunction<CoordRepType, SpaceDimension, SplineOrder>;
    using ContinuousIndexType = FunctionType::ContinuousIndexType;
    using IndexType = FunctionType::IndexType;
    using WeightsType = FunctionType::WeightsType;
    using SizeType = FunctionType::SizeType;

    auto function = FunctionType::New();
    function->Print(std::cout);

    SizeType      size = FunctionType::SupportSize;
    unsigned long numberOfWeights = FunctionType::NumberOfWeights;

    std::cout << "Number Of Weights: " << numberOfWeights << std::endl;

    ContinuousIndexType position;
    WeightsType         weights;
    IndexType           startIndex;

    position.Fill(4.15);
    weights = function->Evaluate(position);

    std::cout << "Position: " << position << std::endl;
    std::cout << "Weights: " << weights << std::endl;

    function->Evaluate(position, weights, startIndex);
    std::cout << "Position: " << position << std::endl;
    std::cout << "Weights: " << weights << std::endl;
    std::cout << "Start Index: " << startIndex << std::endl;


    // Check for accuracy
    using KernelType = itk::BSplineKernelFunction<SplineOrder>;
    auto kernel = KernelType::New();

    using ImageType = itk::Image<char, SpaceDimension>;
    auto                  image = ImageType::New();
    ImageType::RegionType region;
    region.SetIndex(startIndex);
    region.SetSize(size);

    image->SetRegions(region);
    image->Allocate(true); // initialize buffer to zero

    using IteratorType = itk::ImageRegionConstIteratorWithIndex<ImageType>;
    IteratorType  iter(image, image->GetBufferedRegion());
    unsigned long counter = 0;

    while (!iter.IsAtEnd())
    {

      double value = 1.0;
      for (unsigned int j = 0; j < SpaceDimension; ++j)
      {
        value *= kernel->Evaluate(static_cast<double>(iter.GetIndex()[j]) - position[j]);
      }
      if (itk::Math::abs(weights[counter] - value) > 1e-7)
      {
        std::cout << "Error at weights[" << counter << "]" << std::endl;
        std::cout << "Computed value: " << weights[counter] << std::endl;
        std::cout << "Expected value: " << value << std::endl;
        return EXIT_FAILURE;
      }

      ++counter;
      ++iter;
    }

    std::cout << "Test passed. " << std::endl;
  }
  return EXIT_SUCCESS;
}
