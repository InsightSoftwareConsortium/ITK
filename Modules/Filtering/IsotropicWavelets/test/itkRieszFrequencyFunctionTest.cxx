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

#include "itkRieszFrequencyFunction.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkForwardFFTImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkTestingMacros.h"

#include <memory>
#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkNumberToString.h"
#  include "itkViewImage.h"
#endif

template <unsigned int VDimension>
int
runRieszFrequencyFunctionTest(unsigned int inputOrder)
{
  bool               testPassed = true;
  const unsigned int Dimension = VDimension;

  using OutputType = std::complex<double>;
  using InputType = itk::Point<itk::SpacePrecisionType, Dimension>;

  using RieszFrequencyFunctionType = itk::RieszFrequencyFunction<OutputType, Dimension, InputType>;
  auto rieszFunction = RieszFrequencyFunctionType::New();

  InputType frequencyPoint;
  frequencyPoint.Fill(0.1);
  rieszFunction->SetOrder(1);

  typename RieszFrequencyFunctionType::IndicesArrayType indices(VDimension);
  indices[0] = 1;
  OutputType resultFirstOrderWithIndices = rieszFunction->EvaluateWithIndices(frequencyPoint, indices);
  // Calculate euclidean norm / magnitude of frequencyPoint.
  double accum(0);
  for (size_t d = 0; d < Dimension; ++d)
  {
    accum += frequencyPoint[d] * frequencyPoint[d];
  }
  auto       frequencyMagnitude = static_cast<itk::SpacePrecisionType>(sqrt(accum));
  OutputType trueResult(0, -frequencyPoint[0] / frequencyMagnitude);
  if (itk::Math::NotAlmostEquals(resultFirstOrderWithIndices, trueResult))
  {
    std::cerr << "Error. EvaluateWithIndices with order 1, index (1,...,0):\n actual: " << resultFirstOrderWithIndices
              << " expected: " << trueResult << " are not equal!" << std::endl;
  }

  // Test getting subIndexs
  // Init index has to be sorted in descending order, example:(3,0,0)
  typename RieszFrequencyFunctionType::IndicesArrayType initIndex;
  initIndex.resize(Dimension);
  initIndex[0] = inputOrder;
  using SetType = typename RieszFrequencyFunctionType::SetType;
  SetType      uniqueIndices;
  unsigned int positionToStartProcessing = 0;
  RieszFrequencyFunctionType::ComputeUniqueIndices(initIndex, uniqueIndices, positionToStartProcessing);
  std::cout << "UniqueIndices: " << uniqueIndices.size() << std::endl;
  for (auto it = uniqueIndices.begin(); it != uniqueIndices.end(); ++it)
  {
    std::cout << "(";
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      std::cout << (*it)[i] << ", ";
    }
    std::cout << ")" << std::endl;
  }

  SetType allPermutations = RieszFrequencyFunctionType::ComputeAllPermutations(uniqueIndices);
  std::cout << "AllPermutations: " << allPermutations.size() << std::endl;
  for (auto it = allPermutations.begin(); it != allPermutations.end(); ++it)
  {
    std::cout << "(";
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      std::cout << (*it)[i] << ", ";
    }
    std::cout << ")" << std::endl;
  }

  unsigned int expectedNumberOfComponents = RieszFrequencyFunctionType::ComputeNumberOfComponents(inputOrder);
  unsigned int actualNumberOfComponents = allPermutations.size();
  if (actualNumberOfComponents != expectedNumberOfComponents)
  {
    std::cerr << "Error. NumberOfComponents for inputOrder: " << inputOrder << "; actual: " << actualNumberOfComponents
              << " expected: " << expectedNumberOfComponents << " are not equal!" << std::endl;
    testPassed = false;
  }
  // Regression test for index calculation.
  for (unsigned int order = 1; order < 6; ++order)
  {
    uniqueIndices.clear();
    allPermutations.clear();
    initIndex[0] = order;
    RieszFrequencyFunctionType::ComputeUniqueIndices(initIndex, uniqueIndices /*, 0 */);
    allPermutations = RieszFrequencyFunctionType::ComputeAllPermutations(uniqueIndices);
    actualNumberOfComponents = allPermutations.size();
    expectedNumberOfComponents = RieszFrequencyFunctionType::ComputeNumberOfComponents(order);
    if (actualNumberOfComponents != expectedNumberOfComponents)
    {
      std::cerr << "Error. NumberOfComponents for order: " << order << "; actual: " << actualNumberOfComponents
                << " expected: " << expectedNumberOfComponents << " are not equal!" << std::endl;
      testPassed = false;
    }
  }

  // Evaluate All Components:
  rieszFunction->SetOrder(inputOrder);
  rieszFunction->DebugOn();
  using OutputComponentType = typename RieszFrequencyFunctionType::OutputComponentsType;

  OutputComponentType allComponents = rieszFunction->EvaluateAllComponents(frequencyPoint);
  std::cout << "allComponents:" << std::endl;
  for (typename OutputComponentType::const_iterator it = allComponents.begin(); it != allComponents.end(); ++it)
  {
    std::cout << *it << ",";
  }
  std::cout << std::endl;

  if (testPassed)
  {
    std::cout << "Test Passed!" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }

  //   // Get real part of complex image for visualization
  //   using ComplexToRealFilter = itk::ComplexToRealImageFilter< ComplexImageType, ImageType >;
  //   auto complexToRealFilter = ComplexToRealFilter::New();
  //   std::cout << "Real part of complex image:" << std::endl;
  //   for( unsigned int dir = 0; dir < ImageType::ImageDimension; dir++ )
  //     {
  //     std::cout << "Direction: " << dir + 1 << " / " << ImageType::ImageDimension << std::endl;
  //     complexToRealFilter->SetInput( filterBank->GetOutput( dir ) );
  //     complexToRealFilter->Update();
  //
  // #ifdef ITK_VISUALIZE_TESTS
  //     itk::NumberToString< unsigned int > n2s;
  //     itk::ViewImage<ImageType>::View( complexToRealFilter->GetOutput(), "RealPart of Complex. Direction: " + n2s(
  //                          dir + 1) + " / " + n2s( ImageType::ImageDimension ) );
  // #endif
}

int
itkRieszFrequencyFunctionTest(int argc, char * argv[])
{
  if (argc < 2 || argc > 3)
  {
    std::cerr << "Usage: " << argv[0] << "dimension [order]" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int dimension = std::stoi(argv[1]);
  unsigned int order = 5;
  if (argc == 3)
  {
    order = std::stoi(argv[2]);
  }

  if (dimension == 2)
  {
    return runRieszFrequencyFunctionTest<2>(order);
  }
  else if (dimension == 3)
  {
    return runRieszFrequencyFunctionTest<3>(order);
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
