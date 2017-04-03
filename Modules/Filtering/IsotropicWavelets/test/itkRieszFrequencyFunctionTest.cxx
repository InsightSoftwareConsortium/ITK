/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
runRieszFrequencyFunctionTest(unsigned int order)
{
  bool                                                   testPassed = true;
  const unsigned int                                     Dimension = VDimension;
  typedef std::complex<double>                           OutputType;
  typedef itk::Point<itk::SpacePrecisionType, Dimension> InputType;

  typedef itk::RieszFrequencyFunction<OutputType, Dimension, InputType> RieszFrequencyFunctionType;
  typename RieszFrequencyFunctionType::Pointer rieszFunction = RieszFrequencyFunctionType::New();

  InputType frequencyPoint;
  frequencyPoint.Fill(0.1);
  OutputType resultFirstOrder = rieszFunction->Evaluate(frequencyPoint, 0);
  rieszFunction->DebugOn();
  rieszFunction->SetOrder(1);
  typename RieszFrequencyFunctionType::IndicesFixedArrayType indices;
  indices.Fill(0);
  indices[0] = 1;
  OutputType resultFirstOrderWithIndices = rieszFunction->EvaluateWithIndices(frequencyPoint, indices);

  if (itk::Math::NotAlmostEquals(resultFirstOrder, resultFirstOrderWithIndices))
  {
    std::cerr << "Should be equal: " << resultFirstOrder << " == " << resultFirstOrderWithIndices << std::endl;
    testPassed = false;
  }

  // Test getting subIndices
  // Init indice has to be sorted in descending order, example:(3,0,0)
  typename RieszFrequencyFunctionType::IndicesArrayType initIndice;
  initIndice.resize(Dimension);
  initIndice[0] = order;
  typedef typename RieszFrequencyFunctionType::SetType SetType;
  SetType                                              uniqueIndices;
  unsigned int                                         positionToStartProcessing = 0;
  RieszFrequencyFunctionType::ComputeUniqueIndices(initIndice, positionToStartProcessing, uniqueIndices);
  std::cout << "UniqueIndices: " << uniqueIndices.size() << std::endl;
  for (typename SetType::const_iterator it = uniqueIndices.begin(); it != uniqueIndices.end(); ++it)
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
  for (typename SetType::const_iterator it = allPermutations.begin(); it != allPermutations.end(); ++it)
  {
    std::cout << "(";
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      std::cout << (*it)[i] << ", ";
    }
    std::cout << ")" << std::endl;
  }

  if (testPassed)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }

  //   // Get real part of complex image for visualization
  //   typedef itk::ComplexToRealImageFilter< ComplexImageType, ImageType > ComplexToRealFilter;
  //   ComplexToRealFilter::Pointer complexToRealFilter = ComplexToRealFilter::New();
  //   std::cout << "Real part of complex image:" << std::endl;
  //   for( unsigned int dir = 0; dir < ImageType::ImageDimension; dir++ )
  //     {
  //     std::cout << "Direction: " << dir + 1 << " / " << ImageType::ImageDimension << std::endl;
  //     complexToRealFilter->SetInput( filterBank->GetOutput( dir ) );
  //     complexToRealFilter->Update();
  //
  // #ifdef ITK_VISUALIZE_TESTS
  //     itk::NumberToString< unsigned int > n2s;
  //     itk::Testing::ViewImage( complexToRealFilter->GetOutput(), "RealPart of Complex. Direction: " + n2s(
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

  unsigned int dimension = atoi(argv[1]);
  unsigned int order = 5;
  if (argc == 3)
  {
    order = atoi(argv[2]);
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
