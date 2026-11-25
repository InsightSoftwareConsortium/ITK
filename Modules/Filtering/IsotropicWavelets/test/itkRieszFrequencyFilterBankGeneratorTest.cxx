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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkForwardFFTImageFilter.h"
#include "itkRieszFrequencyFilterBankGenerator.h"
#include "itkComplexToRealImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingMacros.h"

#include <memory>
#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkNumberToString.h"
#  include "itkViewImage.h"
#endif

int
itkRieszFrequencyFilterBankGeneratorTest(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage inputOrder" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string inputImage = argv[1];
  const std::string outputImage = argv[2];

  constexpr unsigned int Dimension = 3;
  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  // Perform FFT on input image
  using FFTFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());
  fftFilter->Update();

  using ComplexImageType = FFTFilterType::OutputImageType;

  // using FunctionType = itk::RieszFrequencyFunction<>;
  using RieszFilterBankType = itk::RieszFrequencyFilterBankGenerator<ComplexImageType>;
  auto filterBank = RieszFilterBankType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filterBank, RieszFrequencyFilterBankGenerator, GenerateImageSource);

  filterBank->SetSize(fftFilter->GetOutput()->GetLargestPossibleRegion().GetSize());

  // Test exception cases
  unsigned int inputOrder = 0;
  ITK_TRY_EXPECT_EXCEPTION(filterBank->SetOrder(inputOrder));

  inputOrder = std::stoi(argv[3]);
  filterBank->SetOrder(inputOrder);
  ITK_TEST_SET_GET_VALUE(inputOrder, filterBank->GetOrder());

  ITK_TRY_EXPECT_NO_EXCEPTION(filterBank->Update());

  // Get iterator to Indices of RieszFunction.
  using IndicesType = RieszFilterBankType::RieszFunctionType::SetType;
  IndicesType indices = filterBank->GetModifiableEvaluator()->GetIndices();
  auto        indicesIt = indices.begin();

  // Get real part of complex image for visualization
  using ComplexToRealFilter = itk::ComplexToRealImageFilter<ComplexImageType, ImageType>;
  auto complexToRealFilter = ComplexToRealFilter::New();
  using ComplexToImaginaryFilter = itk::ComplexToImaginaryImageFilter<ComplexImageType, ImageType>;
  auto complexToImaginaryFilter = ComplexToImaginaryFilter::New();
  std::cout << "Order: " << inputOrder << std::endl;
  bool orderIsEven = (inputOrder % 2 == 0);
  if (orderIsEven)
  {
    std::cout << "Real part of complex image:" << std::endl;
    for (unsigned int comp = 0; comp < filterBank->GetNumberOfOutputs(); comp++)
    {
      std::ostringstream oss;
      oss << "(";
      std::cout << "Component: " << comp << " / " << filterBank->GetNumberOfOutputs() - 1 << std::endl;
      for (unsigned int i = 0; i < Dimension; ++i)
      {
        oss << (*indicesIt)[i] << ", ";
      }
      ++indicesIt;
      oss << ")";
      std::cout << "  Index: " << oss.str() << std::endl;

      complexToRealFilter->SetInput(filterBank->GetOutput(comp));
      complexToRealFilter->Update();
#ifdef ITK_VISUALIZE_TESTS
      itk::NumberToString<unsigned int> n2s;
      itk::ViewImage<ImageType>::View(complexToRealFilter->GetOutput(),
                                      "RealPart of Complex. Component: " + n2s(comp) + " / " +
                                        n2s(filterBank->GetNumberOfOutputs() - 1) + ". Indices: " + oss.str());
#endif
    }
  }
  else
  {
    std::cout << "Imaginary part of complex image:" << std::endl;
    for (unsigned int comp = 0; comp < filterBank->GetNumberOfOutputs(); comp++)
    {
      std::ostringstream oss;
      oss << "(";
      std::cout << "Component: " << comp << " / " << filterBank->GetNumberOfOutputs() - 1 << std::endl;
      for (unsigned int i = 0; i < Dimension; ++i)
      {
        oss << (*indicesIt)[i] << ", ";
      }
      ++indicesIt;
      oss << ")";
      std::cout << "  Index: " << oss.str() << std::endl;

      complexToImaginaryFilter->SetInput(filterBank->GetOutput(comp));
      complexToImaginaryFilter->Update();
#ifdef ITK_VISUALIZE_TESTS
      itk::NumberToString<unsigned int> n2s;
      itk::ViewImage<ImageType>::View(complexToImaginaryFilter->GetOutput(),
                                      "ImaginaryPart of Complex. Component: " + n2s(comp) + " / " +
                                        n2s(filterBank->GetNumberOfOutputs() - 1) + ". Indices: " + oss.str());
#endif
    }
  }

  return EXIT_SUCCESS;
}
