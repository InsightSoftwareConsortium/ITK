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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkStructureTensor.h"
#include "itkTestingMacros.h"

#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkNumberToString.h"
#  include "itkViewImage.h"
#endif

template <unsigned int VDimension>
int
runStructureTensorTest()
{
  const unsigned int Dimension = VDimension;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using IndexType = itk::Index<Dimension>;
  using SizeType = itk::Size<Dimension>;
  using RegionType = itk::ImageRegion<Dimension>;

  bool testFailed = false;

  unsigned int sizeValue = 12;
  SizeType     size;
  size.Fill(sizeValue);
  IndexType start;
  start.Fill(0);
  RegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  typename ImageType::Pointer inputImage1 = ImageType::New();
  inputImage1->SetRegions(region);
  inputImage1->Allocate();
  inputImage1->FillBuffer(0);

  typename ImageType::Pointer inputImage2 = ImageType::New();
  inputImage2->SetRegions(region);
  inputImage2->Allocate();
  inputImage2->FillBuffer(0);

  // Fill half image with non-zero
  SizeType sizeHalf;
  sizeHalf = size;
  sizeHalf[0] = sizeValue / 2;
  IndexType startHalf;
  startHalf = start;
  startHalf[0] = sizeValue / 2;
  RegionType regionHalfLeft;
  regionHalfLeft.SetIndex(start);
  regionHalfLeft.SetSize(sizeHalf);
  RegionType regionHalfRight;
  regionHalfRight.SetIndex(startHalf);
  regionHalfRight.SetSize(sizeHalf);

  constexpr unsigned int nInputs = 2;
  using InputIteratorType = itk::ImageRegionIterator<ImageType>;
  InputIteratorType inputIt1(inputImage1, regionHalfLeft);
  inputIt1.GoToBegin();
  while (!inputIt1.IsAtEnd())
  {
    inputIt1.Set(1.0);
    ++inputIt1;
  }

  InputIteratorType inputIt2(inputImage2, regionHalfRight);
  inputIt2.GoToBegin();
  while (!inputIt2.IsAtEnd())
  {
    inputIt2.Set(1.0);
    ++inputIt2;
  }

#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(inputImage1.GetPointer(), "input1");
  itk::ViewImage<ImageType>::View(inputImage2.GetPointer(), "input2");
#endif

  // Structure Tensor
  using StructureTensorType = itk::StructureTensor<ImageType>;
  auto tensor = StructureTensorType::New();
  // Test Set/Get
  // WindowRadius
  unsigned int nonDefaultGaussianRadius = 3;
  tensor->SetGaussianWindowRadius(nonDefaultGaussianRadius);
  TEST_SET_GET_VALUE(nonDefaultGaussianRadius, tensor->GetGaussianWindowRadius());
  tensor->SetGaussianWindowRadius(2); // Restore default.

  // WindowSigma
  typename StructureTensorType::FloatType nonDefaultGaussianSigma = 2.0;
  tensor->SetGaussianWindowSigma(nonDefaultGaussianSigma);
  TEST_SET_GET_VALUE(nonDefaultGaussianSigma, tensor->GetGaussianWindowSigma());
  tensor->SetGaussianWindowSigma(1.0); // Restore default.
  // Use a external, new GaussianSource:
  // The gaussian source is modified in BeforeThreadedGenerateData() only if
  // the source has different sigma or radius than this class.
  typename StructureTensorType::GaussianSourceType::Pointer gaussianSource = tensor->GetModifiableGaussianSource();
  gaussianSource = StructureTensorType::GaussianSourceType::New();

  std::vector<typename ImageType::Pointer> inputs;
  inputs.push_back(inputImage1);
  inputs.push_back(inputImage2);
  tensor->SetInputs(inputs);
  tensor->Update();

  auto eigenImage = tensor->GetOutput();

  auto     outMatrixAtStart = eigenImage->GetPixel(start);
  unsigned eigenMatrixRows = outMatrixAtStart.Rows();
  unsigned eigenMatrixCols = outMatrixAtStart.Cols();
  if (eigenMatrixRows != nInputs || eigenMatrixCols != nInputs + 1)
  {
    testFailed = true;
    std::cout << "The resulting eigenMatrix size is wrong. Rows: " << eigenMatrixRows
              << " . Columns: " << eigenMatrixCols << std::endl;
  }
  auto rotationMatrix = tensor->GetRotationMatrixFromOutputMatrix(outMatrixAtStart);
  auto rotationMatrixReOrdered = tensor->GetRotationMatrixFromOutputMatrix(outMatrixAtStart, true);
  for (unsigned int r = 0; r < nInputs; ++r)
  {
    for (unsigned int c = 0; c < nInputs; ++c)
    {
      if (rotationMatrix[r][c] != outMatrixAtStart[c][r])
      {
        testFailed = true;
        std::cout << "GetRotationMatrixFromOutputMatrix fails. rotationMatrix[r][c] " << rotationMatrix[r][c]
                  << " !=  outMatrixAtStart[c][r] " << outMatrixAtStart[c][r] << std::endl;
      }
      if (rotationMatrixReOrdered[r][c] != outMatrixAtStart[nInputs - 1 - c][r])
      {
        testFailed = true;
        std::cout << "GetRotationMatrixFromOutputMatrix fails. rotationMatrixReOrdered[r][c] "
                  << rotationMatrixReOrdered[r][c] << " !=  outMatrixAtStart[nInputs - 1 - c][r] "
                  << outMatrixAtStart[nInputs - 1 - c][r] << std::endl;
      }
    }
  }

#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(tensor->GetGaussianSource()->GetOutput(), "Gaussian");
#endif

  typename ImageType::Pointer largestEigenValueProjectionImage;
  for (unsigned int eigenNumber = 0; eigenNumber < nInputs; ++eigenNumber)
  {
    typename StructureTensorType::InputImagePointer projectImage = tensor->ComputeProjectionImage(eigenNumber);
    if (eigenNumber == nInputs - 1)
    {
      largestEigenValueProjectionImage = projectImage;
    }

#ifdef ITK_VISUALIZE_TESTS
    itk::NumberToString<float> n2s;
    itk::ViewImage<ImageType>::View(projectImage.GetPointer(), "eigen number: " + n2s(eigenNumber));
#endif
  }
  auto coherencyImage = tensor->ComputeCoherencyImage();
#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(coherencyImage.GetPointer(), "Coherency image");
#endif

  // Compare to known result
  // The projected image from the largest eigenValue must be all ones.
  auto validImage = ImageType::New();
  validImage->SetRegions(region);
  validImage->Allocate();
  validImage->FillBuffer(1);

  using ComparisonType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto diff = ComparisonType::New();

  diff->SetValidInput(validImage);
  diff->SetTestInput(largestEigenValueProjectionImage);

  // diff->SetDifferenceThreshold( intensityTolerance );
  // diff->SetToleranceRadius( radiusTolerance );
  diff->UpdateLargestPossibleRegion();

  bool                differenceFailed = false;
  const double        averageIntensityDifference = diff->GetTotalDifference();
  const unsigned long numberOfPixelsWithDifferences = diff->GetNumberOfPixelsWithDifferences();
  if (averageIntensityDifference > 0.0)
  {
    if (static_cast<int>(numberOfPixelsWithDifferences) > 0)
    {
      differenceFailed = true;
    }
    else
    {
      differenceFailed = false;
    }
  }
  else
  {
    differenceFailed = false;
  }

  if (differenceFailed)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Expected 0 different pixels, but got " << numberOfPixelsWithDifferences << std::endl;
    return EXIT_FAILURE;
  }
  if (testFailed)
  {
    std::cerr << "Test failed!" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

int
itkStructureTensorTest(int, char *[])
{
  int result2D = runStructureTensorTest<2>();
  int result3D = runStructureTensorTest<3>();

#ifndef ITK_VISUALIZE_TESTS
  // cannot visualize 4D images with viewimage
  int result4D = runStructureTensorTest<4>();
  return result2D && result3D && result4D;
#else
  return result2D && result3D;
#endif
}
