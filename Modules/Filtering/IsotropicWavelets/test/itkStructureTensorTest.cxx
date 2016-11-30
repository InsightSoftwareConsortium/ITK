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
#include <string>
#include <cmath>
#include "itkStructureTensor.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include <itkTestingComparisonImageFilter.h>
// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#ifdef ITK_VISUALIZE_TESTS
#  include <itkNumberToString.h>
#  include "itkViewImage.h"
#endif
using namespace std;
using namespace itk;

template <unsigned int N>
int
runStructureTensorTest()
{
  const unsigned int                         ImageDimension = N;
  typedef itk::Image<double, ImageDimension> ImageType;
  typedef itk::Index<ImageDimension>         IndexType;
  typedef itk::Size<ImageDimension>          SizeType;
  typedef itk::ImageRegion<ImageDimension>   RegionType;
  bool                                       testFailed = false;

  unsigned int L = 24;
  SizeType     size;
  size.Fill(L);
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

  // Fill half image with non-zero.
  SizeType sizeHalf;
  sizeHalf = size;
  sizeHalf[0] = L / 2;
  IndexType startHalf;
  startHalf = start;
  startHalf[0] = L / 2;
  RegionType regionHalfLeft;
  regionHalfLeft.SetIndex(start);
  regionHalfLeft.SetSize(sizeHalf);
  RegionType regionHalfRight;
  regionHalfRight.SetIndex(startHalf);
  regionHalfRight.SetSize(sizeHalf);

  const unsigned int                          nInputs = 2;
  typedef itk::ImageRegionIterator<ImageType> InputIteratorType;
  InputIteratorType                           inputIt1(inputImage1, regionHalfLeft);
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
  itk::Testing::ViewImage(inputImage1.GetPointer(), "input1");
  itk::Testing::ViewImage(inputImage2.GetPointer(), "input2");
#endif

  // Structure Tensor
  typedef itk::StructureTensor<ImageType>  StructureTensorType;
  typename StructureTensorType::Pointer    tensor = StructureTensorType::New();
  std::vector<typename ImageType::Pointer> inputs;
  inputs.push_back(inputImage1);
  inputs.push_back(inputImage2);
  tensor->SetInputs(inputs);
  tensor->Update();
  typename StructureTensorType::OutputImageType::Pointer eigenImage = tensor->GetOutput();
  unsigned                                               eigenMatrixRows = eigenImage->GetPixel(start).Rows();
  unsigned                                               eigenMatrixCols = eigenImage->GetPixel(start).Cols();
  if (eigenMatrixRows != nInputs || eigenMatrixCols != nInputs + 1)
  {
    testFailed = true;
    std::cout << "The resulting eigenMatrix size is wrong. Rows: " << eigenMatrixRows
              << " . Columns: " << eigenMatrixCols << std::endl;
  }
#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(tensor->GetGaussianSource()->GetOutput(), "gaussian");
#endif

  typename ImageType::Pointer largestEigenValueProjectionImage;
  for (unsigned int eigen_number = 0; eigen_number < nInputs; ++eigen_number)
  {
    typename StructureTensorType::InputImagePointer projectImage = tensor->ComputeProjectionImage(eigen_number);
    if (eigen_number == nInputs - 1)
      largestEigenValueProjectionImage = projectImage;

#ifdef ITK_VISUALIZE_TESTS
    itk::NumberToString<float> n2s;
    itk::Testing::ViewImage(projectImage.GetPointer(), "eigen_number: " + n2s(eigen_number));
#endif
  }
  typename ImageType::Pointer coherencyImage = tensor->ComputeCoherencyImage();
#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(coherencyImage.GetPointer(), "coherency image");
#endif

  /** Compare With Known result: ***/
  // The projected image from the largest eigenValue must be all ones.
  typename ImageType::Pointer validImage = ImageType::New();
  validImage->SetRegions(region);
  validImage->Allocate();
  validImage->FillBuffer(1);
  typedef itk::Testing::ComparisonImageFilter<ImageType, ImageType> ComparisonType;
  typename ComparisonType::Pointer                                  diff = ComparisonType::New();
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
    return EXIT_FAILURE;
  if (testFailed)
    return EXIT_FAILURE;

  return EXIT_SUCCESS;
}

int
itkStructureTensorTest(int, char *[])
{
  const unsigned int dimension2D = 2;
  int                result2D = runStructureTensorTest<dimension2D>();
  const unsigned int dimension3D = 3;
  int                result3D = runStructureTensorTest<dimension3D>();
  int                result4D = EXIT_SUCCESS;
#ifndef ITK_VISUALIZE_TESTS
  // cannot visualize 4D images with viewimage
  result4D = runStructureTensorTest<4>();
#endif
  return result2D && result3D && result4D;
}
