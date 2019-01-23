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

#include <complex>
#include "itkComplexToRealImageFilter.h"
#include "itkImage.h"
#include "itkImageDuplicator.h"
#include "itkMath.h"
#include "itkRieszRotationMatrix.h"
#include "itkTestingComparisonImageFilter.h"

template <typename TImage>
int
compareRealImagesAndReport(typename TImage::Pointer validImage, typename TImage::Pointer testImage)
{
  using ImageType = TImage;
  using ComparisonType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto diff = ComparisonType::New();
  diff->SetValidInput(validImage);
  diff->SetTestInput(testImage);
  diff->UpdateLargestPossibleRegion();
  bool                             differenceFailed = false;
  const typename TImage::PixelType averageIntensityDifference = diff->GetTotalDifference();
  const unsigned long              numberOfPixelsWithDifferences = diff->GetNumberOfPixelsWithDifferences();
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

  return EXIT_SUCCESS;
}

int
runRieszRotationMatrixInterfaceWithRieszFrequencyFilterBankGeneratorTest()
{
  constexpr unsigned int Dimension = 2;
  // Create a rotation matrix
  using ValueType = std::complex<double>;
  using SteerableMatrix = itk::RieszRotationMatrix<Dimension>;
  using SpatialRotationMatrix = SteerableMatrix::SpatialRotationMatrixType;
  SpatialRotationMatrix R;
  double                angle = itk::Math::pi_over_2;
  double                cosA = cos(angle);
  double                sinA = sin(angle);
  R[0][0] = cosA;
  R[0][1] = -sinA;
  R[1][0] = sinA;
  R[1][1] = cosA;

  // Create a RieszRotationMatrix
  constexpr unsigned int order2 = 2;
  SteerableMatrix        S(R, order2);
  S.SetDebugOn();
  S.ComputeSteerableMatrix();
  // f: order 2 dim 2:
  // typename SteerableMatrix::InternalMatrixType g(3, 3);
  // g(0, 0) = 0; g(0, 1) = 0;  g(0, 2) = 1;
  // g(1, 0) = 0; g(1, 1) = -1; g(1, 2) = 0;
  // g(2, 0) = 1; g(2, 1) = 0;  g(2, 2) = 0;

  // Set a std::vector<TImageType::Pointer>
  // emulating the output of RieszFrequencyFilterBankGenerator
  // of Order = 2, Dimension = 2

  using PixelType = std::complex<double>;
  using RealPixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ImagePointer = ImageType::Pointer;
  using RealImageType = itk::Image<RealPixelType, Dimension>;
  using RealImagePointer = RealImageType::Pointer;
  ImagePointer         image = ImageType::New();
  ImageType::IndexType start;
  start.Fill(0);
  ImageType::SizeType size;
  size.Fill(10);
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(start);
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(itk::NumericTraits<ImageType::PixelType>::OneValue());

  // M = number of outputs from RieszBankGenerator
  unsigned int M = 3;
  // Create M images with ALL pixels with value: 1, 2, 3 respectively.
  std::vector<ImagePointer> images(M);
  using MultiplyImageFilterType = itk::MultiplyImageFilter<ImageType>;
  for (unsigned int i = 0; i < M; i++)
  {
    typename MultiplyImageFilterType::Pointer multiplyImageFilter = MultiplyImageFilterType::New();
    multiplyImageFilter->SetInput(image);
    multiplyImageFilter->SetConstant(i + 1);
    multiplyImageFilter->Update();
    images[i] = multiplyImageFilter->GetOutput();
    std::cout << "images vector ";
    std::cout << images[i]->GetLargestPossibleRegion() << std::endl;
  }

  bool                   multiplyWithSomethingPassed = true;
  std::vector<PixelType> inputVector(M, 0);
  inputVector[0] = 1;
  inputVector[1] = 2;
  inputVector[2] = 3;
  auto                   vectorRotated = S.MultiplyWithVector(inputVector);
  std::vector<PixelType> expectedMultiplyResult(M, 0);
  expectedMultiplyResult[0] = 3;
  expectedMultiplyResult[1] = -2;
  expectedMultiplyResult[2] = 1;
  for (unsigned int i = 0; i < M; ++i)
  {
    if (!itk::Math::FloatAlmostEqual(vectorRotated[i].real(), expectedMultiplyResult[i].real()))
    {
      std::cout << "vectorRotated not equal!: ";
      std::cout << vectorRotated[i] << " != " << expectedMultiplyResult[i] << std::endl;
      multiplyWithSomethingPassed = false;
    }
  }

  itk::VariableSizeMatrix<PixelType> inputColumnMatrix(M, 1);
  inputColumnMatrix.GetVnlMatrix()(0, 0) = 1;
  inputColumnMatrix.GetVnlMatrix()(1, 0) = 2;
  inputColumnMatrix.GetVnlMatrix()(2, 0) = 3;
  auto columnMatrixRotated = S.MultiplyWithColumnMatrix(inputColumnMatrix);
  for (unsigned int i = 0; i < M; ++i)
  {
    if (!itk::Math::FloatAlmostEqual(columnMatrixRotated.GetVnlMatrix()(i, 0).real(), expectedMultiplyResult[i].real()))
    {
      std::cout << "columnMatrixRotated not Equal!: ";
      std::cout << columnMatrixRotated.GetVnlMatrix()(i, 0) << " != " << expectedMultiplyResult[i] << std::endl;
      multiplyWithSomethingPassed = false;
    }
  }

  auto imagesMultipliedByRieszRotationMatrix = S.MultiplyWithVectorOfImages<ImageType>(images);
  std::cout << "Size: ";
  std::cout << imagesMultipliedByRieszRotationMatrix.size() << std::endl;

  // Convert input images to real to perform comparison.
  std::vector<RealImagePointer> realImages(M);
  {
    for (unsigned int i = 0; i < M; ++i)
    {
      using ComplexToRealFilter = itk::ComplexToRealImageFilter<ImageType, RealImageType>;
      auto complexToRealFilter = ComplexToRealFilter::New();
      complexToRealFilter->SetInput(images[i]);
      complexToRealFilter->Update();
      realImages[i] = complexToRealFilter->GetOutput();
    }
  }
  std::vector<RealImagePointer> realImagesMultipliedByRieszRotationMatrix(M);
  {
    for (unsigned int i = 0; i < M; ++i)
    {
      using ComplexToRealFilter = itk::ComplexToRealImageFilter<ImageType, RealImageType>;
      auto complexToRealFilter = ComplexToRealFilter::New();
      complexToRealFilter->SetInput(imagesMultipliedByRieszRotationMatrix[i]);
      complexToRealFilter->Update();
      realImagesMultipliedByRieszRotationMatrix[i] = complexToRealFilter->GetOutput();
    }
  }

  // First
  // g(0, 0) = 0; g(0, 1) = 0;  g(0, 2) = 1;
  // result = 1.0 * images[2]
  int firstComponentStatus =
    compareRealImagesAndReport<RealImageType>(realImages[2], realImagesMultipliedByRieszRotationMatrix[0]);
  // Second
  // g(1, 0) = 0; g(1, 1) = -1; g(1, 2) = 0;
  // expectedresult = -1.0 * images[1]
  using MultiplyRealImageFilterType = itk::MultiplyImageFilter<RealImageType>;
  typename MultiplyRealImageFilterType::Pointer multiplyImageFilterInvert = MultiplyRealImageFilterType::New();
  multiplyImageFilterInvert->SetInput(realImagesMultipliedByRieszRotationMatrix[1]);
  multiplyImageFilterInvert->SetConstant(-1.0);
  multiplyImageFilterInvert->Update();
  int secondComponentStatus =
    compareRealImagesAndReport<RealImageType>(realImages[1], multiplyImageFilterInvert->GetOutput());
  // Third
  // g(2, 0) = 1; g(2, 1) = 0;  g(2, 2) = 0;
  // result = 1.0 * images[0]
  int thirdComponentStatus =
    compareRealImagesAndReport<RealImageType>(realImages[0], realImagesMultipliedByRieszRotationMatrix[2]);

  if (!multiplyWithSomethingPassed)
  {
    return EXIT_FAILURE;
  }
  return firstComponentStatus && secondComponentStatus && thirdComponentStatus;
}

template <unsigned int VDimension>
int
runRieszRotationMatrixTest()
{
  bool               testPassed = true;
  const unsigned int Dimension = VDimension;

  using ValueType = std::complex<double>;
  using SteerableMatrix = itk::RieszRotationMatrix<Dimension>;
  using SpatialRotationMatrix = typename SteerableMatrix::SpatialRotationMatrixType;

  // Define a spatial rotation matrix.
  SpatialRotationMatrix R;
  // double angle = itk::Math::pi_over_4;
  double angle = itk::Math::pi_over_2;
  double cosA = cos(angle);
  double sinA = sin(angle);
  double C = 1 - cosA;
  if (Dimension == 2)
  {
    R[0][0] = cosA;
    R[0][1] = -sinA;
    R[1][0] = sinA;
    R[1][1] = cosA;
  }

  itk::FixedArray<double, Dimension> dir;
  dir.Fill(0);
  if (Dimension == 3)
  {
    // Check the general form of a rotation in 3D given a
    // rotation axis and angle:
    // https://en.wikipedia.org/wiki/Rotation_matrix
    // Choose the rotation around the z axis.
    double x = dir[0] = 0;
    double y = dir[1] = 0;
    double z = dir[2] = 1;
    R[0][0] = x * x * C + cosA;
    R[0][1] = x * y * C - z * sinA;
    R[0][2] = x * z * C + y * sinA;

    R[1][0] = y * x * C + z * sinA;
    R[1][1] = y * y * C + cosA;
    R[1][2] = y * z * C - x * sinA;

    R[2][0] = z * x * C - y * sinA;
    R[2][1] = z * y * C + x * sinA;
    R[2][2] = z * z * C + cosA;
  }
  // Check constructors of SteerableMatrix.
  // default
  SteerableMatrix        Sdefault;
  constexpr unsigned int order1 = 1;
  Sdefault.SetOrder(order1);
  Sdefault.SetSpatialRotationMatrix(R);
  // compute
  SteerableMatrix S(R, order1);
  // copy
  SteerableMatrix Scopy(S);
  Scopy.SetOrder(2);
  Scopy.SetDebugOn();
  Scopy.ComputeSteerableMatrix();

  std::cout << "Rotation Matrix:" << std::endl;
  std::cout << R << std::endl;

  std::cout << "Matrix: Order 2" << std::endl;
  std::cout << Scopy << std::endl;

  typename SteerableMatrix::InternalMatrixType transposeMatrix = Scopy.GetTranspose();
  typename SteerableMatrix::InternalMatrixType identityMatrix = Scopy.GetVnlMatrix();
  identityMatrix.set_identity();
  typename SteerableMatrix::InternalMatrixType SmultST = Scopy.GetVnlMatrix() * transposeMatrix;

  if (SmultST != identityMatrix)
  {
    testPassed = false;
    std::cout << "test failed!" << std::endl;
    std::cerr << "S * S_transpose != identity" << std::endl;
    std::cout << "Matrix: S*S_transpose" << std::endl;
    std::cout << SmultST << std::endl;
  }

  // For angle = pi/2
  if (itk::Math::AlmostEquals(angle, itk::Math::pi_over_2))
  {
    if (Dimension == 2)
    {
      // f: order 2 dim 2:
      typename SteerableMatrix::InternalMatrixType g(3, 3);
      g(0, 0) = 0;
      g(0, 1) = 0;
      g(0, 2) = 1;
      g(1, 0) = 0;
      g(1, 1) = -1;
      g(1, 2) = 0;
      g(2, 0) = 1;
      g(2, 1) = 0;
      g(2, 2) = 0;
      if (Scopy.GetVnlMatrix() != g)
      {
        testPassed = false;
        std::cout << "test failed!" << std::endl;
        std::cerr << "expected result:" << std::endl;
        std::cerr << g << std::endl;
        std::cerr << "but got:" << std::endl;
        std::cerr << Scopy.GetVnlMatrix() << std::endl;
      }
    }
    if (Dimension == 3)
    {
      // f: order 2 dim 3:
      typename SteerableMatrix::InternalMatrixType f(6, 6);
      f(0, 0) = 0;
      f(0, 1) = 0;
      f(0, 2) = 0;
      f(0, 3) = 1;
      f(0, 4) = 0;
      f(0, 5) = 0;
      f(1, 0) = 0;
      f(1, 1) = -1;
      f(1, 2) = 0;
      f(1, 3) = 0;
      f(1, 4) = 0;
      f(1, 5) = 0;
      f(2, 0) = 0;
      f(2, 1) = 0;
      f(2, 2) = 0;
      f(2, 3) = 0;
      f(2, 4) = -1;
      f(2, 5) = 0;
      f(3, 0) = 1;
      f(3, 1) = 0;
      f(3, 2) = 0;
      f(3, 3) = 0;
      f(3, 4) = 0;
      f(3, 5) = 0;
      f(4, 0) = 0;
      f(4, 1) = 0;
      f(4, 2) = 1;
      f(4, 3) = 0;
      f(4, 4) = 0;
      f(4, 5) = 0;
      f(5, 0) = 0;
      f(5, 1) = 0;
      f(5, 2) = 0;
      f(5, 3) = 0;
      f(5, 4) = 0;
      f(5, 5) = 1;
      if (Scopy.GetVnlMatrix() != f)
      {
        testPassed = false;
        std::cout << "test failed!" << std::endl;
        std::cerr << "expected result:" << std::endl;
        std::cerr << f << std::endl;
        std::cerr << "but got:" << std::endl;
        std::cerr << Scopy.GetVnlMatrix() << std::endl;
      }
    }
  }
  std::cout << "Matrix: Order 1" << std::endl;
  std::cout << S << std::endl;

  // unsigned int highOrder = 10;
  // Sdefault.SetOrder(highOrder);
  // Sdefault.ComputeSteerableMatrix();
  // std::cout << "Matrix: Order " << highOrder << std::endl;
  // std::cout << "Matrix: size " << Sdefault.Rows() << std::endl;
  // std::cout << Sdefault << std::endl;

  if (testPassed)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}

int
itkRieszRotationMatrixTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << argv[0] << "dimension" << std::endl;
    return EXIT_FAILURE;
  }

  int interfaceStatus = runRieszRotationMatrixInterfaceWithRieszFrequencyFilterBankGeneratorTest();
  if (interfaceStatus == EXIT_FAILURE)
  {
    std::cerr << "Failure in the interface with FilterBankGenerator" << std::endl;
    return EXIT_FAILURE;
  }


  const unsigned int dimension = std::stoi(argv[1]);
  if (dimension == 2)
  {
    return runRieszRotationMatrixTest<2>();
  }
  else if (dimension == 3)
  {
    return runRieszRotationMatrixTest<3>();
  }
  else
  {
    std::cerr << "dimension = " << dimension << " . But test only work for 2 or 3 dimension." << std::endl;
    return EXIT_FAILURE;
  }
}
