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

#include "itkDiffusionTensor3D.h"
#include "itkImageRegionIterator.h"
#include "itkMath.h"

int
itkDiffusionTensor3DTest(int, char *[])
{
  // Test it all
  float val[6] = { 1.8f, 0.2f, 0.5f, 3.4f, 2.0f, 1.2f };

  using Float3DTensorType = itk::DiffusionTensor3D<float>;
  using Uchar3DTensorType = itk::DiffusionTensor3D<unsigned char>;

  Float3DTensorType pixel(val);

  unsigned char pixelInit0[6] = { 255, 255, 255, 128, 34, 17 };
  unsigned char pixelInit1[6] = { 255, 255, 244, 19, 23, 29 };

  Uchar3DTensorType pixelArray[2];
  pixelArray[0] = pixelInit0;
  pixelArray[1] = pixelInit1;

  std::cout << "sizeof(pixel) = " << sizeof(pixel) << '\n';
  if (sizeof(pixel) != 6 * sizeof(Float3DTensorType::ComponentType))
  {
    std::cerr << "ERROR: sizeof(pixel) == " << sizeof(pixel) << " but is should be "
              << 6 * sizeof(Float3DTensorType::ComponentType) << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "pixel.GetNumberOfComponents = " << pixel.GetNumberOfComponents() << '\n';
  std::cout << "pixel.GetNthComponent()" << '\n';
  bool passed = true;
  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); ++i)
  {
    if (itk::Math::NotExactlyEquals(pixel.GetNthComponent(i), val[i]))
    {
      std::cout << "Float3DTensorType pixel(val) failed." << '\n'
                << "\tExpected val[" << i << "] = " << val[i] << " but got pixel.GetNthComponent(" << i
                << ") = " << pixel.GetNthComponent(i) << '\n';
      passed = false;
    }
    else
    {
      std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << '\n';
    }
  }

  pixel(0, 0) = 11.0;
  pixel(0, 1) = 21.0;
  pixel(0, 2) = 15.0;
  pixel(1, 0) = 11.0;
  pixel(1, 1) = 31.0;
  pixel(1, 2) = 10.0;
  pixel(2, 0) = 11.0; // these three last element will overwrite its symmetric counterparts
  pixel(2, 1) = 41.0;
  pixel(2, 2) = 14.0;

  std::cout << "testing the pixel(i,j) API" << '\n';
  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); ++i)
  {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << '\n';
  }

  std::cout << "pixel[0] = 111; pixel[1] = 222; pixel[2] = 333;" << '\n';
  std::cout << "pixel[3] = 444; pixel[4] = 555; pixel[5] = 666;" << '\n';

  pixel[0] = 111;
  pixel[1] = 222;
  pixel[2] = 333;
  pixel[3] = 444;
  pixel[4] = 555;
  pixel[5] = 666;

  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); ++i)
  {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << '\n';
  }

  std::cout << "std::cout << pixel << '\n';" << '\n';
  std::cout << '\t' << pixel << '\n';

  for (unsigned int j = 0; j < 2; ++j)
  {
    std::cout << "pixelArray[" << j << "].GetNumberOfComponents = " << pixelArray[j].GetNumberOfComponents() << '\n';
    std::cout << "pixelArray[" << j << "].GetNthComponent()" << '\n';
    for (unsigned int i = 0; i < pixelArray[j].GetNumberOfComponents(); ++i)
    {
      std::cout << "\tpixelArray[" << j << "].GetNthComponent(" << i
                << ") = " << static_cast<int>(pixelArray[j].GetNthComponent(i)) << '\n';
    }
  }

  std::cout << "Testing arithmetic methods" << '\n';
  Float3DTensorType pa;
  Float3DTensorType pb;

  pa[0] = 1.25;
  pa[1] = 3.25;
  pa[2] = 5.25;
  pa[3] = 1.25;
  pa[4] = 3.25;
  pa[5] = 5.25;

  pb[0] = 1.55;
  pb[1] = 3.55;
  pb[2] = 5.55;
  pb[3] = 1.55;
  pb[4] = 3.55;
  pb[5] = 5.55;

  Float3DTensorType pc;

  pc = pa + pb;
  std::cout << "addition = " << pc << '\n';

  pc = pa - pb;
  std::cout << "subtraction = " << pc << '\n';

  pc += pb;
  std::cout << "in-place addition = " << pc << '\n';

  pc -= pb;
  std::cout << "in-place subtraction = " << pc << '\n';

  pc = pa * 3.2;
  std::cout << "product by scalar = " << pc << '\n';

  /** Create an Image of tensors  */
  using PixelType = Float3DTensorType;
  using ImageType = itk::Image<PixelType, 3>;

  auto dti = ImageType::New();

  ImageType::SizeType   size;
  ImageType::IndexType  start;
  ImageType::RegionType region;

  size[0] = 128;
  size[1] = 128;
  size[2] = 128;

  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  region.SetIndex(start);
  region.SetSize(size);

  dti->SetRegions(region);
  dti->Allocate();

  ImageType::SpacingType spacing;
  spacing[0] = 0.5;
  spacing[1] = 0.5;
  spacing[2] = 1.5;

  ImageType::PointType origin;
  origin[0] = 25.5;
  origin[1] = 25.5;
  origin[2] = 27.5;

  dti->SetOrigin(origin);
  dti->SetSpacing(spacing);

  PixelType tensor;

  tensor[0] = 1.2;
  tensor[1] = 2.2;
  tensor[2] = 3.2;
  tensor[3] = 4.2;
  tensor[4] = 5.2;
  tensor[5] = 6.2;

  dti->FillBuffer(tensor);

  using IteratorType = itk::ImageRegionIterator<ImageType>;

  IteratorType it(dti, region);
  it.GoToBegin();

  while (!it.IsAtEnd())
  {
    it.Set(tensor);
    ++it;
  }

  // Test Eigen values computation
  {
    using Double3DTensorType = itk::DiffusionTensor3D<double>;

    Double3DTensorType tensor2;

    double v[3];
    v[0] = 19.0;
    v[1] = 23.0;
    v[2] = 29.0;

    tensor2(0, 0) = v[0];
    tensor2(0, 1) = 0.0;
    tensor2(0, 2) = 0.0;
    tensor2(1, 0) = 0.0; // overrides (0,1)
    tensor2(1, 1) = v[1];
    tensor2(1, 2) = 0.0;
    tensor2(2, 0) = 0.0; // overrides (0,2)
    tensor2(2, 1) = 0.0; // overrides (1,2)
    tensor2(2, 2) = v[2];

    std::cout << "DiffusionTensor3D = " << '\n';
    std::cout << tensor2 << '\n';

    Double3DTensorType::EigenValuesArrayType   eigenValues;
    Double3DTensorType::EigenVectorsMatrixType eigenVectors;

    tensor2.ComputeEigenAnalysis(eigenValues, eigenVectors);

    std::cout << "EigenValues = " << '\n';
    std::cout << eigenValues << '\n';

    std::cout << "EigenVectors = " << '\n';
    std::cout << eigenVectors << '\n';

    const double tolerance = 1e-4;

    {
      Double3DTensorType::EigenValuesArrayType expectedValues;
      expectedValues[0] = v[0];
      expectedValues[1] = v[1];
      expectedValues[2] = v[2];

      for (unsigned int i = 0; i < 3; ++i)
      {
        if (itk::Math::abs(expectedValues[i] - eigenValues[i]) > tolerance)
        {
          std::cerr << "Eigenvalue computation failed" << '\n';
          std::cerr << "expectedValues = " << expectedValues << '\n';
          std::cerr << "eigenValues    = " << eigenValues << '\n';
          return EXIT_FAILURE;
        }
      }
    }

    // Now let's do something more involved...
    tensor2(0, 0) = 7.0;
    tensor2(0, 1) = 0.0;
    tensor2(0, 2) = 3.0;
    tensor2(1, 0) = 0.0; // overrides (0,1)
    tensor2(1, 1) = 0.0;
    tensor2(1, 2) = 0.0;
    tensor2(2, 0) = 3.0; // overrides (0,2)
    tensor2(2, 1) = 0.0; // overrides (1,2)
    tensor2(2, 2) = 7.0;

    std::cout << "DiffusionTensor3D = " << '\n';
    std::cout << tensor2 << '\n';

    tensor2.ComputeEigenAnalysis(eigenValues, eigenVectors);

    std::cout << "EigenValues = " << '\n';
    std::cout << eigenValues << '\n';

    std::cout << "EigenVectors = " << '\n';
    std::cout << eigenVectors << '\n';

    {
      Double3DTensorType::EigenValuesArrayType expectedValues;
      expectedValues[0] = 0.0;
      expectedValues[1] = 4.0;
      expectedValues[2] = 10.0;

      for (unsigned int i = 0; i < 3; ++i)
      {
        if (itk::Math::abs(expectedValues[i] - eigenValues[i]) > tolerance)
        {
          std::cerr << "Eigenvalue computation failed" << '\n';
          std::cerr << "expectedValues = " << expectedValues << '\n';
          std::cerr << "eigenValues    = " << eigenValues << '\n';
          return EXIT_FAILURE;
        }
      }
    }

    // Now let's do one where we know the rotation...
    tensor2(0, 0) = 9.0;
    tensor2(0, 1) = 0.0;
    tensor2(0, 2) = 7.0;
    tensor2(1, 0) = 0.0; // overrides (0,1)
    tensor2(1, 1) = 0.0;
    tensor2(1, 2) = 0.0;
    tensor2(2, 0) = 7.0; // overrides (0,2)
    tensor2(2, 1) = 0.0; // overrides (1,2)
    tensor2(2, 2) = 3.0;

    std::cout << "DiffusionTensor3D = " << '\n';
    std::cout << tensor2 << '\n';

    tensor2.ComputeEigenAnalysis(eigenValues, eigenVectors);

    std::cout << "EigenValues = " << '\n';
    std::cout << eigenValues << '\n';

    std::cout << "EigenVectors = " << '\n';
    std::cout << eigenVectors << '\n';

    {
      Double3DTensorType::EigenValuesArrayType expectedValues;
      expectedValues[0] = -1.61577;
      expectedValues[1] = 0.00000;
      expectedValues[2] = 13.61580;

      for (unsigned int i = 0; i < 3; ++i)
      {
        if (itk::Math::abs(expectedValues[i] - eigenValues[i]) > tolerance)
        {
          std::cerr << "Eigenvalue computation failed" << '\n';
          std::cerr << "expectedValues = " << expectedValues << '\n';
          std::cerr << "eigenValues    = " << eigenValues << '\n';
          return EXIT_FAILURE;
        }
      }
    }
  }

  // Test GetTrace() and GetFractionalAnisotropy methods
  {

    using Double3DTensorType = itk::DiffusionTensor3D<double>;
    using AccumulateValueType = Double3DTensorType::AccumulateValueType;
    using RealValueType = Double3DTensorType::RealValueType;

    Double3DTensorType tensor3;

    tensor3(0, 0) = 19.0;
    tensor3(0, 1) = 0.0;
    tensor3(0, 2) = 0.0;
    tensor3(1, 0) = 0.0; // overrides (0,1)
    tensor3(1, 1) = 23.0;
    tensor3(1, 2) = 0.0;
    tensor3(2, 0) = 7.0; // overrides (0,2)
    tensor3(2, 1) = 0.0; // overrides (1,2)
    tensor3(2, 2) = 29.0;

    AccumulateValueType expectedTrace{};

    expectedTrace += tensor3(0, 0);
    expectedTrace += tensor3(1, 1);
    expectedTrace += tensor3(2, 2);

    const double tolerance = 1e-4;

    const AccumulateValueType computedTrace = tensor3.GetTrace();
    if (itk::Math::abs(computedTrace - expectedTrace) > tolerance)
    {
      std::cerr << "Error computing the Trace" << '\n';
      std::cerr << "Expected trace = " << expectedTrace << '\n';
      std::cerr << "Computed trace = " << computedTrace << '\n';
      return EXIT_FAILURE;
    }

    // Test the value of internal scalar product
    constexpr RealValueType expectedInternalScalarProduct = 1829;

    const RealValueType computedInternalScalarProduct = tensor3.GetInnerScalarProduct();
    if (itk::Math::abs(computedInternalScalarProduct - expectedInternalScalarProduct) > tolerance)
    {
      std::cerr << "Error computing Internal Scalar Product" << '\n';
      std::cerr << "Expected = " << expectedInternalScalarProduct << '\n';
      std::cerr << "Computed = " << computedInternalScalarProduct << '\n';
      return EXIT_FAILURE;
    }


    // Test the value of Fractional Anisotropy
    constexpr RealValueType expectedFractionalAnisotropy = 0.349177;

    const RealValueType computedFractionalAnisotropy = tensor3.GetFractionalAnisotropy();
    if (itk::Math::abs(computedFractionalAnisotropy - expectedFractionalAnisotropy) > tolerance)
    {
      std::cerr << "Error computing Fractional Anisotropy" << '\n';
      std::cerr << "Expected = " << expectedFractionalAnisotropy << '\n';
      std::cerr << "Computed = " << computedFractionalAnisotropy << '\n';
      return EXIT_FAILURE;
    }

    // Test the value of Relative Anisotropy
    constexpr RealValueType expectedRelativeAnisotropy = 1.9044;

    const RealValueType computedRelativeAnisotropy = tensor3.GetRelativeAnisotropy();
    if (itk::Math::abs(computedRelativeAnisotropy - expectedRelativeAnisotropy) > tolerance)
    {
      std::cerr << "Error computing Relative Anisotropy" << '\n';
      std::cerr << "Expected = " << expectedRelativeAnisotropy << '\n';
      std::cerr << "Computed = " << computedRelativeAnisotropy << '\n';
      return EXIT_FAILURE;
    }


  } // end of Test GetTrace() method

  // Test Numeric Traits
  {
    using TensorType = itk::DiffusionTensor3D<int>;

    const TensorType maxTensor = itk::NumericTraits<TensorType>::max();
    std::cout << maxTensor << '\n';

    const TensorType minTensor = itk::NumericTraits<TensorType>::min();
    std::cout << minTensor << '\n';

    const TensorType nonpositiveMinTensor = itk::NumericTraits<TensorType>::NonpositiveMin();
    std::cout << nonpositiveMinTensor << '\n';

    const TensorType zeroValue{};
    std::cout << zeroValue << '\n';

    const TensorType oneValue = itk::NumericTraits<TensorType>::OneValue();
    std::cout << oneValue << '\n';

    const TensorType zero{};
    std::cout << zero << '\n';

    const TensorType one = itk::NumericTraits<TensorType>::OneValue();
    std::cout << one << '\n';
  }

  // Test casting constructors
  {
    using Int3DTensorType = itk::DiffusionTensor3D<int>;

    Int3DTensorType intTensor;
    intTensor[0] = 1;
    intTensor[1] = -2;
    intTensor[2] = 3;
    intTensor[3] = 4;
    intTensor[4] = 5;
    intTensor[5] = 6;

    // Test constructors
    Float3DTensorType floatTensor(intTensor);

    // test Assignment
    Float3DTensorType floatTensor2 = intTensor;

    // Test casting
    auto floatTensor3 = static_cast<Float3DTensorType>(intTensor);

    // Check that all floatTensors have are the same
    const float precision = 1e-6;
    for (unsigned int i = 0; i < Float3DTensorType::InternalDimension; ++i)
    {
      auto intVal = static_cast<float>(intTensor[i]);
      if ((floatTensor[i] - intVal) > precision || (floatTensor2[i] - intVal) > precision ||
          (floatTensor3[i] - intVal) > precision)
      {
        std::cerr << "Error failed casting/templated Constructor Test" << '\n';
        return EXIT_FAILURE;
      }
    }
  }

  return (passed ? EXIT_SUCCESS : EXIT_FAILURE);
}
