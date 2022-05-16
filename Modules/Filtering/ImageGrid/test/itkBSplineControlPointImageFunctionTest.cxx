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
#include "itkBSplineControlPointImageFunction.h"
#include "itkTestingMacros.h"

int
itkBSplineControlPointImageFunctionTest(int, char *[])
{

  // We construct a B-spline parametric curve equal to f(u) = 0.5 * u^2 - 0.5 * u
  // + 1/6.  This is done using a cubic order spline with controls points
  // (1, 0, 0, 1)

  constexpr unsigned int ParametricDimension = 1;
  constexpr unsigned int DataDimension = 1;

  using RealType = float;
  using VectorType = itk::Vector<RealType, DataDimension>;
  using VectorImageType = itk::Image<VectorType, ParametricDimension>;

  auto phiLattice = VectorImageType::New();

  VectorImageType::SizeType    size;
  VectorImageType::SpacingType spacing;
  VectorImageType::PointType   origin;

  size.Fill(4);
  spacing.Fill(1.0);
  origin.Fill(0.0);
  phiLattice->SetOrigin(origin);
  phiLattice->SetSpacing(spacing);
  phiLattice->SetRegions(size);
  phiLattice->Allocate();
  phiLattice->FillBuffer(VectorType{});

  // To create the specified function, the first and last control points have
  // a value of 1.0;

  VectorImageType::IndexType index;
  VectorImageType::PixelType value;
  index.Fill(0);
  value.Fill(1.0);
  phiLattice->SetPixel(index, value);
  index.Fill(3);
  value.Fill(1.0);
  phiLattice->SetPixel(index, value);

  using BSplinerType = itk::BSplineControlPointImageFunction<VectorImageType>;
  auto bspliner = BSplinerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(bspliner, BSplineControlPointImageFunction, ImageFunction);

  // Define the parametric domain [0, 1).
  origin.Fill(0);
  spacing.Fill(0.01);
  size.Fill(101);

  bspliner->SetOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, bspliner->GetOrigin());

  bspliner->SetSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, bspliner->GetSpacing());

  bspliner->SetSize(size);
  ITK_TEST_SET_GET_VALUE(size, bspliner->GetSize());

  unsigned int bSplineOrderValue = 3;
  bspliner->SetSplineOrder(bSplineOrderValue);
  for (auto i : bspliner->GetSplineOrder())
  {
    if (i != bSplineOrderValue)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in itk::BSplineControlPointImageFunction::GetSplineOrder" << std::endl;
      std::cerr << "Expected: " << bSplineOrderValue << ", but got: " << i << std::endl;
      return EXIT_FAILURE;
    }
  }

  BSplinerType::ArrayType bSplineOrder;
  bSplineOrder.Fill(bSplineOrderValue);
  bspliner->SetSplineOrder(bSplineOrder);
  ITK_TEST_SET_GET_VALUE(bSplineOrder, bspliner->GetSplineOrder());

  BSplinerType::ArrayType::ValueType closeDimensionValue = 0;
  BSplinerType::ArrayType            closeDimension;
  closeDimension.Fill(closeDimensionValue);
  bspliner->SetCloseDimension(closeDimension);
  ITK_TEST_SET_GET_VALUE(closeDimension, bspliner->GetCloseDimension());

  BSplinerType::RealType bSplineEpsilon = 1e-3;
  bspliner->SetBSplineEpsilon(bSplineEpsilon);
  ITK_TEST_SET_GET_VALUE(bSplineEpsilon, bspliner->GetBSplineEpsilon());


  bspliner->SetInputImage(phiLattice);

  BSplinerType::PointType    point;
  BSplinerType::GradientType gradient;
  BSplinerType::GradientType hessianComponent;
  BSplinerType::OutputType   data;

  // f(0) = 1/6;
  // f'(u) = u - 0.5 so f'(0) should be -0.5.
  // f"(u) = 1
  try
  {
    point[0] = 0.0;

    data = bspliner->EvaluateAtParametricPoint(point);
    if (itk::Math::abs(data[0] - 0.166666666667) > 1e-5)
    {
      std::cerr << "Evaluate1: data is further away from the expected value." << std::endl;
      return EXIT_FAILURE;
    }

    gradient = bspliner->EvaluateGradientAtParametricPoint(point);
    if (itk::Math::abs(gradient(0, 0) + 0.5) > 1e-5)
    {
      std::cerr << "Evaluate1: gradient is further away from the expected value." << std::endl;
      return EXIT_FAILURE;
    }

    hessianComponent = bspliner->EvaluateHessianAtParametricPoint(point, 0);

    if (itk::Math::abs(hessianComponent(0, 0) - 1.0) > 1e-5)
    {
      std::cerr << "Evaluate1: hessian is further away from the expected value." << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (...)
  {
    std::cerr << "Error in evaluate functions" << std::endl;
    return EXIT_FAILURE;
  }

  // f(0.351) = 0.05276717;
  // f'(0.351) = -0.149
  try
  {
    point[0] = 0.351;

    data = bspliner->EvaluateAtParametricPoint(point);
    if (itk::Math::abs(data[0] - 0.05276717) > 1e-5)
    {
      std::cerr << "Evaluate2: data is further away from the expected value." << std::endl;
      return EXIT_FAILURE;
    }

    gradient = bspliner->EvaluateGradientAtParametricPoint(point);
    if (itk::Math::abs(gradient(0, 0) + 0.149) > 1e-5)
    {
      std::cerr << "Evaluate2: gradient is further away from the expected value." << std::endl;
      return EXIT_FAILURE;
    }

    hessianComponent = bspliner->EvaluateHessianAtParametricPoint(point, 0);
    if (itk::Math::abs(hessianComponent(0, 0) - 1.0) > 1e-5)
    {
      std::cerr << "Evaluate2: hessian is further away from the expected value." << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (...)
  {
    std::cerr << "Error in evaluate functions" << std::endl;
    return EXIT_FAILURE;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
