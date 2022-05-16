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

#include "itkAnnulusOperator.h"
#include "itkBackwardDifferenceOperator.h"
#include "itkDerivativeOperator.h"
#include "itkForwardDifferenceOperator.h"
#include "itkGaussianDerivativeOperator.h"
#include "itkGaussianOperator.h"
#include "itkImageKernelOperator.h"
#include "itkLaplacianOperator.h"
#include "itkNeighborhoodOperator.h"
#include "itkSobelOperator.h"
#include "itkTestingMacros.h"

#include <type_traits> // For is_default_constructible, is_copy_constructible, etc.

namespace
{
template <typename T>
constexpr bool
IsDefaultConstructibleCopyableNoThrowMovableAndDestructible()
{
  return std::is_default_constructible<T>::value && std::is_copy_constructible<T>::value &&
         std::is_copy_assignable<T>::value && std::is_nothrow_move_constructible<T>::value &&
         std::is_nothrow_move_assignable<T>::value && std::is_nothrow_destructible<T>::value;
}

static_assert(std::is_copy_assignable<itk::NeighborhoodOperator<int, 3>>::value,
              "NeighborhoodOperator should be copy-assignable.");
static_assert(std::is_nothrow_move_assignable<itk::NeighborhoodOperator<int, 3>>::value,
              "NeighborhoodOperator should be noexcept move-assignable.");

static_assert(IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::AnnulusOperator<int>>(),
              "AnnulusOperator should be default-constructible, copyable, noexcept movable, and destructible.");
static_assert(
  IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::BackwardDifferenceOperator<int>>(),
  "BackwardDifferenceOperator should be default-constructible, copyable, noexcept movable, and destructible.");
static_assert(IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::DerivativeOperator<int>>(),
              "DerivativeOperator should be default-constructible, copyable, noexcept movable, and destructible.");
static_assert(
  IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::ForwardDifferenceOperator<int>>(),
  "ForwardDifferenceOperator should be default-constructible, copyable, noexcept movable, and destructible.");
static_assert(
  IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::GaussianDerivativeOperator<int>>(),
  "GaussianDerivativeOperator should be default-constructible, copyable, noexcept movable, and destructible.");
static_assert(IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::GaussianOperator<int>>(),
              "GaussianOperator should be default-constructible, copyable, noexcept movable, and destructible.");
static_assert(IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::ImageKernelOperator<int>>(),
              "ImageKernelOperator should be default-constructible, copyable, noexcept movable, and destructible.");
static_assert(IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::LaplacianOperator<int>>(),
              "LaplacianOperator should be default-constructible, copyable, noexcept movable, and destructible.");
static_assert(IsDefaultConstructibleCopyableNoThrowMovableAndDestructible<itk::SobelOperator<int>>(),
              "SobelOperator should be default-constructible, copyable, noexcept movable, and destructible.");
} // namespace

int
itkNeighborhoodOperatorTest(int, char *[])
{
  using PixelType = float;
  constexpr unsigned int Dimension1D = 1;
  constexpr unsigned int Dimension2D = 2;
  constexpr unsigned int Dimension3D = 3;
  constexpr unsigned int Dimension4D = 4;

  std::cout << "Testing derivative operator" << std::endl;
  itk::DerivativeOperator<PixelType, Dimension3D, vnl_vector<PixelType>> d;

  ITK_EXERCISE_BASIC_OBJECT_METHODS((&d), DerivativeOperator, NeighborhoodOperator);


  const unsigned int order = 2;
  d.SetOrder(order);
  ITK_TEST_SET_GET_VALUE(order, d.GetOrder());

  unsigned long direction = 1;
  d.SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, d.GetDirection());

  d.CreateDirectional();


  std::cout << "Testing Gaussian operator" << std::endl;
  itk::GaussianOperator<PixelType, Dimension2D, vnl_vector<PixelType>> g;

  ITK_EXERCISE_BASIC_OBJECT_METHODS((&g), GaussianOperator, NeighborhoodOperator);


  const double variance = 2.3;
  g.SetVariance(variance);
  ITK_TEST_SET_GET_VALUE(variance, g.GetVariance());

  const double maximumError = .01;
  g.SetMaximumError(maximumError);
  ITK_TEST_SET_GET_VALUE(maximumError, g.GetMaximumError());

  g.CreateDirectional();


  std::cout << "Testing ForwardDifferenceOperator" << std::endl;
  itk::ForwardDifferenceOperator<PixelType, Dimension4D, vnl_vector<PixelType>> f;

  ITK_EXERCISE_BASIC_OBJECT_METHODS((&f), ForwardDifferenceOperator, NeighborhoodOperator);

  direction = 2;
  f.SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, f.GetDirection());

  itk::Size<4> sz;
  sz[0] = sz[1] = sz[2] = sz[3] = 2;
  f.CreateToRadius(sz);


  std::cout << "Testing BackwardDifferenceOperator" << std::endl;
  itk::BackwardDifferenceOperator<PixelType, Dimension2D, vnl_vector<PixelType>> b;

  ITK_EXERCISE_BASIC_OBJECT_METHODS((&b), BackwardDifferenceOperator, NeighborhoodOperator);


  direction = 0;
  b.SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, b.GetDirection());

  b.CreateDirectional();


  std::cout << "Testing 1D LaplacianOperator" << std::endl;
  itk::LaplacianOperator<PixelType, Dimension1D, vnl_vector<PixelType>> a1;

  ITK_EXERCISE_BASIC_OBJECT_METHODS((&a1), LaplacianOperator, NeighborhoodOperator);


  const double scales_1d[] = { 1.0 / 2.0 };
  a1.SetDerivativeScalings(scales_1d);
  ITK_TEST_SET_GET_VALUE(*scales_1d, *(a1.GetDerivativeScalings()));

  a1.CreateOperator();

  std::cout << "    Operator = [ ";
  for (unsigned int i = 0; i < a1.Size(); ++i)
  {
    std::cout << a1[i];
    if (i < a1.Size() - 1)
    {
      std::cout << ", ";
    }
  }
  std::cout << "]" << std::endl << std::endl;

  std::cout << "Testing 2D LaplacianOperator" << std::endl;
  itk::LaplacianOperator<PixelType, Dimension2D, vnl_vector<PixelType>> a2;

  const double scales_2d[] = { 1.0 / 2.0, 1.0 / 4.0 };
  a2.SetDerivativeScalings(scales_2d);
  ITK_TEST_SET_GET_VALUE(*scales_2d, *(a2.GetDerivativeScalings()));

  a2.CreateOperator();

  std::cout << "    Operator = [ ";
  for (unsigned int i = 0; i < a2.Size(); ++i)
  {
    std::cout << a2[i];
    if (i < a2.Size() - 1)
    {
      std::cout << ", ";
    }
  }
  std::cout << "]" << std::endl << std::endl;

  std::cout << "Testing 3D LaplacianOperator" << std::endl;
  itk::LaplacianOperator<PixelType, Dimension3D, vnl_vector<PixelType>> a3;

  const double scales_3d[] = { 1.0 / 2.0, 1.0 / 4.0, 1.0 / 5.0 };
  a3.SetDerivativeScalings(scales_3d);
  ITK_TEST_SET_GET_VALUE(*scales_3d, *(a3.GetDerivativeScalings()));

  a3.CreateOperator();

  std::cout << "    Operator = [ ";
  for (unsigned int i = 0; i < a3.Size(); ++i)
  {
    std::cout << a3[i];
    if (i < a3.Size() - 1)
    {
      std::cout << ", ";
    }
  }
  std::cout << "]" << std::endl << std::endl;

  std::cout << "Testing SobelOperator2D" << std::endl;
  itk::SobelOperator<PixelType, Dimension2D, vnl_vector<PixelType>> c;

  ITK_EXERCISE_BASIC_OBJECT_METHODS((&c), SobelOperator, NeighborhoodOperator);

  direction = 0;
  c.SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, c.GetDirection());

  c.CreateDirectional();

  std::cout << "    Operator = [ ";
  for (unsigned int i = 0; i < c.Size(); ++i)
  {
    std::cout << c[i];
    if (i < c.Size() - 1)
    {
      std::cout << ", ";
    }
  }
  std::cout << "]" << std::endl << std::endl;

  direction = 1;
  c.SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, c.GetDirection());

  c.CreateDirectional();

  std::cout << "    Operator = [ ";
  for (unsigned int i = 0; i < c.Size(); ++i)
  {
    std::cout << c[i];
    if (i < c.Size() - 1)
    {
      std::cout << ", ";
    }
  }
  std::cout << "]" << std::endl << std::endl;

  std::cout << "Testing SobelOperator3D" << std::endl;
  itk::SobelOperator<PixelType, Dimension3D, vnl_vector<PixelType>> c2;

  direction = 0;
  c2.SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, c2.GetDirection());

  c2.CreateDirectional();

  std::cout << "    Operator = [ ";
  for (unsigned int i = 0; i < c2.Size(); ++i)
  {
    std::cout << c2[i];
    if (i < c2.Size() - 1)
    {
      std::cout << ", ";
    }
  }
  std::cout << "]" << std::endl << std::endl;

  direction = 1;
  c2.SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, c2.GetDirection());

  c2.CreateDirectional();

  std::cout << "    Operator = [ ";
  for (unsigned int i = 0; i < c2.Size(); ++i)
  {
    std::cout << c2[i];
    if (i < c2.Size() - 1)
    {
      std::cout << ", ";
    }
  }
  std::cout << "]" << std::endl << std::endl;

  direction = 2;
  c2.SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, c2.GetDirection());

  c2.CreateDirectional();

  std::cout << "    Operator = [ ";
  for (unsigned int i = 0; i < c2.Size(); ++i)
  {
    std::cout << c2[i];
    if (i < c2.Size() - 1)
    {
      std::cout << ", ";
    }
  }
  std::cout << "]" << std::endl << std::endl;


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
