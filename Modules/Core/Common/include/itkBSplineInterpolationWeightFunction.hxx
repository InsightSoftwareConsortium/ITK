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
#ifndef itkBSplineInterpolationWeightFunction_hxx
#define itkBSplineInterpolationWeightFunction_hxx

#include "itkBSplineKernelFunction.h"
#include "itkImage.h"
#include "itkMatrix.h"
#include "itkMath.h"

namespace itk
{

#if __cplusplus < 201703L
// For compatibility with pre C++17 International Standards, a constexpr static
// data member may be redundantly redeclared outside the class with no
// initializer. This usage is deprecated.
//
// For C++14 and earlier, the definition of static constexpr must be outside of
// class until C++17, see .h file for declaration & initialization

template <typename TCoordRep, unsigned int VSpaceDimension, unsigned int VSplineOrder>
constexpr typename BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>::SizeType
  BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>::SupportSize;
#endif

/** Compute weights for interpolation at continuous index position */
template <typename TCoordRep, unsigned int VSpaceDimension, unsigned int VSplineOrder>
typename BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>::WeightsType
BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>::Evaluate(
  const ContinuousIndexType & index) const
{
  WeightsType weights;
  IndexType   startIndex;

  this->Evaluate(index, weights, startIndex);

  return weights;
}

/** Compute weights for interpolation at continuous index position */
template <typename TCoordRep, unsigned int VSpaceDimension, unsigned int VSplineOrder>
void
BSplineInterpolationWeightFunction<TCoordRep, VSpaceDimension, VSplineOrder>::Evaluate(
  const ContinuousIndexType & index,
  WeightsType &               weights,
  IndexType &                 startIndex) const
{
  unsigned int j, k;

  // Find the starting index of the support region
  for (j = 0; j < SpaceDimension; ++j)
  {
    // Note that the expression passed to Math::Floor is adapted to work around
    // a compiler bug which caused endless compilations (apparently), by
    // Visual C++ 2015 Update 3, on 64-bit builds of Release configurations.
    startIndex[j] = Math::Floor<IndexValueType>(index[j] + 0.5 - SplineOrder / 2.0);
  }

  // Compute the weights
  Matrix<double, SpaceDimension, SplineOrder + 1> weights1D;
  for (j = 0; j < SpaceDimension; ++j)
  {
    double x = index[j] - static_cast<double>(startIndex[j]);

    for (k = 0; k <= SplineOrder; ++k)
    {
      weights1D[j][k] = BSplineKernelFunction<SplineOrder>::FastEvaluate(x);
      x -= 1.0;
    }
  }

  for (k = 0; k < Self::NumberOfWeights; ++k)
  {
    weights[k] = 1.0;

    for (j = 0; j < SpaceDimension; ++j)
    {
      weights[k] *= weights1D[j][m_OffsetToIndexTable[k][j]];
    }
  }
}
} // end namespace itk

#endif
