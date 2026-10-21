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
#ifndef itkDirectionCollapse_h
#define itkDirectionCollapse_h

#include "itkBridgeMathDeterminant.h"
#include "itkFixedArray.h"
#include "itkMacro.h"
#include "itkMatrix.h"
#include "itkVariableSizeMatrix.h"
#include "ITKCommonExport.h"

#include <cstdint>
#include <ostream>
#include <vector>

namespace itk
{

/** \class ExtractImageFilterEnums
 *
 * \brief enums for itk::ExtractImageFilter
 *
 * \ingroup ITKCommon
 */
class ExtractImageFilterEnums
{
public:
  /**
   * \ingroup ITKCommon
   * Strategy to be used to collapse physical space dimensions
   */
  enum class DirectionCollapseStrategy : uint8_t
  {
    DIRECTIONCOLLAPSETOUNKOWN = 0,
    DIRECTIONCOLLAPSETOIDENTITY = 1,
    DIRECTIONCOLLAPSETOSUBMATRIX = 2,
    DIRECTIONCOLLAPSETOGUESS = 3
  };
};

/** Define how to print enumerations */
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const ExtractImageFilterEnums::DirectionCollapseStrategy value);

/** Compute the direction of an image whose axes have been collapsed.
 *
 * Rows and columns of \a direction whose \a collapseDimensions entry is true
 * are removed, and the remaining sub-matrix is resolved by \a strategy:
 * DIRECTIONCOLLAPSETOIDENTITY returns identity, DIRECTIONCOLLAPSETOSUBMATRIX
 * returns the sub-matrix and throws when it is singular, and
 * DIRECTIONCOLLAPSETOGUESS returns the sub-matrix or identity when singular.
 * DIRECTIONCOLLAPSETOUNKOWN throws.
 *
 * The input dimension is taken from \a direction at run time, which lets
 * callers such as ImageFileReader use it with an ImageIOBase direction. The
 * number of retained axes must equal VOutputDimension. The result has element
 * type TValue, so it can be assigned to a float DirectionType.
 */
template <unsigned int VOutputDimension, typename TValue = double>
Matrix<TValue, VOutputDimension, VOutputDimension>
CollapseDirection(const VariableSizeMatrix<double> &                       direction,
                  const std::vector<bool> &                                collapseDimensions,
                  const ExtractImageFilterEnums::DirectionCollapseStrategy strategy)
{
  using StrategyEnum = ExtractImageFilterEnums::DirectionCollapseStrategy;
  using SubMatrixType = Matrix<double, VOutputDimension, VOutputDimension>;
  using ResultType = Matrix<TValue, VOutputDimension, VOutputDimension>;
  const auto convert = [](const SubMatrixType & matrix) {
    ResultType result;
    for (unsigned int i = 0; i < VOutputDimension; ++i)
    {
      for (unsigned int j = 0; j < VOutputDimension; ++j)
      {
        result[i][j] = static_cast<TValue>(matrix[i][j]);
      }
    }
    return result;
  };

  const unsigned int inputDimension = direction.Rows();
  if (direction.Cols() != inputDimension || collapseDimensions.size() != inputDimension)
  {
    itkGenericExceptionMacro("The direction matrix must be square (" << direction.Rows() << 'x' << direction.Cols()
                                                                     << ") with one collapse flag per axis ("
                                                                     << collapseDimensions.size() << ").");
  }

  SubMatrixType subMatrix{};
  unsigned int  row = 0;
  for (unsigned int i = 0; i < inputDimension; ++i)
  {
    if (collapseDimensions[i])
    {
      continue;
    }
    unsigned int col = 0;
    for (unsigned int j = 0; j < inputDimension; ++j)
    {
      if (collapseDimensions[j])
      {
        continue;
      }
      if (row < VOutputDimension && col < VOutputDimension)
      {
        subMatrix[row][col] = direction(i, j);
      }
      ++col;
    }
    ++row;
  }

  if (row != VOutputDimension)
  {
    itkGenericExceptionMacro("The number of retained dimensions (" << row << ") does not match the output dimension ("
                                                                   << VOutputDimension << ").");
  }

  switch (strategy)
  {
    case StrategyEnum::DIRECTIONCOLLAPSETOIDENTITY:
      return ResultType::GetIdentity();
    case StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX:
    {
      // Test the converted matrix: a block invertible in double can be singular in float.
      const ResultType collapsed = convert(subMatrix);
      if (bridge::Math::Determinant(collapsed) == TValue{ 0 })
      {
        itkGenericExceptionMacro("Invalid submatrix extracted for collapsed direction.");
      }
      return collapsed;
    }
    case StrategyEnum::DIRECTIONCOLLAPSETOGUESS:
    {
      const ResultType collapsed = convert(subMatrix);
      if (bridge::Math::Determinant(collapsed) == TValue{ 0 })
      {
        return ResultType::GetIdentity();
      }
      return collapsed;
    }
    case StrategyEnum::DIRECTIONCOLLAPSETOUNKOWN:
    default:
      itkGenericExceptionMacro(
        "It is required that the strategy for collapsing the direction matrix be explicitly specified. "
        "Set with either myfilter->SetDirectionCollapseToIdentity() or "
        "myfilter->SetDirectionCollapseToSubmatrix().");
  }
}

/** Overload for a direction whose dimension is known at compile time. */
template <unsigned int VOutputDimension, typename TValue, unsigned int VInputDimension>
Matrix<TValue, VOutputDimension, VOutputDimension>
CollapseDirection(const Matrix<TValue, VInputDimension, VInputDimension> & direction,
                  const FixedArray<bool, VInputDimension> &                collapseDimensions,
                  const ExtractImageFilterEnums::DirectionCollapseStrategy strategy)
{
  VariableSizeMatrix<double> variableDirection(VInputDimension, VInputDimension);
  for (unsigned int i = 0; i < VInputDimension; ++i)
  {
    for (unsigned int j = 0; j < VInputDimension; ++j)
    {
      variableDirection(i, j) = direction[i][j];
    }
  }
  return CollapseDirection<VOutputDimension, TValue>(
    variableDirection, std::vector<bool>(collapseDimensions.begin(), collapseDimensions.end()), strategy);
}

} // namespace itk

#endif
