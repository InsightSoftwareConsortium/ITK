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
#include "itkDirectionAlgorithm.h"
#include "itkBridgeMathDeterminant.h"
#include "itkMacro.h"

#include <vector>

namespace itk
{
/** Define how to print enumeration */
std::ostream &
operator<<(std::ostream & out, const ExtractImageFilterEnums::DirectionCollapseStrategy value)
{
  return out << [value] {
    switch (value)
    {
      case ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOUNKOWN:
        return "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOUNKOWN";
      case ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOIDENTITY:
        return "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOIDENTITY";
      case ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOSUBMATRIX:
        return "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOSUBMATRIX";
      case ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOGUESS:
        return "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOGUESS";
      default:
        return "INVALID VALUE FOR ExtractImageFilterEnums::DirectionCollapseStrategy";
    }
  }();
}
namespace
{
template <typename TValue>
void
CollapseDirectionImpl(
  const Eigen::Ref<const Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> & direction,
  const Eigen::Ref<const Eigen::Array<bool, Eigen::Dynamic, 1>> &                                  collapseDimensions,
  const ExtractImageFilterEnums::DirectionCollapseStrategy                                         strategy,
  Eigen::Ref<Eigen::Matrix<TValue, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>>               result)
{
  using StrategyEnum = ExtractImageFilterEnums::DirectionCollapseStrategy;
  using ResultMatrix = Eigen::Matrix<TValue, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;

  const Eigen::Index inputDimension = direction.rows();
  if (direction.cols() != inputDimension || collapseDimensions.size() != inputDimension)
  {
    itkGenericExceptionMacro("The direction matrix must be square (" << direction.rows() << 'x' << direction.cols()
                                                                     << ") with one collapse flag per axis ("
                                                                     << collapseDimensions.size() << ").");
  }

  std::vector<Eigen::Index> retained;
  for (Eigen::Index i = 0; i < inputDimension; ++i)
  {
    if (!collapseDimensions[i])
    {
      retained.push_back(i);
    }
  }
  const auto outputDimension = result.rows();
  if (static_cast<Eigen::Index>(retained.size()) != outputDimension)
  {
    itkGenericExceptionMacro("The number of retained dimensions ("
                             << retained.size() << ") does not match the output dimension (" << outputDimension
                             << ").");
  }

  // Test the converted matrix: a block invertible in double can be singular in float.
  // Scalar indexing: slicing by index vectors requires Eigen 3.4, ITK accepts 3.3.
  const ResultMatrix collapsed = [&direction, &retained, outputDimension] {
    ResultMatrix block(outputDimension, outputDimension);
    for (Eigen::Index row = 0; row < outputDimension; ++row)
    {
      for (Eigen::Index col = 0; col < outputDimension; ++col)
      {
        block(row, col) = static_cast<TValue>(direction(retained[row], retained[col]));
      }
    }
    return block;
  }();
  const auto isSingular = [&collapsed, outputDimension] {
    return bridge::Math::detail::DynamicDeterminantEigen<TValue>(
             collapsed.data(), static_cast<unsigned int>(outputDimension)) == TValue{ 0 };
  };

  switch (strategy)
  {
    case StrategyEnum::DIRECTIONCOLLAPSETOIDENTITY:
      result.setIdentity();
      return;
    case StrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX:
      if (isSingular())
      {
        itkGenericExceptionMacro("Invalid submatrix extracted for collapsed direction.");
      }
      result = collapsed;
      return;
    case StrategyEnum::DIRECTIONCOLLAPSETOGUESS:
      if (isSingular())
      {
        result.setIdentity();
      }
      else
      {
        result = collapsed;
      }
      return;
    case StrategyEnum::DIRECTIONCOLLAPSETOUNKOWN:
    default:
      itkGenericExceptionMacro(
        "It is required that the strategy for collapsing the direction matrix be explicitly specified. "
        "Set with either myfilter->SetDirectionCollapseToIdentity() or "
        "myfilter->SetDirectionCollapseToSubmatrix().");
  }
}
} // namespace

void
DirectionAlgorithm::CollapseDirectionInternal(const Eigen::Ref<const RowMajorMatrix<double>> & direction,
                                              const Eigen::Ref<const FlagArray> &              collapseDimensions,
                                              CollapseStrategyEnum                             strategy,
                                              Eigen::Ref<RowMajorMatrix<double>>               result)
{
  CollapseDirectionImpl<double>(direction, collapseDimensions, strategy, result);
}

void
DirectionAlgorithm::CollapseDirectionInternal(const Eigen::Ref<const RowMajorMatrix<double>> & direction,
                                              const Eigen::Ref<const FlagArray> &              collapseDimensions,
                                              CollapseStrategyEnum                             strategy,
                                              Eigen::Ref<RowMajorMatrix<float>>                result)
{
  CollapseDirectionImpl<float>(direction, collapseDimensions, strategy, result);
}
} // end namespace itk
