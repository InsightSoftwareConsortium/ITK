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
#ifndef itkDirectionAlgorithm_h
#define itkDirectionAlgorithm_h

#include "itkFixedArray.h"
#include "itkMatrix.h"
#include "itkVariableSizeMatrix.h"
#include "ITKCommonExport.h"
#include "itk_eigen.h"
#include ITK_EIGEN(Core)

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

/** \class DirectionAlgorithm
 *
 * \brief Functions that operate on image direction matrices.
 *
 * \ingroup ITKCommon
 */
struct ITKCommon_EXPORT DirectionAlgorithm
{
  using CollapseStrategyEnum = ExtractImageFilterEnums::DirectionCollapseStrategy;

  /** Compute the direction of an image whose axes have been collapsed.
   *
   * Rows and columns of \a direction whose \a collapseDimensions entry is true
   * are removed, and the remaining sub-matrix is resolved by \a strategy:
   * DIRECTIONCOLLAPSETOIDENTITY returns identity, DIRECTIONCOLLAPSETOSUBMATRIX
   * returns the sub-matrix and throws when it is singular, and
   * DIRECTIONCOLLAPSETOGUESS returns the sub-matrix or identity when singular.
   * DIRECTIONCOLLAPSETOUNKOWN throws. Singularity is tested after conversion to
   * the result's element type, which must be float or double. The number of
   * retained axes must equal VOutputDimension.
   *
   * This overload takes a direction whose dimension is known at compile time;
   * the result has the same element type. */
  template <unsigned int VOutputDimension, typename TValue, unsigned int VInputDimension>
  static Matrix<TValue, VOutputDimension, VOutputDimension>
  CollapseDirection(const Matrix<TValue, VInputDimension, VInputDimension> & direction,
                    const FixedArray<bool, VInputDimension> &                collapseDimensions,
                    const CollapseStrategyEnum                               strategy)
  {
    const Eigen::Map<const RowMajorMatrix<TValue>> directionView(
      direction.GetVnlMatrix().data_block(), VInputDimension, VInputDimension);
    const Eigen::Map<const FlagArray>                  collapseView(collapseDimensions.data(), VInputDimension);
    Matrix<TValue, VOutputDimension, VOutputDimension> result;
    Eigen::Map<RowMajorMatrix<TValue>>                 resultView(
      result.GetVnlMatrix().data_block(), VOutputDimension, VOutputDimension);
    CollapseDirectionInternal(directionView.template cast<double>(), collapseView, strategy, resultView);
    return result;
  }

  /** Overload for a direction whose dimension is known only at run time, such
   * as an ImageIOBase direction; the result has element type TValue. */
  template <unsigned int VOutputDimension, typename TValue = double>
  static Matrix<TValue, VOutputDimension, VOutputDimension>
  CollapseDirection(const VariableSizeMatrix<double> & direction,
                    const std::vector<bool> &          collapseDimensions,
                    const CollapseStrategyEnum         strategy)
  {
    const auto &                                   vnlDirection = direction.GetVnlMatrix();
    const Eigen::Map<const RowMajorMatrix<double>> directionView(
      vnlDirection.data_block(), vnlDirection.rows(), vnlDirection.cols());
    FlagArray collapseFlags(collapseDimensions.size());
    for (size_t i = 0; i < collapseDimensions.size(); ++i)
    {
      collapseFlags[i] = collapseDimensions[i];
    }
    Matrix<TValue, VOutputDimension, VOutputDimension> result;
    Eigen::Map<RowMajorMatrix<TValue>>                 resultView(
      result.GetVnlMatrix().data_block(), VOutputDimension, VOutputDimension);
    CollapseDirectionInternal(directionView, collapseFlags, strategy, resultView);
    return result;
  }

private:
  template <typename TValue>
  using RowMajorMatrix = Eigen::Matrix<TValue, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;
  using FlagArray = Eigen::Array<bool, Eigen::Dynamic, 1>;

  static void
  CollapseDirectionInternal(const Eigen::Ref<const RowMajorMatrix<double>> & direction,
                            const Eigen::Ref<const FlagArray> &              collapseDimensions,
                            CollapseStrategyEnum                             strategy,
                            Eigen::Ref<RowMajorMatrix<double>>               result);
  static void
  CollapseDirectionInternal(const Eigen::Ref<const RowMajorMatrix<double>> & direction,
                            const Eigen::Ref<const FlagArray> &              collapseDimensions,
                            CollapseStrategyEnum                             strategy,
                            Eigen::Ref<RowMajorMatrix<float>>                result);
};

} // namespace itk

#endif
