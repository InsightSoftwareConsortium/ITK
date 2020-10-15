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
#ifndef itkBSplineInterpolationWeightFunction_h
#define itkBSplineInterpolationWeightFunction_h

#include "itkFunctionBase.h"
#include "itkContinuousIndex.h"
#include "itkBSplineKernelFunction.h"
#include "itkArray.h"
#include "itkArray2D.h"

namespace itk
{
/** \class BSplineInterpolationWeightFunction
 * \brief Returns the weights over the support region used for B-spline
 * interpolation/reconstruction.
 *
 * Computes/evaluate the B-spline interpolation weights over the
 * support region of the B-spline.
 *
 * This class is templated over the coordinate representation type,
 * the space dimension and the spline order.
 *
 * \sa Point
 * \sa Index
 * \sa ContinuousIndex
 *
 * \ingroup Functions ImageInterpolators
 * \ingroup ITKCommon
 */
template <typename TCoordRep = float, unsigned int VSpaceDimension = 2, unsigned int VSplineOrder = 3>
class ITK_TEMPLATE_EXPORT BSplineInterpolationWeightFunction
  : public FunctionBase<ContinuousIndex<TCoordRep, VSpaceDimension>, Array<double>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineInterpolationWeightFunction);

  /** Standard class type aliases. */
  using Self = BSplineInterpolationWeightFunction;
  using Superclass = FunctionBase<ContinuousIndex<TCoordRep, VSpaceDimension>, Array<double>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineInterpolationWeightFunction, FunctionBase);

  /** Space dimension. */
  static constexpr unsigned int SpaceDimension = VSpaceDimension;

  /** Spline order. */
  static constexpr unsigned int SplineOrder = VSplineOrder;

  /** OutputType type alias support. */
  using WeightsType = Array<double>;

  /** Index and size type alias support. */
  using IndexType = Index<VSpaceDimension>;
  using SizeType = Size<VSpaceDimension>;

  /** ContinuousIndex type alias support. */
  using ContinuousIndexType = ContinuousIndex<TCoordRep, VSpaceDimension>;

  /** Evaluate the weights at specified ContinuousIndex position.
   * Subclasses must provide this method. */
  WeightsType
  Evaluate(const ContinuousIndexType & index) const override;

  /** Evaluate the weights at specified ContinuousIndex position.
   * The weights are returned in the user specified container.
   * This function assume that weights can hold
   * (SplineOrder + 1)^(SpaceDimension) elements. For efficiency,
   * no size checking is done.
   * On return, startIndex contains the start index of the
   * support region over which the weights are defined.
   */
  virtual void
  Evaluate(const ContinuousIndexType & index, WeightsType & weights, IndexType & startIndex) const;

  /** Get support region size. */
  itkGetConstMacro(SupportSize, SizeType);

  /** Get number of weights. */
  itkGetConstMacro(NumberOfWeights, unsigned int);

protected:
  BSplineInterpolationWeightFunction();
  ~BSplineInterpolationWeightFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Number of weights. */
  unsigned int m_NumberOfWeights;

  /** Size of support region. */
  SizeType m_SupportSize;

  /** Lookup table type. */
  using TableType = Array2D<unsigned int>;

  /** Table mapping linear offset to indices. */
  TableType m_OffsetToIndexTable;

  /** Interpolation kernel type. */
  using KernelType = BSplineKernelFunction<Self::SplineOrder>;

  /** Interpolation kernel. */
  typename KernelType::Pointer m_Kernel;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineInterpolationWeightFunction.hxx"
#endif

#endif
