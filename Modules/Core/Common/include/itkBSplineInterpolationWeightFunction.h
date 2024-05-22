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
#ifndef itkBSplineInterpolationWeightFunction_h
#define itkBSplineInterpolationWeightFunction_h

#include "itkFunctionBase.h"
#include "itkContinuousIndex.h"
#include "itkArray.h"
#include "itkArray2D.h"
#include "itkIndexRange.h"
#include "itkMath.h"

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
  : public FunctionBase<ContinuousIndex<TCoordRep, VSpaceDimension>,
                        FixedArray<double, Math::UnsignedPower(VSplineOrder + 1, VSpaceDimension)>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BSplineInterpolationWeightFunction);

  /** Standard class type aliases. */
  using Self = BSplineInterpolationWeightFunction;
  using Superclass = FunctionBase<ContinuousIndex<TCoordRep, VSpaceDimension>,
                                  FixedArray<double, Math::UnsignedPower(VSplineOrder + 1, VSpaceDimension)>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(BSplineInterpolationWeightFunction);

  /** Space dimension. */
  static constexpr unsigned int SpaceDimension = VSpaceDimension;

  /** Spline order. */
  static constexpr unsigned int SplineOrder = VSplineOrder;

  /** OutputType type alias support. */
  using WeightsType = typename Superclass::OutputType;

  /** Number of weights. */
  static constexpr unsigned int NumberOfWeights{ WeightsType::Length };

  /** Index and size type alias support. */
  using IndexType = Index<VSpaceDimension>;
  using SizeType = Size<VSpaceDimension>;

  /** ContinuousIndex type alias support. */
  using ContinuousIndexType = ContinuousIndex<TCoordRep, VSpaceDimension>;

  /** The support region size: a hypercube of length SplineOrder + 1 */
  static constexpr SizeType SupportSize{ SizeType::Filled(VSplineOrder + 1) };

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

#if !defined(ITK_LEGACY_REMOVE)
  /** Get support region size. */
  itkLegacyMacro(SizeType GetSupportSize() const) { return Self::SupportSize; };

  /** Get number of weights. */
  itkLegacyMacro(unsigned int GetNumberOfWeights() const) { return Self::NumberOfWeights; }
#endif

protected:
  BSplineInterpolationWeightFunction() = default;
  ~BSplineInterpolationWeightFunction() override = default;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBSplineInterpolationWeightFunction.hxx"
#endif

#endif
