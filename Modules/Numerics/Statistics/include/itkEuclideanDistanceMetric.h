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
#ifndef itkEuclideanDistanceMetric_h
#define itkEuclideanDistanceMetric_h

#include "itkDistanceMetric.h"

namespace itk
{
namespace Statistics
{
/**
 *\class EuclideanDistanceMetric
 * \brief Euclidean distance function.
 *
 *
 * \ingroup ITKStatistics
 */
template <typename TVector>
class ITK_TEMPLATE_EXPORT EuclideanDistanceMetric : public DistanceMetric<TVector>
{
public:
  /** Standard "Self" type alias. */
  using Self = EuclideanDistanceMetric;
  using Superclass = DistanceMetric<TVector>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using MeasurementVectorType = typename Superclass::MeasurementVectorType;
  using ValueType = typename MeasurementVectorTraitsTypes<MeasurementVectorType>::ValueType;
  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;

  using OriginType = typename Superclass::OriginType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(EuclideanDistanceMetric, DistanceMetric);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Gets the distance between the origin and x */
  double
  Evaluate(const MeasurementVectorType & x) const override;

  /** Gets the distance between x1 and x2 */
  double
  Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const override;

  /** Gets the coordinate distance between a and b. NOTE: a and b
   * should be type of component. This method is used by
   * KdTreeKMeans estimators. When the estimator is refactored,
   * this method should be removed. */
  double
  Evaluate(const ValueType & a, const ValueType & b) const;

protected:
  EuclideanDistanceMetric() = default;
  ~EuclideanDistanceMetric() override = default;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkEuclideanDistanceMetric.hxx"
#endif

#endif
