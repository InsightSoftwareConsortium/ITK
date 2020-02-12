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
#ifndef itkLabeledPointSetToPointSetMetricv4_h
#define itkLabeledPointSetToPointSetMetricv4_h

#include "itkPointSetToPointSetMetricv4.h"

#include <vector>

namespace itk
{
/** \class LabeledPointSetToPointSetMetricv4
 * \brief Computes the distance metric and gradient between two
 * labeled point sets.
 *
 * This class is generic in that it takes an unlabeled point set metric,
 * such as one of the following options
 *
 * \li \c EuclideanDistancePointSetToPointSetMetricv4 (default)
 * \li \c ExpectationBasedPointSetToPointSetMetricv4
 * \li \c JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4
 *
 * where it is assumed that the point set \c PixelType associated with
 * each point is a label (i.e. \c LabelEnum).  This class is used to find
 * the total metric and total derivative based on the matching between the
 * separate fixed and moving labeled point sets.  This class first determines
 * the common label set between the fixed and moving point sets.  For each
 * common label, the specified unlabeled point set metric is cloned and the
 * fixed and moving point subset associated with that common label is specified
 * as the fixed and moving point set input.  Functionality in the base class
 * (\c PointSetToPointSetMetric) is used to accumulated the value and gradient.
 *
 * \author Nick Tustison
 *
 * \ingroup ITKMetricsv4
 */
template <typename TFixedPointSet,
          typename TMovingPointSet = TFixedPointSet,
          class TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT LabeledPointSetToPointSetMetricv4
  : public PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabeledPointSetToPointSetMetricv4);

  /** Standard class type aliases. */
  using Self = LabeledPointSetToPointSetMetricv4;
  using Superclass = PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LabeledPointSetToPointSetMetricv4, PointSetToPointSetMetricv4);

  using FixedPointSetType = TFixedPointSet;
  using FixedPointSetPointer = typename FixedPointSetType::Pointer;
  using MovingPointSetType = TMovingPointSet;
  using MovingPointSetPointer = typename MovingPointSetType::Pointer;

  /** Types transferred from the base class */
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using LocalDerivativeType = typename Superclass::LocalDerivativeType;
  using PointType = typename Superclass::PointType;
  using PointIdentifier = typename Superclass::PointIdentifier;

  using LabelType = typename Superclass::PixelType;
  using LabelSetType = std::vector<LabelType>;

  using PointSetMetricType = Superclass;
  using PointSetMetricPointer = typename PointSetMetricType::Pointer;

  /**
   * Initialize the metric by making sure that all the components
   *  are present and plugged together correctly.
   */
  void
  Initialize() override;

  /**
   * Calculates the local metric value for a single point.  The label type
   * is used to segregate the computation.
   */
  MeasureType
  GetLocalNeighborhoodValue(const PointType &, const LabelType &) const override;

  /**
   * Calculates the local value and derivative for a single point. The label type
   * is used to segregate the computation.
   */
  void
  GetLocalNeighborhoodValueAndDerivative(const PointType &,
                                         MeasureType &,
                                         LocalDerivativeType &,
                                         const LabelType &) const override;

  /**
   * Set/get the specific unlabeled point set metric type.  Default is
   * the \c EuclideanDistancePointSetToPointSetMetricv4.
   */
  itkSetObjectMacro(PointSetMetric, PointSetMetricType);
  itkGetModifiableObjectMacro(PointSetMetric, PointSetMetricType);

  /**
   * Ensure label type is an integer type
   */
  itkConceptMacro(LabelTypeIsInteger, (Concept::IsInteger<LabelType>));

protected:
  LabeledPointSetToPointSetMetricv4();
  ~LabeledPointSetToPointSetMetricv4() override = default;

  /** PrintSelf function */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /**
   * Private function to find the common label set for the moving
   * and fixed point sets.
   */
  void
  DetermineCommonPointSetLabels();

  /**
   * Private function to create a fixed point set from the input fixed point
   * set with a specific label
   */
  FixedPointSetPointer
  GetLabeledFixedPointSet(const LabelType) const;

  /**
   * Private function to create a moving point set from the input moving point
   * set with a specific label
   */
  MovingPointSetPointer
  GetLabeledMovingPointSet(const LabelType) const;

  PointSetMetricPointer              m_PointSetMetric;
  std::vector<PointSetMetricPointer> m_PointSetMetricClones;

  LabelSetType m_FixedPointSetLabels;
  LabelSetType m_MovingPointSetLabels;
  LabelSetType m_CommonPointSetLabels;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabeledPointSetToPointSetMetricv4.hxx"
#endif

#endif
