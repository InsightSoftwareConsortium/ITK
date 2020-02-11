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
#ifndef itkExpectationBasedPointSetToPointSetMetricv4_h
#define itkExpectationBasedPointSetToPointSetMetricv4_h

#include "itkPointSetToPointSetMetricv4.h"
#include "itkPointSet.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class ExpectationBasedPointSetToPointSetMetricv4
 * \brief Computes an expectation-based metric between two point sets.
 *
 * This information-theoretic point set measure models each point set
 * as a sum of Gaussians.  To speed up computation, evaluation of the local
 * value/derivative is done in a user-specified neighborhood using the k-d
 * tree constructed in the superclass.
 *
 *  Reference:
 *    Pluta J, Avants BB, Glynn S, Awate S, Gee JC, Detre JA,
 *    "Appearance and incomplete label matching for diffeomorphic template
 *     "based hippocampus segmentation", Hippocampus, 2009 Jun; 19(6):565-71.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TFixedPointSet,
          typename TMovingPointSet = TFixedPointSet,
          class TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT ExpectationBasedPointSetToPointSetMetricv4
  : public PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExpectationBasedPointSetToPointSetMetricv4);

  /** Standard class type aliases. */
  using Self = ExpectationBasedPointSetToPointSetMetricv4;
  using Superclass = PointSetToPointSetMetricv4<TFixedPointSet, TMovingPointSet, TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExpectationBasedPointSetToPointSetMetricv4, PointSetToPointSetMetricv4);

  /** Types transferred from the base class */
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using LocalDerivativeType = typename Superclass::LocalDerivativeType;
  using PointType = typename Superclass::PointType;
  using PixelType = typename Superclass::PixelType;
  using CoordRepType = typename Superclass::CoordRepType;
  using PointIdentifier = typename Superclass::PointIdentifier;
  using NeighborsIdentifierType = typename Superclass::NeighborsIdentifierType;

  /**
   * Calculates the local metric value for a single point.
   */
  MeasureType
  GetLocalNeighborhoodValue(const PointType &, const PixelType & pixel = 0) const override;

  /**
   * Calculates the local value and derivative for a single point.
   */
  void
  GetLocalNeighborhoodValueAndDerivative(const PointType &,
                                         MeasureType &,
                                         LocalDerivativeType &,
                                         const PixelType & pixel = 0) const override;

  /**
   * Each point is associated with a Gaussian characterized by m_PointSetSigma
   * which provides a sense of scale for determining the similarity between two
   * point sets.  Default = 1.0.
   */
  itkSetMacro(PointSetSigma, CoordRepType);

  /** Get the point set sigma function */
  itkGetConstMacro(PointSetSigma, CoordRepType);

  /**
   * Set the neighborhood size used to evaluate the measurement at each
   * point.  Default = 50.
   */
  itkSetMacro(EvaluationKNeighborhood, unsigned int);

  /**
   * Get the neighborhood size used to evaluate the measurement at each
   * point.  Default = 50.
   */
  itkGetConstMacro(EvaluationKNeighborhood, unsigned int);

  void
  Initialize() override;

  /** Clone method will clone the existing instance of this type,
   *  including its internal member variables. */
  typename LightObject::Pointer
  InternalClone() const override;

protected:
  ExpectationBasedPointSetToPointSetMetricv4();
  ~ExpectationBasedPointSetToPointSetMetricv4() override = default;

  bool
  RequiresFixedPointsLocator() const override
  {
    return false;
  }

  /** PrintSelf function */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  using VectorType = typename PointType::VectorType;
  using NeighborsIterator = typename NeighborsIdentifierType::const_iterator;

  CoordRepType m_PointSetSigma;
  MeasureType  m_PreFactor;
  MeasureType  m_Denominator;
  unsigned int m_EvaluationKNeighborhood{ 50 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkExpectationBasedPointSetToPointSetMetricv4.hxx"
#endif

#endif
