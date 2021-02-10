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
#ifndef itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4_h
#define itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4_h

#include "itkPointSetToPointSetMetricv4.h"

#include "itkManifoldParzenWindowsPointSetFunction.h"

namespace itk
{

/** \class JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4
 *
 * \brief Implementation of the Jensen Havrda Charvat Tsallis Point Set metric.
 *
 * Given a specified transform and direction, this class calculates the
 * value and derivative between a "fixed" and "moving" point set pair using
 * the Havrda-Charvat-Tsallis entropy family, a generalization of the well-known
 * Shannon entropy, and the Jensen divergence.  Another way to look at the
 * family of information-theoretic measures is that the points are used to
 * construct the corresponding probably density functions.
 *
 * In addition, we allow the user to invoke a manifold parzen windowing of the
 * data.  Instead of an isotropic Gaussian being associated with each point,
 * we can actually calculate the covariance matrix for each point such that it
 * reflects the locate point set structure.
 *
 * To speed up the metric calculation, we use ITK's K-d tree to query the
 * metric value only for a given neighborhood.  Considering that probably only
 * a small subset of points is needed to get a good approximation of the
 * metric value for a single point, this is probably warranted.  So what we do
 * is transform each point (with the specified transform) and construct the
 * k-d tree from the transformed points.
 *
 * Contributed by Nicholas J. Tustison, James C. Gee in the Insight Journal
 * paper: https://www.insight-journal.org/browse/publication/317
 *
 * \note The original work reported in Tustison et al. 2011 optionally employed
 * a regularization term to prevent the moving point set(s) from coalescing
 * to a single point location. However, within the registration framework,
 * this term is of limited utility as such regularization is dictated by the
 * transform and any explicit regularization terms. Also note that the
 * published work applies to multiple points sets each of which could
 * be considered "moving" but this is also not applicable for this particular
 * implementation.
 *
 * \par REFERENCE
 *
 * N.J. Tustison, S. P. Awate, G. Song, T. S. Cook, and J. C. Gee.
 * "Point set registration using Havrda-Charvat-Tsallis entropy measures"
 * IEEE Transactions on Medical Imaging, 30(2):451-60, 2011.
 * \ingroup ITKMetricsv4
 */

template <typename TPointSet, class TInternalComputationValueType = double>
class ITK_TEMPLATE_EXPORT JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4
  : public PointSetToPointSetMetricv4<TPointSet, TPointSet, TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4);

  /** Standard class type aliases. */
  using Self = JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4;
  using Superclass = PointSetToPointSetMetricv4<TPointSet, TPointSet, TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4, PointSetToPointSetMetricv4);

  using PointSetType = TPointSet;
  using PointsContainer = typename PointSetType::PointsContainer;
  using PointsContainerConstIterator = typename PointsContainer::ConstIterator;

  static constexpr unsigned int PointDimension = TPointSet::PointDimension;

  /** Types transferred from the base class */
  using MeasureType = typename Superclass::MeasureType;
  using DerivativeType = typename Superclass::DerivativeType;
  using DerivativeValueType = typename Superclass::DerivativeValueType;
  using LocalDerivativeType = typename Superclass::LocalDerivativeType;
  using PointType = typename Superclass::PointType;
  using PixelType = typename Superclass::PixelType;
  using CoordRepType = typename Superclass::CoordRepType;
  using PointIdentifier = typename Superclass::PointIdentifier;
  using NeighborsIdentifierType = typename Superclass::NeighborsIdentifierType;
  using NumberOfParametersType = typename Superclass::NumberOfParametersType;

  using JacobianType = typename Superclass::JacobianType;
  using FixedTransformJacobianType = typename Superclass::FixedTransformJacobianType;
  using MovingTransformJacobianType = typename Superclass::MovingTransformJacobianType;

  using RealType = MeasureType;

  /**
   * Other typedefs
   */
  using DensityFunctionType = ManifoldParzenWindowsPointSetFunction<PointSetType, RealType>;
  using GaussianType = typename DensityFunctionType::GaussianType;
  using DensityFunctionPointer = typename DensityFunctionType::Pointer;

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  void
  Initialize() override;

  /**
   * Set the alpha parameter used to tune the point-set metric from
   * a maximum-likelihood measure (alpha = 1) to the more robust L2
   * solution (alpha = 2).  Typically, "robustness" is associated with
   * performance in the presence of uniform noise but in most applications
   * the noise will be highly correlated with the point sets, therefore
   * an alpha value close to 1, in general, provides better performance.
   * Only values between 1 and 2 are convex.
   */
  itkSetClampMacro(Alpha, RealType, 1.0, 2.0);

  /**
   * Get the alpha parameter used to tune the point-set metric.
   */
  itkGetConstMacro(Alpha, RealType);

  /**
   * Each point is associated with a Gaussian characterized by m_PointSetSigma
   * which provides a sense of scale for determining the similarity between two
   * point sets.  Default = 1.0.
   */
  itkSetMacro(PointSetSigma, RealType);

  /** Get the point set sigma function */
  itkGetConstMacro(PointSetSigma, RealType);

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

  /**
   * Set whether or not anisotropic covariances are determined for each
   * Gaussian.  Default = false.
   */
  itkSetMacro(UseAnisotropicCovariances, bool);

  /**
   * Get whether or not anisotropic covariances are determined for each
   * Gaussian.  Default = false.
   */
  itkGetConstMacro(UseAnisotropicCovariances, bool);

  /**
   * Get/set whether or not anisotropic covariances are determined for each
   * Gaussian.  Default = false.
   */
  itkBooleanMacro(UseAnisotropicCovariances);

  /**
   * Set the size of the covariance neighborhood used to construct the
   * anisotropic covariances.  Only relevant if m_UseAnisotropicCovariances =
   * true.  Default = 5.
   */
  itkSetMacro(CovarianceKNeighborhood, unsigned int);

  /**
   * Get the size of the covariance neighborhood used to construct the
   * anisotropic covariances.  Only relevant if m_UseAnisotropicCovariances =
   * true.  Default = 5.
   */
  itkGetConstMacro(CovarianceKNeighborhood, unsigned int);

  /**
   * Set the size of the noise kernel used to construct each covariance image.
   * To avoid the case where the local point set structure would result in a
   * degenerate covariance matrix, a small amount of noise is added along the
   * diagonal represented by this variable.  Only relevant if
   * m_UseAnisotropicCovariances = true.  Default = 10.0.
   */
  itkSetMacro(KernelSigma, RealType);

  /** Get the noise kernel sigma for the anisotropic covariances. */
  itkGetConstMacro(KernelSigma, RealType);

  MeasureType
  GetLocalNeighborhoodValue(const PointType & point, const PixelType & pixel = 0) const override;

  void
  GetLocalNeighborhoodValueAndDerivative(const PointType &,
                                         MeasureType &,
                                         LocalDerivativeType &,
                                         const PixelType & pixel = 0) const override;

  /** Clone method will clone the existing instance of this type,
   *  including its internal member variables. */
  typename LightObject::Pointer
  InternalClone() const override;

protected:
  JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4();
  ~JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4() override = default;

  void
  ComputeValueAndDerivative(const PointType &     samplePoint,
                            MeasureType &         value,
                            LocalDerivativeType & derivativeReturn,
                            bool                  calcValue,
                            bool                  calcDerivative) const;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  bool
  RequiresFixedPointsLocator() const override
  {
    return false;
  }

  bool
  RequiresMovingPointsLocator() const override
  {
    return false;
  }

private:
  DensityFunctionPointer m_MovingDensityFunction;

  bool m_UseAnisotropicCovariances{ false };

  RealType     m_PointSetSigma;
  RealType     m_KernelSigma;
  unsigned int m_CovarianceKNeighborhood;
  unsigned int m_EvaluationKNeighborhood;

  RealType m_Alpha;

  /** Precomputed cached values */
  mutable RealType m_TotalNumberOfPoints;
  mutable RealType m_Prefactor0;
  mutable RealType m_Prefactor1;
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4.hxx"
#endif

#endif
