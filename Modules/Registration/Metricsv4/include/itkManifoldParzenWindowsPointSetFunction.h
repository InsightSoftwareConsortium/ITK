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
#ifndef itkManifoldParzenWindowsPointSetFunction_h
#define itkManifoldParzenWindowsPointSetFunction_h

#include "itkPointSetFunction.h"

#include "itkGaussianMembershipFunction.h"
#include "itkMatrix.h"
#include "itkPointSet.h"
#include "itkPointsLocator.h"
#include "itkVector.h"
#include "itkMultiThreaderBase.h"

#include <vector>

namespace itk
{

/** \class ManifoldParzenWindowsPointSetFunction
 * \brief Point set function based on n-dimensional parzen windowing.
 *
 * This class allows evaluating a function derived from a point set
 * by creating a continuous distribution using manifold parzen windowing.
 * Each point is associated with a Gaussian and local shape can
 * be encoded in the covariance matrix.
 *
 * \ingroup ITKMetricsv4
 */
template <typename TPointSet, typename TOutput = double, typename TCoordRep = double>
class ITK_TEMPLATE_EXPORT ManifoldParzenWindowsPointSetFunction : public PointSetFunction<TPointSet, TOutput, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ManifoldParzenWindowsPointSetFunction);

  using Self = ManifoldParzenWindowsPointSetFunction;
  using Superclass = PointSetFunction<TPointSet, TOutput, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Extract dimension from output image. */
  static constexpr unsigned int PointDimension = TPointSet::PointDimension;

  using InputPointSetType = typename Superclass::InputPointSetType;
  using InputPointType = typename Superclass::InputPointType;

  /** Point set type alias support */
  using PointSetType = TPointSet;
  using PointType = typename PointSetType::PointType;
  using PointsContainer = typename PointSetType::PointsContainer;
  using PointIdentifier = typename PointsContainer::ElementIdentifier;

  /** Other type alias */
  using RealType = TOutput;
  using OutputType = TOutput;
  using CoordRepType = TCoordRep;

  /** Typedef for points locator class to speed up finding neighboring points */
  using PointsLocatorType = PointsLocator<PointsContainer>;
  using NeighborsIdentifierType = typename PointsLocatorType::NeighborsIdentifierType;

  using GaussianType = typename Statistics::GaussianMembershipFunction<PointType>;
  using GaussianPointer = typename GaussianType::Pointer;
  using GaussianConstPointer = typename GaussianType::ConstPointer;
  using GaussianContainerType = std::vector<GaussianPointer>;
  using CovarianceMatrixType = typename GaussianType::CovarianceMatrixType;

  /** Helper functions */

  /**
   * Set the covariance K neighborhood.  For a given point the closest K
   * points are used to construct the corresponding covariance reflecting
   * the local point set structure.  Default = 5.
   */
  itkSetMacro(CovarianceKNeighborhood, unsigned int);

  /** Get the covariance k neighborhood size.  Default = 5.*/
  itkGetConstMacro(CovarianceKNeighborhood, unsigned int);

  /**
   * Set the evaluation K neighborhood.  To evaluate the the manifold parzen
   * windows function, one could sum the value contributed by each Gaussian or
   * to speed calculation, we could sum the value contributed by the nearest
   * K Gaussians.  Default = 50.
   */
  itkSetMacro(EvaluationKNeighborhood, unsigned int);

  /** Get the evaluation K neighborhood.  Default = 50.*/
  itkGetConstMacro(EvaluationKNeighborhood, unsigned int);

  /**
   * Set the regularization sigma.  To avoid singular covariance matrices,
   * a regularization sigma value is added to the diagonal.  Default = 1.0.
   */
  itkSetMacro(RegularizationSigma, RealType);

  /** Get the regularization sigma.  Default = 1.0. */
  itkGetConstMacro(RegularizationSigma, RealType);

  /**
   * Set the kernel sigma.  In constructing the covariance from k neighbors,
   * a Gaussian is used to weight more strongly the closest neighbors.  This
   * defines that weighting Gaussian.  Default = 1.0.
   */
  itkSetMacro(KernelSigma, RealType);

  /** Get the kernel sigma.  Default = 1.0. */
  itkGetConstMacro(KernelSigma, RealType);

  /**
   * Normalize covariance by the sum of the weights of the nearest neighbors.
   * Default = true.
   */
  itkSetMacro(Normalize, bool);

  /**
   * Normalize covariance by the sum of the weights of the nearest neighbors.
   * Default = true.
   */
  itkGetConstMacro(Normalize, bool);

  /**
   * Normalize covariance by the sum of the weights of the nearest neighbors.
   * Default = true.
   */
  itkBooleanMacro(Normalize);

  /**
   * Construct covariances using the local neighborhood point set structure.
   * Otherwise, the Gaussian for each point is characterized by the value
   * of m_RegularizationSigma.  Default = true.
   */
  itkSetMacro(UseAnisotropicCovariances, bool);

  /**
   * Construct covariances using the local neighborhood point set structure.
   * Otherwise, the Gaussian for each point is characterized by the value
   * of m_RegularizationSigma.  Default = true.
   */
  itkGetConstMacro(UseAnisotropicCovariances, bool);

  /**
   * Construct covariances using the local neighborhood point set structure.
   * Otherwise, the Gaussian for each point is characterized by the value
   * of m_RegularizationSigma.  Default = true.
   */
  itkBooleanMacro(UseAnisotropicCovariances);

  /** Set the input point set */
  void
  SetInputPointSet(const InputPointSetType *) override;

  /** Evaluate function value at specified point */
  TOutput
  Evaluate(const InputPointType &) const override;

  /** Get Gaussian corresponding to a specific point */
  GaussianConstPointer GetGaussian(PointIdentifier) const;

  /** Get the points locator describing the point set neighborhood */
  itkGetModifiableObjectMacro(PointsLocator, PointsLocatorType);

protected:
  ManifoldParzenWindowsPointSetFunction();
  ~ManifoldParzenWindowsPointSetFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData();

private:
  typename PointsLocatorType::Pointer m_PointsLocator;

  unsigned int m_CovarianceKNeighborhood{ 5 };
  unsigned int m_EvaluationKNeighborhood{ 50 };
  RealType     m_RegularizationSigma;
  RealType     m_KernelSigma;

  GaussianContainerType m_Gaussians;
  bool                  m_Normalize{ true };
  bool                  m_UseAnisotropicCovariances{ true };

  MultiThreaderBase::Pointer m_MultiThreader;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkManifoldParzenWindowsPointSetFunction.hxx"
#endif

#endif
