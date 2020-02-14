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
#ifndef itkGaussianMixtureModelComponent_h
#define itkGaussianMixtureModelComponent_h

#include "itkMixtureModelComponentBase.h"
#include "itkGaussianMembershipFunction.h"
#include "itkWeightedMeanSampleFilter.h"
#include "itkWeightedCovarianceSampleFilter.h"

namespace itk
{
namespace Statistics
{
/**
 *\class GaussianMixtureModelComponent
 * \brief is a component (derived from MixtureModelComponentBase) for
 * Gaussian class. This class is used in
 * ExpectationMaximizationMixtureModelEstimator.
 *
 * On every iteration of EM estimation, this class's GenerateData
 * method is called to compute the new distribution parameters.
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained at run time from the
 * sample set as input. Please use the function
 * GetMeasurementVectorSize() to get the length.
 *
 * \sa MixtureModelComponentBase, ExpectationMaximizationMixtureModelEstimator
 * \ingroup ITKStatistics
 */

template <typename TSample>
class ITK_TEMPLATE_EXPORT GaussianMixtureModelComponent : public MixtureModelComponentBase<TSample>
{
public:
  /**Standard class type aliases. */
  using Self = GaussianMixtureModelComponent;
  using Superclass = MixtureModelComponentBase<TSample>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**Standard Macros */
  itkTypeMacro(GaussianMixtureModelComponent, MixtureModelComponentBase);
  itkNewMacro(Self);

  /** Typedefs from the superclass */
  using MeasurementVectorType = typename Superclass::MeasurementVectorType;
  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;
  using MembershipFunctionType = typename Superclass::MembershipFunctionType;
  using WeightArrayType = typename Superclass::WeightArrayType;
  using ParametersType = typename Superclass::ParametersType;

  /** Type of the membership function. Gaussian density function */
  using NativeMembershipFunctionType = GaussianMembershipFunction<MeasurementVectorType>;

  /** Types of the mean and the covariance calculator that will update
   *  this component's distribution parameters */
  using MeanEstimatorType = WeightedMeanSampleFilter<TSample>;
  using CovarianceEstimatorType = WeightedCovarianceSampleFilter<TSample>;

  /** Type of the mean vector */
  using MeanVectorType = typename MeanEstimatorType::OutputType;

  /** Type of the covariance matrix */
  using CovarianceMatrixType = typename CovarianceEstimatorType::OutputType;

  /** Sets the input sample */
  void
  SetSample(const TSample * sample) override;

  /** Sets the component's distribution parameters. */
  void
  SetParameters(const ParametersType & parameters) override;

protected:
  GaussianMixtureModelComponent();
  ~GaussianMixtureModelComponent() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Returns the sum of squared changes in parameters between
   * iterations */
  double
  CalculateParametersChange();

  /** Computes the new distribution parameters */
  void
  GenerateData() override;

private:
  typename NativeMembershipFunctionType::Pointer m_GaussianMembershipFunction;

  typename MeanEstimatorType::MeasurementVectorType m_Mean;

  typename CovarianceEstimatorType::MatrixType m_Covariance;

  typename MeanEstimatorType::Pointer m_MeanEstimator;

  typename CovarianceEstimatorType::Pointer m_CovarianceEstimator;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianMixtureModelComponent.hxx"
#endif

#endif
