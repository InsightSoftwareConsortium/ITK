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
#ifndef itkGaussianMembershipFunction_h
#define itkGaussianMembershipFunction_h

#include "itkMatrix.h"
#include "itkMembershipFunctionBase.h"

namespace itk
{
namespace Statistics
{
/**
 *\class GaussianMembershipFunction
 * \brief GaussianMembershipFunction models class membership through a
 * multivariate Gaussian function.
 *
 * GaussianMembershipFunction is a subclass of MembershipFunctionBase
 * that models class membership (or likelihood) using a multivariate
 * Gaussian function. The mean and covariance structure of the
 * Gaussian are established using the methods SetMean() and
 * SetCovariance(). The mean is a vector-type that is the same
 * vector-type as the measurement vector but guaranteed to have a real
 * element type. For instance, if the measurement type is an
 * Vector<int,3>, then the mean is Vector<double,3>. If the
 * measurement type is a VariableLengthVector<float>, then the mean is
 * VariableLengthVector<double>. In contrast to this behavior, the
 * covariance is always a VariableSizeMatrix<double>.
 *
 * If the covariance is singular or nearly singular, the membership function
 * behaves somewhat like an impulse located at the mean. In this case,
 * we specify the covariance to be a diagonal matrix with large values
 * along the diagonal. This membership function, therefore,
 * will return small but differentiable values everywhere and increase
 * sharply near the mean.
 *
 * \ingroup ITKStatistics
 */

template <typename TMeasurementVector>
class ITK_TEMPLATE_EXPORT GaussianMembershipFunction : public MembershipFunctionBase<TMeasurementVector>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GaussianMembershipFunction);

  /** Standard class type aliases */
  using Self = GaussianMembershipFunction;
  using Superclass = MembershipFunctionBase<TMeasurementVector>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(GaussianMembershipFunction, MembershipFunction);
  itkNewMacro(Self);

  /** SmartPointer class for superclass */
  using MembershipFunctionPointer = typename Superclass::Pointer;

  /** Typedef alias for the measurement vectors */
  using MeasurementVectorType = TMeasurementVector;

  /** Length of each measurement vector */
  using MeasurementVectorSizeType = typename Superclass::MeasurementVectorSizeType;

  /** Type of the mean vector. RealType on a vector-type is the same
   * vector-type but with a real element type.  */
  using MeasurementVectorRealType = typename itk::NumericTraits<MeasurementVectorType>::RealType;
  using MeanVectorType = MeasurementVectorRealType;

  /** Type of the covariance matrix */
  using CovarianceMatrixType = VariableSizeMatrix<double>;

  /** Set the mean of the Gaussian distribution. Mean is a vector type
   * similar to the measurement type but with a real element type. */
  void
  SetMean(const MeanVectorType & mean);

  /** Get the mean of the Gaussian distribution. Mean is a vector type
   * similar to the measurement type but with a real element type. */
  itkGetConstReferenceMacro(Mean, MeanVectorType);

  /** Set the covariance matrix. Covariance matrix is a
   * VariableSizeMatrix of doubles. The inverse of the covariance
   * matrix and the normalization term for the multivariate Gaussian
   * are calculated whenever the covaraince matrix is changed. */
  void
  SetCovariance(const CovarianceMatrixType & cov);

  /* Get the covariance matrix. Covariance matrix is a
  VariableSizeMatrix of doubles. */
  itkGetConstReferenceMacro(Covariance, CovarianceMatrixType);

  /* Get the inverse covariance matrix. Covariance matrix is a
  VariableSizeMatrix of doubles. */
  itkGetConstReferenceMacro(InverseCovariance, CovarianceMatrixType);

  /** Evaluate the probability density of a measurement vector. */
  double
  Evaluate(const MeasurementVectorType & measurement) const override;

  /** Method to clone a membership function, i.e. create a new instance of
   * the same type of membership function and configure its ivars to
   * match. */
  typename LightObject::Pointer
  InternalClone() const override;

protected:
  GaussianMembershipFunction();
  ~GaussianMembershipFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  MeanVectorType       m_Mean;       // mean
  CovarianceMatrixType m_Covariance; // covariance matrix

  // inverse covariance matrix. automatically calculated
  // when covariance matirx is set.
  CovarianceMatrixType m_InverseCovariance;

  // pre_factor (normalization term). automatically calculated
  // when covariance matirx is set.
  double m_PreFactor;

  /** Boolean to cache whether the covariance is singular or nearly singular */
  bool m_CovarianceNonsingular;
};
} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianMembershipFunction.hxx"
#endif

#endif
