/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkMahalanobisDistanceMembershipFunction_h
#define itkMahalanobisDistanceMembershipFunction_h

#include "itkVariableSizeMatrix.h"

#include "itkMembershipFunctionBase.h"

namespace itk
{
namespace Statistics
{
/** \class MahalanobisDistanceMembershipFunction
 * \brief MahalanobisDistanceMembershipFunction models class
 * membership using Mahalanobis distance.
 *
 * MahalanobisDistanceMembershipFunction is a subclass of
 * MembershipFunctionBase that models class membership (or likelihood)
 * using the Mahalanobis distance. The mean and covariance structure
 * of the Mahalanobis distance are established using the methods
 * SetMean() and SetCovariance(). The mean is a vector-type that is the same
 * vector-type as the measurement vector but guaranteed to have a real
 * element type. For instance, if the measurement type is an
 * Vector<int,3>, then the mean is Vector<double,3>. If the
 * measurement type is a VariableLengthVector<float>, then the mean is
 * VariableLengthVector<double>. In contrast to this behavior, the
 * covariance is always a VariableSizeMatrix<double>.
 *
 * Note that this membership function does not return a probability
 * density function in contrast to the GaussianMembershipFunction.
 *
 * Note, as is the case in other packages (MATLAB, R), the value
 * returned by this membership function is the squared distance.
 *
 * If the covariance is singular or nearly singular, the membership function
 * behaves somewhat like (the opposite of) an impulse located at the
 * mean. In this case, we specify the covariance to be a diagonal
 * matrix with large values along the diagonal. This membership
 * function, therefore, will return large but differentiable values
 * everywhere and decay to zero sharply near the mean.
 *
 * \ingroup ITKStatistics
 */

template< typename TVector >
class ITK_TEMPLATE_EXPORT MahalanobisDistanceMembershipFunction:
  public MembershipFunctionBase< TVector >
{
public:
  /** Standard class typedefs */
  typedef MahalanobisDistanceMembershipFunction Self;
  typedef MembershipFunctionBase< TVector >     Superclass;
  typedef SmartPointer< Self >                  Pointer;
  typedef SmartPointer< const Self >            ConstPointer;

  /** Strandard macros */
  itkTypeMacro(MahalanobisDistanceMembershipFunction, MembershipFunctionBase);
  itkNewMacro(Self);

  /** SmartPointer class for superclass */
  typedef typename Superclass::Pointer MembershipFunctionPointer;

  /** Typedef alias for the measurement vectors */
  typedef TVector MeasurementVectorType;

  /** Typedef to represent the length of measurement vectors */
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Type of the mean vector. RealType on a vector-type is the same
   * vector-type but with a real element type.  */
  typedef typename itk::NumericTraits< MeasurementVectorType >::RealType MeasurementVectorRealType;
  typedef MeasurementVectorRealType  MeanVectorType;

  /** Type of the covariance matrix */
  typedef VariableSizeMatrix< double > CovarianceMatrixType;

  /** Set the mean used in the Mahalanobis distance. Mean is a vector type
   * similar to the measurement type but with a real element type.  */
  void SetMean(const MeanVectorType & mean);

  /** Get the mean of the Mahalanobis distance. Mean is a vector type
   * similar to the measurement type but with a real element type. */
  itkGetConstReferenceMacro(Mean, MeanVectorType);

  /** Set the covariance matrix. Covariance matrix is a
   * VariableSizeMatrix of doubles. The inverse of the covariance
   * matrix is calculated whenever the covaraince matrix is changed. */
  void SetCovariance(const CovarianceMatrixType & cov);

  /** Get the covariance matrix. Covariance matrix is a
   * VariableSizeMatrix of doubles. */
  itkGetConstReferenceMacro(Covariance, CovarianceMatrixType);

  /**
   * Evaluate the Mahalanobis distance of a measurement using the
   * prescribed mean and covariance. Note that the Mahalanobis
   * distance is not a probability density. The square of the
   * distance is returned. */
  double Evaluate(const MeasurementVectorType & measurement) const ITK_OVERRIDE;

  /** Method to clone a membership function, i.e. create a new instance of
   * the same type of membership function and configure its ivars to
   * match. */
  virtual typename LightObject::Pointer InternalClone() const ITK_OVERRIDE;

protected:
  MahalanobisDistanceMembershipFunction();
  virtual ~MahalanobisDistanceMembershipFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  MeanVectorType       m_Mean;               // mean
  CovarianceMatrixType m_Covariance;         // covariance matrix

  // inverse covariance matrix. automatically calculated
  // when covariace matirx is set.
  CovarianceMatrixType m_InverseCovariance;

  /** Boolean to cache whether the covarinace is singular or nearly singular */
  bool m_CovarianceNonsingular;
};
} // end of namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMahalanobisDistanceMembershipFunction.hxx"
#endif

#endif
