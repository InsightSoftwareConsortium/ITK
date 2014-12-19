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
#ifndef itkMahalanobisDistanceMembershipFunction_hxx
#define itkMahalanobisDistanceMembershipFunction_hxx

#include "itkMahalanobisDistanceMembershipFunction.h"

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_matrix_inverse.h"

namespace itk
{
namespace Statistics
{
template< typename TVector >
MahalanobisDistanceMembershipFunction< TVector >
::MahalanobisDistanceMembershipFunction()
{
  NumericTraits<MeanVectorType>::SetLength(m_Mean, this->GetMeasurementVectorSize());
  m_Mean.Fill(0.0f);

  m_Covariance.SetSize(this->GetMeasurementVectorSize(), this->GetMeasurementVectorSize());
  m_Covariance.SetIdentity();

  m_InverseCovariance = m_Covariance;

  m_CovarianceNonsingular = true;
}

template< typename TVector >
void
MahalanobisDistanceMembershipFunction< TVector >
::SetMean(const MeanVectorType & mean)
{
  if ( this->GetMeasurementVectorSize() )
    {
    MeasurementVectorTraits::Assert(mean,
                                    this->GetMeasurementVectorSize(),
                                    "GaussianMembershipFunction::SetMean(): Size of mean vector specified does not match the size of a measurement vector.");
    }
  else
    {
    // not already set, cache the size
    this->SetMeasurementVectorSize( mean.Size() );
    }

  if ( m_Mean != mean )
    {
    m_Mean = mean;
    this->Modified();
    }
}

template< typename TVector >
void
MahalanobisDistanceMembershipFunction< TVector >
::SetCovariance(const CovarianceMatrixType & cov)
{
  // Sanity check
  if ( cov.GetVnlMatrix().rows() != cov.GetVnlMatrix().cols() )
    {
    itkExceptionMacro(<< "Covariance matrix must be square");
    }
  if ( this->GetMeasurementVectorSize() )
    {
    if ( cov.GetVnlMatrix().rows() != this->GetMeasurementVectorSize() )
      {
      itkExceptionMacro(<< "Length of measurement vectors must be"
                        << " the same as the size of the covariance.");
      }
    }
  else
    {
    // not already set, cache the size
    this->SetMeasurementVectorSize( cov.GetVnlMatrix().rows() );
    }

  if (m_Covariance == cov)
    {
    // no need to copy the matrix, compute the inverse, or the normalization
    return;
    }

  m_Covariance = cov;

  // the inverse of the covariance matrix is first computed by SVD
  vnl_matrix_inverse< double > inv_cov( m_Covariance.GetVnlMatrix() );

  // the determinant is then costless this way
  double det = inv_cov.determinant_magnitude();

  if( det < 0.)
    {
    itkExceptionMacro( << "det( m_Covariance ) < 0" );
    }

  // 1e-6 is an arbitrary value!!!
  const double singularThreshold = 1.0e-6;
  m_CovarianceNonsingular = ( det > singularThreshold );

  if( m_CovarianceNonsingular )
    {
    // allocate the memory for m_InverseCovariance matrix
    m_InverseCovariance.GetVnlMatrix() = inv_cov.inverse();
    }
  else
    {
    // define the inverse to be diagonal with large values along the
    // diagonal. value chosen so (X-M)'inv(C)*(X-M) will usually stay
    // below NumericTraits<double>::max()
    const double aLargeDouble = std::pow(NumericTraits<double>::max(), 1.0/3.0)
      / (double) this->GetMeasurementVectorSize();
    m_InverseCovariance.SetSize(this->GetMeasurementVectorSize(), this->GetMeasurementVectorSize());
    m_InverseCovariance.SetIdentity();
    m_InverseCovariance *= aLargeDouble;
    }

  this->Modified();
}

template< typename TVector >
double
MahalanobisDistanceMembershipFunction< TVector >
::Evaluate(const MeasurementVectorType & measurement) const
{
  const MeasurementVectorSizeType measurementVectorSize =
    this->GetMeasurementVectorSize();

  // Our inverse covariance is always well formed. When the covariance
  // is singular, we use a diagonal inverse covariance with a large diagnonal

  // temp = ( y - mean )^t * InverseCovariance * ( y - mean )
  //
  // This is manually done to remove dynamic memory allocation:
  // double temp = dot_product( tempVector,  m_InverseCovariance.GetVnlMatrix() * tempVector );
  //
  double temp = 0.0;
  for (unsigned int r = 0; r < measurementVectorSize; ++r)
    {
    double rowdot = 0.0;
    for(unsigned int c = 0; c < measurementVectorSize; ++c)
      {
      rowdot += m_InverseCovariance(r, c) * ( measurement[c] - m_Mean[c] );
      }
    temp += rowdot * ( measurement[r] - m_Mean[r] );
    }

  return temp;
}

template< typename TVector >
void
MahalanobisDistanceMembershipFunction< TVector >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Covariance: " << std::endl;
  os << m_Covariance.GetVnlMatrix();
  os << indent << "InverseCovariance: " << std::endl;
  os << indent << m_InverseCovariance.GetVnlMatrix();
  os << indent << "Covariance nonsingular: " <<
    (m_CovarianceNonsingular ? "true" : "false") << std::endl;
}

template< typename TVector >
typename LightObject::Pointer
MahalanobisDistanceMembershipFunction< TVector >
::InternalClone() const
{
  LightObject::Pointer loPtr = Superclass::InternalClone();
  typename Self::Pointer membershipFunction =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(membershipFunction.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }

  membershipFunction->SetMeasurementVectorSize( this->GetMeasurementVectorSize() );
  membershipFunction->SetMean( this->GetMean() );
  membershipFunction->SetCovariance( this->GetCovariance() );

  return loPtr;
}

} // end namespace Statistics
} // end of namespace itk

#endif
