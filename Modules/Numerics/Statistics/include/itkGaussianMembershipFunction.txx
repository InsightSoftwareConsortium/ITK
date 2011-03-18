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
#ifndef __itkGaussianMembershipFunction_txx
#define __itkGaussianMembershipFunction_txx

#include "itkGaussianMembershipFunction.h"

namespace itk
{
namespace Statistics
{
template< class TMeasurementVector >
GaussianMembershipFunction< TMeasurementVector >
::GaussianMembershipFunction()
{
  for( unsigned int i = 0; i < this->GetMeasurementVectorSize(); i++ )
    {
    this->m_Mean[i] = 0;
    }
  this->m_PreFactor = 0.0;
  this->m_Covariance.SetIdentity();
}

template< class TMeasurementVector >
void
GaussianMembershipFunction< TMeasurementVector >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Covariance: " << std::endl;
  os << m_Covariance.GetVnlMatrix();
  os << indent << "InverseCovariance: " << std::endl;
  os << indent << m_InverseCovariance.GetVnlMatrix();
  os << indent << "Prefactor: " << m_PreFactor << std::endl;
}

template< class TMeasurementVector >
void
GaussianMembershipFunction< TMeasurementVector >
::SetMean(const MeanType & mean)
{
  if ( this->GetMeasurementVectorSize() )
    {
    MeasurementVectorTraits::Assert(mean,
                                    this->GetMeasurementVectorSize(),
                                    "GaussianMembershipFunction::SetMean Size of measurement vectors in \
    the sample must the same as the size of the mean." );
    }
  else
    {
    this->SetMeasurementVectorSize( mean.Size() );
    }

  if ( m_Mean != mean )
    {
    m_Mean = mean;
    this->Modified();
    }
}

template< class TMeasurementVector >
void
GaussianMembershipFunction< TMeasurementVector >
::SetCovariance(const CovarianceType & cov)
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
      itkExceptionMacro(<< "Length of measurement vectors in the sample must be"
                        << " the same as the size of the covariance.");
      }
    }
  else
    {
    this->SetMeasurementVectorSize( cov.GetVnlMatrix().rows() );
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
  m_DeterminantOK = ( det > 1e-6 );

  if( m_DeterminantOK )
    {
    // allocate the memory for m_InverseCovariance matrix
    m_InverseCovariance.GetVnlMatrix() = inv_cov.inverse();

    // calculate coefficient C of multivariate gaussian
    m_PreFactor =
      1.0 / ( vcl_sqrt(det) *
        vcl_pow( vcl_sqrt(2.0 * vnl_math::pi),
               static_cast< double >( this->GetMeasurementVectorSize() ) ) );
    }
}

template< class TMeasurementVector >
inline double
GaussianMembershipFunction< TMeasurementVector >
::Evaluate(const MeasurementVectorType & measurement) const
{
  const MeasurementVectorSizeType measurementVectorSize =
    this->GetMeasurementVectorSize();

  //if ( !m_IsCovarianceZero )
  if( m_DeterminantOK )
    {
    // Compute ( y - mean )
    vnl_vector< double > tempVector( measurementVectorSize );

    for ( MeasurementVectorSizeType i = 0; i < measurementVectorSize; ++i )
      {
      tempVector[i] = measurement[i] - m_Mean[i];
      }

    // temp = ( y - mean )^t * InverseCovariance * ( y - mean )
    double temp = dot_product( tempVector,
                               m_InverseCovariance.GetVnlMatrix() * tempVector );

    temp = vcl_exp(-0.5 * temp);

    return m_PreFactor * temp;
    }
  else
    {
    for ( MeasurementVectorSizeType i = 0; i < measurementVectorSize; ++i )
      {
      if ( m_Mean[i] != static_cast< double >( measurement[i] ) )
        {
        return 0.;
        }
      }
    return NumericTraits< double >::max();
    }
}

template< class TVector >
typename GaussianMembershipFunction< TVector >::Pointer
GaussianMembershipFunction< TVector >
::Clone()
{
  Pointer membershipFunction = GaussianMembershipFunction< TVector >::New();

  membershipFunction->SetMeasurementVectorSize( this->GetMeasurementVectorSize() );
  membershipFunction->SetMean( this->GetMean() );
  membershipFunction->SetCovariance( this->GetCovariance() );

  return membershipFunction;
}
} // end namespace Statistics
} // end of namespace itk

#endif
