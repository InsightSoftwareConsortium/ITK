/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMahalanobisDistanceMembershipFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMahalanobisDistanceMembershipFunction_txx
#define __itkMahalanobisDistanceMembershipFunction_txx

#include "itkMahalanobisDistanceMembershipFunction.h"

namespace itk{ 
namespace Statistics{

template < class TVector >
MahalanobisDistanceMembershipFunction< TVector >
::MahalanobisDistanceMembershipFunction():
  m_NumberOfSamples(0),
  m_PreFactor(0),
  m_Epsilon( 1e-100 ),
  m_DoubleMax( 1e+20 ),
  m_ValidInverseCovarianceFlag( false )
{
  m_Mean.fill( 0.0f );
  m_Covariance.set_identity();
  m_InverseCovariance.set_identity();
}

template < class TVector >
void 
MahalanobisDistanceMembershipFunction< TVector >
::SetMean(const MeanVectorType & mean)
{
  m_Mean = mean ;
}

template < class TVector >
const typename
MahalanobisDistanceMembershipFunction< TVector >::MeanVectorType &
MahalanobisDistanceMembershipFunction< TVector >
::GetMean() const
{
  return m_Mean ;
}

template < class TVector >
void 
MahalanobisDistanceMembershipFunction< TVector >
::SetCovariance(const CovarianceMatrixType &cov)
{
  m_Covariance = cov; 
  if( !m_ValidInverseCovarianceFlag ) CalculateInverseCovariance();
}

template < class TVector >
const typename
MahalanobisDistanceMembershipFunction< TVector >::CovarianceMatrixType &
MahalanobisDistanceMembershipFunction< TVector >
::GetCovariance() const
{
  return m_Covariance ;
}

template < class TVector >
void 
MahalanobisDistanceMembershipFunction< TVector >
::SetInverseCovariance(const CovarianceMatrixType &invcov)
{
  m_InverseCovariance = invcov; 
  m_ValidInverseCovarianceFlag = true;
}

template < class TVector >
const typename
MahalanobisDistanceMembershipFunction< TVector >::CovarianceMatrixType &
MahalanobisDistanceMembershipFunction< TVector >
::GetInverseCovariance() const
{
  return m_InverserCovariance ;
}

template < class TVector >
void
MahalanobisDistanceMembershipFunction< TVector >
::CalculateInverseCovariance() 
{

  // pack the cov matrix from in_model to tmp_cov_mat 
  double cov_sum = 0;
  for(unsigned int band_x = 0; band_x < VectorDimension; band_x++)
    { 
    for(unsigned int band_y = 0; band_y < VectorDimension; band_y++)
      {
      cov_sum += vnl_math_abs( m_Covariance[band_x][band_y] );
      }
    } 
  // check if it is a zero covariance, if it is, we make its
  // inverse as an identity matrix with diagonal elements as
  // a very large number; otherwise, inverse it 
  if( cov_sum < m_Epsilon ) 
    {
    m_InverseCovariance.resize( VectorDimension, VectorDimension );
    m_InverseCovariance.set_identity();
    m_InverseCovariance *= m_DoubleMax;
    }
  else 
    {
    // check if num_bands == 1, if it is, we just use 1 to divide it
    if( VectorDimension < 2 ) 
      {
      m_InverseCovariance.resize(1,1);
      m_InverseCovariance[0][0] = 1.0 / m_Covariance[0][0];
      }
    else 
      {
      m_InverseCovariance = vnl_matrix_inverse<double>(m_Covariance);
      }
    }// end inverse calculations

  m_ValidInverseCovarianceFlag = true;

}// CalculateInverseCovariance()

template < class TVector >
double 
MahalanobisDistanceMembershipFunction< TVector >
::Evaluate(const MeasurementVectorType &measurement) const
{ 

  double temp;

  // Compute |y - mean |   
  for ( unsigned int i = 0; i < VectorDimension; i++ )
    {
    m_TempVec[0][i] = measurement[i] - m_Mean[i];
    }

  // Compute |y - mean | * inverse(cov) 
  m_TempMat= m_TempVec * m_InverseCovariance;

  // Compute |y - mean | * inverse(cov) * |y - mean|^T 
  //tmp = (m_TmpMat * (m_TmpVec.transpose()))[0][0];
  temp = dot_product( vnl_vector_ref<double>( m_TempMat.size(), m_TempMat.begin()),
                      vnl_vector_ref<double>( m_TempVec.size(), m_TempVec.begin()));
  // should be this, but to remain comatible with the old vnl for
  // some time, the above is used
//  temp = dot_product( m_TempMat.as_ref(), m_TempVec.as_ref() ); 
  
  return temp ;
}
  
template < class TVector >
void  
MahalanobisDistanceMembershipFunction< TVector >
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i ;
  Superclass::PrintSelf(os,indent);

  os << indent << "Mean: [" ;
  for (i=0; (i + 1) < VectorDimension; i++)
    {
    os << m_Mean[i] << ", ";
    }
  os << m_Mean[i] << "]" << std::endl;

  os << indent << "Number of Samples: " << m_NumberOfSamples << std::endl;
  os << indent << "Covariance:        " << std::endl;
  os << m_Covariance << std::endl;
  os << indent << "Inverse covariance:        " << std::endl;
  os << m_InverseCovariance << std::endl;
  os << indent << "VectorSize:        " << VectorDimension << std::endl;
}
} // end namespace Statistics
} // end of namespace itk



#endif
