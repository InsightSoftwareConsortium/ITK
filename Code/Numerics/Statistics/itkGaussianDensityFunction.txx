/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDensityFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianDensityFunction_txx
#define __itkGaussianDensityFunction_txx

#include "itkGaussianDensityFunction.h"

namespace itk{ 
namespace Statistics{

template < class TMeasurementVector >
GaussianDensityFunction< TMeasurementVector >
::GaussianDensityFunction()
{
}

template < class TMeasurementVector >
void  
GaussianDensityFunction< TMeasurementVector >
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i ;
  Superclass::PrintSelf(os,indent);

  os << indent << "Mean: " << (*m_Mean) << std::endl ;
  os << indent << "Covariance: " << std::endl ;
  os << indent << m_Covariance->GetVnlMatrix() ;
  os << indent << "InverseCovariance: " << std::endl ;
  os << indent << m_InverseCovariance.GetVnlMatrix() ;
  os << indent << "Prefactor: " << m_PreFactor << std::endl;
}

template < class TMeasurementVector >
void 
GaussianDensityFunction< TMeasurementVector >
::SetCovariance(CovarianceType* cov)
{
  m_Covariance = cov; 

  // allocate the memory for m_InverseCovariance matrix   
  m_InverseCovariance.GetVnlMatrix() = 
    vnl_matrix_inverse< double >(m_Covariance->GetVnlMatrix());
  
  // the determinant of the covaraince matrix
  double det = vnl_determinant(m_Covariance->GetVnlMatrix());
 
  // calculate coefficient C of multivariate gaussian
  m_PreFactor = 1.0 / (sqrt(det) * 
                       pow(sqrt(2.0 * vnl_math::pi), double(VectorDimension))) ;


}

template < class TMeasurementVector >
GaussianDensityFunction< TMeasurementVector >::CovarianceType*
GaussianDensityFunction< TMeasurementVector >
::GetCovariance() const
{
  return m_Covariance ;
}

template < class TMeasurementVector >
inline double
GaussianDensityFunction< TMeasurementVector >
::Evaluate(const MeasurementVectorType &measurement) const
{ 

  double temp ;

  MeanType tempVector ;
  MeanType tempVector2 ;

  // Compute |y - mean | 
  for ( unsigned int i = 0 ; i < VectorDimension ; i++)
    {
    tempVector[i] = measurement[i] - (*m_Mean)[i] ;
    }


  // Compute |y - mean | * inverse(cov) 
  for (unsigned int i = 0 ; i < VectorDimension ; i++)
    {
    temp = 0 ;
    for (unsigned int j = 0 ; j < VectorDimension ; j++)
      {
      temp += tempVector[j] * m_InverseCovariance.GetVnlMatrix().get(j, i) ;
      }
    tempVector2[i] = temp ;
    }


  // Compute |y - mean | * inverse(cov) * |y - mean|^T 
  temp = 0 ;
  for (unsigned int i = 0 ; i < VectorDimension ; i++)
    {
    temp += tempVector2[i] * tempVector[i] ;
    }

  return  m_PreFactor * exp( -0.5 * temp ) ;
}
  

} // end namespace Statistics
} // end of namespace itk



#endif
