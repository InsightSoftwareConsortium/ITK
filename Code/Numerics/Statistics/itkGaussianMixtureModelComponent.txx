/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGaussianMixtureModelComponent.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianMixtureModelComponent_txx
#define __itkGaussianMixtureModelComponent_txx

#include <iostream>

#include "itkGaussianMixtureModelComponent.h"

namespace itk{ 
namespace Statistics{
  
template< class TSample >
GaussianMixtureModelComponent< TSample >
::GaussianMixtureModelComponent()
{
  m_MeanEstimator = MeanEstimatorType::New() ;
  m_CovarianceEstimator = CovarianceEstimatorType::New() ;
  m_CovarianceEstimator->SetMean(&m_Mean) ;
  m_GaussianDensityFunction = NativeMembershipFunctionType::New() ;
  m_GaussianDensityFunction->SetMean(&m_Mean) ;
  this->SetMembershipFunction((MembershipFunctionType*)
                              m_GaussianDensityFunction.GetPointer()) ;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent) ;
  std::cout << indent << "Mean: " << m_Mean << std::endl ;
  std::cout << indent << "Covariance: " << m_Covariance 
            << std::endl ;
  std::cout << indent << "Mean Estimator: " << m_MeanEstimator << std::endl ;
  std::cout << indent << "Covariance Estimator: " << m_CovarianceEstimator
            << std::endl ;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::SetSample(TSample* sample)
{
  Superclass::SetSample(sample) ;

  m_MeanEstimator->SetInputSample(sample) ;
  m_CovarianceEstimator->SetInputSample(sample) ;

  WeightArrayType* weights = this->GetWeights() ;
  m_MeanEstimator->SetWeights(weights) ;
  m_CovarianceEstimator->SetWeights(weights) ;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::SetParameters(const ParametersType &parameters)
{
  Superclass::SetParameters(parameters) ;

  unsigned int paramIndex = 0 ;
  unsigned int i, j ;

  bool changed = false ;

  for ( i = 0 ; i < MeasurementVectorSize ; i++)
    {
      if ( m_Mean[i] != parameters[paramIndex] )
        {
          m_Mean[i] = parameters[paramIndex] ;
          changed = true ;
        }
      ++paramIndex ;
    }

  for ( i = 0 ; i < MeasurementVectorSize ; i++ )
    {
      for ( j = 0 ; j < MeasurementVectorSize ; j++ )
        {
          if ( m_Covariance.GetVnlMatrix().get(i, j) != 
               parameters[paramIndex] )
            {
              m_Covariance.GetVnlMatrix().put(i, j, parameters[paramIndex]) ;
              changed = true ;
            }
          ++paramIndex ;
        }
    }
  m_GaussianDensityFunction->SetCovariance(&m_Covariance) ;
  this->AreParametersModified(changed) ;
}


template< class TSample >
double
GaussianMixtureModelComponent< TSample >
::CalculateParametersChange()
{
  unsigned int i, j ;

  MeanType meanEstimate = *(m_MeanEstimator->GetOutput()) ;
  CovarianceType covEstimate = *(m_CovarianceEstimator->GetOutput()) ;

  double temp ;
  double changes = 0.0 ;

  for ( i = 0 ; i < MeasurementVectorSize ; i++)
    {
      temp = m_Mean[i] - meanEstimate[i] ;
      changes += temp * temp ;
    }

  for ( i = 0 ; i < MeasurementVectorSize ; i++ )
    {
      for ( j = 0 ; j < MeasurementVectorSize ; j++ )
        {
          temp = m_Covariance.GetVnlMatrix().get(i, j) - 
            covEstimate.GetVnlMatrix().get(i, j) ;
          changes += temp * temp ;
        }
    }

  changes = sqrt(changes) ;
  return changes ;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::GenerateData()
{
  this->AreParametersModified(false) ;

  m_MeanEstimator->Update() ;

  unsigned int i, j ;
  double temp ;
  double changes = 0.0 ;
  bool changed = false ;
  ParametersType parameters = this->GetFullParameters() ;
  int paramIndex  = 0 ;

  MeanType meanEstimate = *(m_MeanEstimator->GetOutput()) ;
  for ( i = 0 ; i < MeasurementVectorSize ; i++)
    {
      temp = m_Mean[i] - meanEstimate[i] ;
      changes = temp * temp ;
      changes = sqrt(changes) ;
      if ( changes > this->GetMinimalParametersChange() )
        {
          changed = true ;
        }
    }


  if ( changed )
    {
      m_Mean = *(m_MeanEstimator->GetOutput()) ;
      for ( i = 0 ; i < MeasurementVectorSize ; i++)
        {
          parameters[paramIndex] = meanEstimate[i];
          ++paramIndex ;
        }
      this->AreParametersModified(true) ;
    }

  m_CovarianceEstimator->Update() ;
  CovarianceType covEstimate = *(m_CovarianceEstimator->GetOutput()) ;
  changed = false ;
  for ( i = 0 ; i < MeasurementVectorSize ; i++ )
    {
      for ( j = 0 ; j < MeasurementVectorSize ; j++ )
        {
          temp = m_Covariance.GetVnlMatrix().get(i, j) - 
            covEstimate.GetVnlMatrix().get(i, j) ;
          changes = temp * temp ;
          changes = sqrt(changes) ;
          if ( changes > this->GetMinimalParametersChange() )
            {
              changed = true ;
            }
        }
    }
  
  if ( changed )
    {
      m_Covariance = *(m_CovarianceEstimator->GetOutput()) ;
      for ( i = 0 ; i < MeasurementVectorSize ; i++ )
        {
          for ( j = 0 ; j < MeasurementVectorSize ; j++ )
            {
              parameters[paramIndex] = covEstimate.GetVnlMatrix().get(i, j) ;
              ++paramIndex ;
            }
        }
      this->AreParametersModified(true) ;
    }

  Superclass::SetParameters(parameters) ;
}
    
} // end of namespace Statistics 
} // end of namespace itk 

#endif



