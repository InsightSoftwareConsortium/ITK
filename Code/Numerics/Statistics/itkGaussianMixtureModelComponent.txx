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
  m_GaussianDensityFunction = NativeMembershipFunctionType::New() ;
  this->SetMembershipFunction((MembershipFunctionPointer)
                              m_GaussianDensityFunction) ;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent) ;
  std::cout << indent << "Initial Mean: " << m_InitialMean << std::endl ;
  std::cout << indent << "Initial Covariance: " << m_InitialCovariance 
            << std::endl ;
  std::cout << indent << "Mean Estimator: " << m_MeanEstimator << std::endl ;
  std::cout << indent << "Covariance Estimator: " << m_CovarianceEstimator
            << std::endl ;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::SetSample(SamplePointer sample)
{
  Superclass::SetSample(sample) ;
  m_MeanEstimator->SetSample(sample) ;
  m_CovarianceEstimator->SetSample(sample) ;

  WeightArrayType* weights = this->GetWeights() ;
  m_MeanEstimator->SetWeights(weights) ;
  m_CovarianceEstimator->SetWeights(weights) ;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::SetInitialMean(MeanType* mean)
{
  m_InitialMean = mean ;
  m_Mean = *mean ;
  m_GaussianDensityFunction->SetMean(&m_Mean) ;
  m_CovarianceEstimator->SetMean(&m_Mean) ;
  this->AreParametersModified(true) ;
}


template< class TSample >
GaussianMixtureModelComponent< TSample >::MeanType*
GaussianMixtureModelComponent< TSample >
::GetInitialMean()
{
  return m_InitialMean ;
}

template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::SetInitialCovariance(CovarianceType* covariance)
{
  m_InitialCovariance = covariance ;
  m_Covariance = *covariance ;
  m_GaussianDensityFunction->SetCovariance(&m_Covariance) ;
  this->AreParametersModified(true) ;
}

template< class TSample >
GaussianMixtureModelComponent< TSample >::CovarianceType*
GaussianMixtureModelComponent< TSample >
::GetInitialCovariance()
{
  return m_InitialCovariance ;
}


template< class TSample >
void
GaussianMixtureModelComponent< TSample >
::GenerateData()
{
  this->AreParametersModified(false) ;

  m_MeanEstimator->Update() ;
  
  if (*(m_MeanEstimator->GetOutput()) != m_Mean)
    {
      m_Mean = *(m_MeanEstimator->GetOutput()) ;
      this->AreParametersModified(true) ;
    }

  m_CovarianceEstimator->Update() ;
  if (m_CovarianceEstimator->GetOutput()->GetVnlMatrix() !=
      m_Covariance.GetVnlMatrix())
    {
      m_Covariance = *(m_CovarianceEstimator->GetOutput()) ;
      this->AreParametersModified(true) ;
    }
}
    
} // end of namespace Statistics 
} // end of namespace itk 

#endif



