/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MinimumEuclideanDistanceClassifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MinimumEuclideanDistanceClassifier_txx
#define __MinimumEuclideanDistanceClassifier_txx

#include "MinimumEuclideanDistanceClassifier.h"

template< class TSample >
MinimumEuclideanDistanceClassifier< TSample >
::MinimumEuclideanDistanceClassifier()
{
  m_Sample = 0 ;
  m_NumberOfClasses = 0 ;
  m_InternalClassifier = ClassifierType::New() ;
  m_DecisionRule = DecisionRuleType::New() ;
  m_InternalClassifier->SetDecisionRule((itk::DecisionRuleBase::Pointer) m_DecisionRule) ;
}

template< class TSample >
MinimumEuclideanDistanceClassifier< TSample >
::~MinimumEuclideanDistanceClassifier()
{
}

template< class TSample >
void
MinimumEuclideanDistanceClassifier< TSample >
::SetSample(TSample* sample)
{
  if ( m_Sample != sample )
    {
      m_Sample = sample ;
      m_InternalClassifier->SetSample(sample) ;
    }
}

template< class TSample >
void
MinimumEuclideanDistanceClassifier< TSample >
::SetParameters(ParametersType& parameters)
{
  if ( m_Parameters != parameters )
    {
      m_Parameters = parameters ;
      m_NumberOfClasses = m_Parameters.Size() / TSample::MeasurementVectorSize ;
      m_InternalClassifier->SetNumberOfClasses(m_NumberOfClasses) ;
      for (int i = 0 ; i < m_NumberOfClasses ; i++)
        {
          m_FunctionVector.push_back(DistanceFunctionType::New()) ;
          m_InternalClassifier->AddMembershipFunction(m_FunctionVector[i]) ;
        }
    }
}

template< class TSample >
void
MinimumEuclideanDistanceClassifier< TSample >
::SetComponentClassLabels(std::vector< unsigned int >& classLabels)
{
  m_ComponentClassLabels = classLabels ;
  m_InternalClassifier->SetMembershipFunctionClassLabels(m_ComponentClassLabels) ;
}

template< class TSample >
MinimumEuclideanDistanceClassifier< TSample >::ClassLabelsType*
MinimumEuclideanDistanceClassifier< TSample >
::GetClassLabels()
{
  return m_InternalClassifier->GetOutput()->GetClassLabels() ;
}

template< class TSample >
void
MinimumEuclideanDistanceClassifier< TSample >
::GenerateData()
{
  DistanceFunctionType::OriginType mean ;
  int paramIndex = 0 ;
  for (unsigned int i = 0 ; i < m_NumberOfClasses ; i++)
    {
      for (unsigned int j = 0 ; j < TSample::MeasurementVectorSize ; j++)
        {
          mean[j] = m_Parameters[paramIndex] ;
          ++paramIndex ;
        }
      (m_FunctionVector[i])->SetOrigin(mean) ;
    }

  m_InternalClassifier->Update() ;
}

#endif
