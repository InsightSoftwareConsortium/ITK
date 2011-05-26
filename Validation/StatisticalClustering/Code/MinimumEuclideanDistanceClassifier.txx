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
