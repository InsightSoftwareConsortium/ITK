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
#ifndef __KdTreeBasedKmeansClusteringMethod_txx
#define __KdTreeBasedKmeansClusteringMethod_txx

#include "KdTreeBasedKmeansClusteringMethod.h"

template< class TKdTree >
KdTreeBasedKmeansClusteringMethod< TKdTree >
::KdTreeBasedKmeansClusteringMethod()
{
  m_KmeansEstimator =
    KmeansEstimatorType::New() ;
}


template< class TKdTree >
void
KdTreeBasedKmeansClusteringMethod< TKdTree >
::Run()
{
  //  classifier.SetComponentClassLabels(classLabels) ;

  m_KmeansEstimator->SetKdTree(m_KdTree) ;
  m_KmeansEstimator->SetMaximumIteration(m_MaximumIteration) ;
  //  m_KmeansEstimator->SetCentroidPositionChangesThreshold(0.0) ;
  m_KmeansEstimator->SetParameters(m_InitialParameters) ;

  m_ProcessBegin = clock() ;
  m_KmeansEstimator->StartOptimization() ;
  m_EstimationEnd = clock() ;

  m_EstimatedParameters = m_KmeansEstimator->GetParameters() ;
  m_LastIteration = m_KmeansEstimator->GetCurrentIteration() ;

  m_Classifier.SetSample(m_KdTree->GetSample()) ;
  m_Classifier.SetParameters(m_EstimatedParameters) ;
  m_Classifier.GenerateData() ;
  m_ProcessEnd = clock() ;
}

#endif
