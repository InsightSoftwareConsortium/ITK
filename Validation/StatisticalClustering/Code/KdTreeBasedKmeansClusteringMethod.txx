/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    KdTreeBasedKmeansClusteringMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
