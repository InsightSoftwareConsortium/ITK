/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    KdTreeBasedKmeansClusteringMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __KdTreeBasedKmeansClusteringMethod_h
#define __KdTreeBasedKmeansClusteringMethod_h

#include <time.h>

#include "itkMacro.h"
#include "itkArray.h"
#include "itkVector.h"

#include "itkKdTreeBasedKmeansEstimator.h"
#include "MinimumEuclideanDistanceClassifier.h"

template< class TKdTree >
class KdTreeBasedKmeansClusteringMethod
{
public:
  KdTreeBasedKmeansClusteringMethod() ;
  ~KdTreeBasedKmeansClusteringMethod() {}

  typedef itk::Statistics::KdTreeBasedKmeansEstimator< TKdTree > 
  KmeansEstimatorType ;

  typedef MinimumEuclideanDistanceClassifier< typename TKdTree::SampleType > 
  ClassifierType ;

  typedef itk::hash_map< typename TKdTree::InstanceIdentifier, 
                         unsigned int > ClusterLabelsType ;

  typedef itk::Array< double > ParametersType ;
  
  void SetInitialParameters(ParametersType& parameters)
  { m_InitialParameters = parameters ; }

  void SetMaximumIteration(unsigned int numberOfIterations)
  { m_MaximumIteration = numberOfIterations ; }

  void SetKdTree(TKdTree* tree)
  { m_KdTree = tree ; }

  void Run() ;
  
  unsigned int GetLastIteration()
  { return m_LastIteration ; }

  ParametersType& GetEstimatedParameters()
  { return m_EstimatedParameters ; }

  ClusterLabelsType* GetClusterLabels() 
  { return m_Classifier.GetClassLabels() ; }

  double GetTotalElapsedTime()
  { return double(m_ProcessEnd - m_ProcessBegin) / CLOCKS_PER_SEC ; }

  double GetEstimationElapsedTime()
  { return double(m_EstimationEnd - m_ProcessBegin) / CLOCKS_PER_SEC ; }

private:
  /** inputs */
  TKdTree* m_KdTree ;
  unsigned int m_NumberOfClusters ;
  ParametersType m_InitialParameters ;
  unsigned int m_MaximumIteration ;

  /** outputs */
  ParametersType m_EstimatedParameters ;
  unsigned int m_LastIteration ;
  time_t m_ProcessBegin ;
  time_t m_EstimationEnd ;
  time_t m_ProcessEnd ;

  /** helper classes */
  KmeansEstimatorType::Pointer m_KmeansEstimator ;
  ClassifierType m_Classifier ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "KdTreeBasedKmeansClusteringMethod.txx"
#endif

#endif // __KdTreeBasedKmeansClusteringMethod_h
