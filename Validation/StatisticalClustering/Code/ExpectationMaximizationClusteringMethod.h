/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ExpectationMaximizationClusteringMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __ExpectationMaximizationClusteringMethod_h
#define __ExpectationMaximizationClusteringMethod_h

#include <time.h>

#include "itkMacro.h"
#include "itkArray.h"
#include "itkVector.h"

#include "itkExpectationMaximizationMixtureModelEstimator.h"
#include "itkGaussianMixtureModelComponent.h"

template< class TSample, class THistogram >
class ExpectationMaximizationClusteringMethod
{
public:
  ExpectationMaximizationClusteringMethod() ;
  ~ExpectationMaximizationClusteringMethod() {}

  typedef itk::Statistics::ExpectationMaximizationMixtureModelEstimator< THistogram >
    EstimatorType ;

  typedef itk::Statistics::GaussianMixtureModelComponent< THistogram > 
    ComponentType ;

  typedef itk::hash_map< typename TSample::InstanceIdentifier, 
                         unsigned int > ClusterLabelsType ;

  typedef itk::Array< double > ParametersType ;
  
  void SetInitialParameters(ParametersType& parameters)
  { m_InitialParameters = parameters ; }

  void SetMaximumIteration(unsigned int numberOfIterations)
  { m_MaximumIteration = numberOfIterations ; }

  void SetHistogram(THistogram* histogram)
  { m_Histogram = histogram ; }

  void SetSample(TSample* sample)
  { m_Sample = sample ; }

  void Initialize(unsigned int numberOfClusters) ;

  void Run() ;
  
  unsigned int GetLastIteration()
  { return m_LastIteration ; }

  ParametersType& GetEstimatedParameters()
  { return m_EstimatedParameters ; }

  ClusterLabelsType* GetClusterLabels() 
  { return &m_ClusterLabels ; }

  double GetTotalElapsedTime()
  { return double(m_ProcessEnd - m_ProcessBegin) / CLOCKS_PER_SEC ; }

  double GetEstimationElapsedTime()
  { return double(m_EstimationEnd - m_ProcessBegin) / CLOCKS_PER_SEC ; }

private:
  /** inputs */
  THistogram* m_Histogram ;
  TSample* m_Sample ;
  unsigned int m_NumberOfComponents ;
  ParametersType m_InitialParameters ;
  unsigned int m_MaximumIteration ;

  /** outputs */
  ParametersType m_EstimatedParameters ;
  unsigned int m_LastIteration ;
  time_t m_ProcessBegin ;
  time_t m_EstimationEnd ;
  time_t m_ProcessEnd ;
  itk::hash_map< typename TSample::InstanceIdentifier, unsigned int > m_ClusterLabels ;

  /** helper classes */
  EstimatorType::Pointer m_Estimator ;
  std::vector< typename ComponentType::Pointer > m_Components ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "ExpectationMaximizationClusteringMethod.txx"
#endif

#endif // __ExpectationMaximizationClusteringMethod_h
