/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MinimumEuclideanDistanceClassifier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MinimumEuclideanDistanceClassifier_h
#define __MinimumEuclideanDistanceClassifier_h

#include "itkObject.h"
#include "itkArray.h"
#include "itkEuclideanDistance.h"
#include "itkMinimumDecisionRule.h"
#include "itkSampleClassifier.h"

template< class TSample >
class MinimumEuclideanDistanceClassifier
{
public:
  MinimumEuclideanDistanceClassifier() ;
  ~MinimumEuclideanDistanceClassifier() ;

  typedef itk::Statistics::SampleClassifier< TSample > ClassifierType ;
  typedef itk::Statistics::EuclideanDistance< typename TSample::MeasurementVectorType >
    DistanceFunctionType ;
  typedef std::vector< DistanceFunctionType::Pointer > DistanceFunctionVectorType ;
  typedef itk::MinimumDecisionRule DecisionRuleType ;
  typedef itk::Array< double > ParametersType ;
  typedef ClassifierType::OutputType::ClassLabelHolderType ClassLabelsType ;

  void SetSample(TSample* sample) ;
  
  void SetParameters(ParametersType& parameters) ;

  void SetComponentClassLabels(std::vector< unsigned int >& classLabels) ;

  ClassLabelsType* GetClassLabels() ;

  void GenerateData() ;

private:
  TSample* m_Sample ;
  unsigned int m_NumberOfClasses ;
  ParametersType m_Parameters ;
  std::vector< unsigned int > m_ComponentClassLabels ;
  typename DecisionRuleType::Pointer m_DecisionRule ;
  DistanceFunctionVectorType m_FunctionVector ;
  typename ClassifierType::Pointer m_InternalClassifier ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "MinimumEuclideanDistanceClassifier.txx"
#endif

#endif
