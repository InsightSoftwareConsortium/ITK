/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ExpectationMaximizationValidationApp.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __ExpectationMaximizationValidationApp_h
#define __ExpectationMaximizationValidationApp_h

#include "StatisticalClusteringValidationAppBase.h"
#include "ExpectationMaximizationClusteringMethod.h"

template< class TPixel, unsigned int VMeasurementVectorSize >
class ExpectationMaximizationValidationApp : 
  public StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize >
{
public:
  ExpectationMaximizationValidationApp() ;
  ~ExpectationMaximizationValidationApp() ;

  typedef StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize > Superclass ;
  typedef typename Superclass::ParametersType ParametersType ;
  typedef typename Superclass::SampleGeneratorType SampleGeneratorType ;
  
  typedef ExpectationMaximizationClusteringMethod< typename SampleGeneratorType::SubsampleType,
                                                   typename SampleGeneratorType::HistogramType >
  ClusteringMethodType ;

protected:
  void GenerateSample() ;
  void PrepareInputParameterFilter() ;
  void PrepareOutputParameterTableHeader() ;
  void StartClustering(ParametersType& params) ;
  void MapClusterToClass() ;
  void PutResult(unsigned int caseNo) ;

  ClusteringMethodType m_ClusteringMethod ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "ExpectationMaximizationValidationApp.txx"
#endif

#endif
